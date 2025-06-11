//
//  VRSeatManagerPanel.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/16.
//

import UIKit
import RTCCommon
import Combine
import LiveStreamCore
import RTCRoomEngine
import TUILiveComponent

class VRSeatManagerPanel: RTCBaseView {
    private let manager: VoiceRoomManager
    private let routerManager: VRRouterManager
    private weak var coreView: SeatGridView?
    private var cancellableSet: Set<AnyCancellable> = []
    private var onTheSeatList: [TUISeatInfo] = []
    private var applySeatList: [VRSeatApplication] = []
    private lazy var seatListPublisher = manager.subscribeCoreState(StateSelector(keyPath: \SGSeatState.seatList))
    private lazy var seatApplicationPublisher = manager.subscribeState(StateSelector(keyPath: \VRSeatState.seatApplicationList))
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = UIFont.customFont(ofSize: 20)
        label.text = .seatControlTitleText
        return label
    }()
    
    private let changeSeatModeLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .needRequestText
        label.adjustsFontSizeToFitWidth = true
        return label
    }()
    
    private let seatModeSwitch: UISwitch = {
        let view = UISwitch()
        view.onTintColor = .b1
        return view
    }()
    
    private let separatorLine: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.7)
        return view
    }()
    
    private let tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.backgroundColor = .clear
        tableView.register(VRTheSeatCell.self, forCellReuseIdentifier: VRTheSeatCell.identifier)
        tableView.register(VRApplyTakeSeatCell.self, forCellReuseIdentifier: VRApplyTakeSeatCell.identifier)
        tableView.contentInset = UIEdgeInsets(top: 10, left: 0, bottom: 0, right: 0)
        return tableView
    }()
    
    private lazy var onTheSeatHeaderLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = .customFont(ofSize: 16, weight: .medium)
        return label
    }()
    
    private lazy var applySeatHeaderLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = .customFont(ofSize: 16, weight: .medium)
        return label
    }()
    
    private let inviteContentView: UIView = {
        let view = UIView(frame: .zero)
        return view
    }()
    
    private let inviteTipsLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.text = .inviteAudienceText
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .tipsGrayColor
        label.numberOfLines = 1
        return label
    }()
    
    private let inviteButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setTitle(.inviteText, for: .normal)
        button.backgroundColor = .b1
        button.titleLabel?.font = .customFont(ofSize: 20, weight: .semibold)
        button.layer.cornerRadius = 20.scale375Height()
        return button
    }()
    
    private let inviteImageButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setImage(internalImage("live_anchor_invite_icon"), for: .normal)
        return button
    }()
    
    init(manager: VoiceRoomManager, routerManager: VRRouterManager, coreView: SeatGridView) {
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        super.init(frame: .zero)
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
    
    override func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(inviteImageButton)
        addSubview(changeSeatModeLabel)
        addSubview(seatModeSwitch)
        addSubview(separatorLine)
        addSubview(tableView)
        
        addSubview(inviteContentView)
        inviteContentView.addSubview(inviteTipsLabel)
        inviteContentView.addSubview(inviteButton)
    }
    
    override func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
        }
        
        inviteImageButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-26.scale375())
            make.centerY.equalTo(titleLabel.snp.centerY)
        }
        
        changeSeatModeLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(24.scale375())
            make.top.equalTo(titleLabel.snp.bottom).offset(24.scale375Height())
            make.width.lessThanOrEqualTo(200.scale375())
        }
        
        seatModeSwitch.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24.scale375())
            make.centerY.equalTo(changeSeatModeLabel.snp.centerY)
        }
        
        separatorLine.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(24.scale375())
            make.trailing.equalToSuperview().offset(-24.scale375())
            make.height.equalTo(1)
            make.top.equalTo(changeSeatModeLabel.snp.bottom).offset(16.scale375Height())
        }
        
        tableView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(575.scale375Height())
            make.top.equalTo(separatorLine.snp.bottom)
        }
        
        inviteContentView.snp.makeConstraints { make in
            make.center.equalTo(tableView.snp.center)
        }
        
        inviteTipsLabel.snp.makeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
        }
        
        inviteButton.snp.makeConstraints { make in
            make.top.equalTo(inviteTipsLabel.snp.bottom).offset(23.scale375Height())
            make.centerX.equalToSuperview()
            make.size.equalTo(CGSize(width: 200.scale375(), height: 40.scale375Height()))
            make.bottom.equalToSuperview()
        }
    }
    
    override func bindInteraction() {
        tableView.delegate = self
        tableView.dataSource = self
        seatModeSwitch.addTarget(self, action: #selector(seatModeSwitchClick(sender:)), for: .valueChanged)
        inviteButton.addTarget(self, action: #selector(inviteButtonClick(sender:)), for: .touchUpInside)
        inviteImageButton.addTarget(self, action: #selector(inviteButtonClick(sender:)), for: .touchUpInside)
        subscribeOnSeatListState()
        subscribeApplyTakeSeatState()
        subscribeSeatModeState()
        subscribeInviteState()
    }
    
    private func subscribeOnSeatListState() {
        seatListPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] seatList in
                guard let self = self else { return }
                self.onTheSeatList = seatList.filter{ [weak self] in
                    guard let self = self else { return true }
                    return !($0.userId ?? "").isEmpty && $0.userId != manager.coreUserState.selfInfo.userId
                }
                let seatListCount = manager.coreSeatState.seatList.count
                self.onTheSeatHeaderLabel.text = .localizedReplace(.onSeatListText, replace: "\(onTheSeatList.count) / \(seatListCount - 1)")
                self.tableView.reloadData()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeApplyTakeSeatState() {
        seatApplicationPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] applicationSeatList in
                guard let self = self else { return }
                self.applySeatList = applicationSeatList
                self.applySeatHeaderLabel.text = .localizedReplace(.applySeatListText, replace: "\(applySeatList.count)")
                self.tableView.reloadData()
            }
            .store(in: &cancellableSet)
        
    }
    
    private func subscribeInviteState() {
        seatListPublisher
            .combineLatest(seatApplicationPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self] seatList, applicationSeatList in
                guard let self = self else { return }
                let onSeatList = seatList.filter{ [weak self] in
                    guard let self = self else { return true }
                    return !($0.userId ?? "").isEmpty && $0.userId != self.manager.coreUserState.selfInfo.userId
                }
                self.inviteContentView.isHidden = (onSeatList.count != 0 || applicationSeatList.count != 0)
            }
            .store(in: &cancellableSet)
        
        manager.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] message in
                guard let self = self else { return }
                self.makeToast(message)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeSeatModeState() {
        manager.subscribeCoreState(StateSelector(keyPath: \SGRoomState.seatMode))
            .receive(on: RunLoop.main)
            .sink { [weak self] seatMode in
                guard let self = self else { return }
                self.seatModeSwitch.isOn = seatMode == .applyToTake
            }
            .store(in: &cancellableSet)
    }
    
    deinit {
        cancellableSet.forEach { $0.cancel() }
        cancellableSet.removeAll()
    }
}

extension VRSeatManagerPanel {
    @objc
    private func seatModeSwitchClick(sender: UISwitch) {
        coreView?.updateRoomSeatMode(seatMode: sender.isOn ? .applyToTake : .freeToTake, onSuccess: {
        }, onError: { [weak self] code, message in
            guard let self = self else { return }
            let err = InternalError(code: code, message: message)
            manager.onError(err.localizedMessage)
        })
    }
    
    @objc
    private func inviteButtonClick(sender: UIButton) {
        guard let coreView = coreView else { return }
        routerManager.router(action: .present(.linkInviteControl(coreView, -1)))
    }
}

extension VRSeatManagerPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let headerView = UIView(frame: CGRect(x: 0, y: 0, width: tableView.frame.width, height: 30.scale375()))
        headerView.backgroundColor = .clear
        if section == 0 && onTheSeatList.count > 0 {
            headerView.addSubview(onTheSeatHeaderLabel)
            onTheSeatHeaderLabel.snp.makeConstraints { make in
                make.leading.equalToSuperview().offset(24.scale375())
                make.trailing.equalToSuperview().offset(-24.scale375())
                make.top.bottom.equalToSuperview()
            }
        }
        if section == 1 && applySeatList.count > 0 {
            headerView.addSubview(applySeatHeaderLabel)
            applySeatHeaderLabel.snp.makeConstraints { make in
                make.leading.equalToSuperview().offset(24.scale375())
                make.trailing.equalToSuperview().offset(-24.scale375())
                make.top.bottom.equalToSuperview()
            }
        }
        return headerView
    }
    
    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        if section == 0 && onTheSeatList.count > 0 {
            return 30.scale375()
        }
        if section == 1 && applySeatList.count > 0 {
            return 30.scale375()
        }
        return 0
    }
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 55.scale375Height()
    }
}

extension VRSeatManagerPanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if section == 0 {
            return onTheSeatList.count
        } else {
            return applySeatList.count
        }
    }
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return 2
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        if indexPath.section == 0 {
            let cell = tableView.dequeueReusableCell(withIdentifier: VRTheSeatCell.identifier, for: indexPath)
            if let onTheSeatCell = cell as? VRTheSeatCell {
                onTheSeatCell.updateSeatInfo(seatInfo: onTheSeatList[indexPath.row])
                onTheSeatCell.kickoffEventClosure = { [weak self] seatInfo in
                    guard let self = self else { return }
                    self.coreView?.kickUserOffSeatByAdmin(userId: seatInfo.userId ?? "") {
                    } onError: { [weak self] code, message in
                        guard let self = self else { return }
                        let error = InternalError(code: code, message: message)
                        self.manager.onError(error.localizedMessage)
                    }
                }
            }
            return cell
        } else {
            let cell = tableView.dequeueReusableCell(withIdentifier: VRApplyTakeSeatCell.identifier, for: indexPath)
            if let applyTakeSeatCell = cell as? VRApplyTakeSeatCell {
                applyTakeSeatCell.updateSeatApplication(seatApplication: applySeatList[indexPath.row])
                applyTakeSeatCell.approveEventClosure = { [weak self] seatApplication in
                    guard let self = self else { return }
                    self.coreView?.responseRemoteRequest(userId: seatApplication.userId, agree: true) { [weak self] in
                        guard let self = self else { return }
                        self.manager.onRespondedRemoteRequest()
                    } onError: { [weak self] code, message in
                        guard let self = self else { return }
                        let error = InternalError(code: code, message: message)
                        self.manager.onError(error.localizedMessage)
                    }
                }
                
                applyTakeSeatCell.rejectEventClosure = { [weak self] seatApplication in
                    guard let self = self else { return }
                    self.coreView?.responseRemoteRequest(userId: seatApplication.userId, agree: false) { [weak self] in
                        guard let self = self else { return }
                        self.manager.onRespondedRemoteRequest()
                    } onError: { [weak self] code, message in
                        guard let self = self else { return }
                        manager.onRemoteRequestError(userId: seatApplication.userId)
                        let error = InternalError(code: code, message: message)
                        self.manager.onError(error.localizedMessage)
                    }
                }
            }
            return cell
        }
    }
}

fileprivate extension String {
    static let seatControlTitleText = internalLocalized("Link Management")
    static let needRequestText = internalLocalized("Require owner's consent to speak")
    static let onSeatListText = internalLocalized("On Seat List (xxx)")
    static let applySeatListText = internalLocalized("Application List (xxx)")
    static let inviteText = internalLocalized("Invite")
    static let inviteAudienceText = internalLocalized("No users in the seat, go to invite")
}
