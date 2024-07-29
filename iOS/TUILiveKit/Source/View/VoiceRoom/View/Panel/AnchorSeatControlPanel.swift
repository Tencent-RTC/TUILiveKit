//
//  AnchorSeatControlPanel.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/16.
//

import UIKit
import RTCCommon
import Combine

class AnchorSeatControlPanel: RTCBaseView {
    private let store: LiveStore
    private var cancellableSet: Set<AnyCancellable> = []
    private var onTheSeatList: [SeatInfo] = []
    private var applySeatList: [SeatApplication] = []
    private lazy var seatListCount = store.selectCurrent(SeatSelectors.getSeatCount)
    
    let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = UIFont.customFont(ofSize: 20)
        label.text = .seatControlTitleText
        return label
    }()
    
    let changeSeatModeLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .needRequestText
        label.adjustsFontSizeToFitWidth = true
        return label
    }()
    
    let seatModeSwitch: UISwitch = {
        let view = UISwitch()
        view.onTintColor = .b1
        return view
    }()
    
    let separatorLine: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.7)
        return view
    }()
    
    let tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.backgroundColor = .clear
        tableView.register(OnTheSeatCell.self, forCellReuseIdentifier: OnTheSeatCell.identifier)
        tableView.register(ApplyTakeSeatCell.self, forCellReuseIdentifier: ApplyTakeSeatCell.identifier)
        tableView.contentInset = UIEdgeInsets(top: 10, left: 0, bottom: 0, right: 0)
        return tableView
    }()
    
    lazy var onTheSeatHeaderLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 24.scale375(), 
                                          y: 0, 
                                          width: (tableView.frame.width - 20).scale375Height(),
                                          height: 30.scale375()))
        label.textColor = .g7
        label.font = .customFont(ofSize: 16, weight: .medium)
        return label
    }()
    
    lazy var applySeatHeaderLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 24.scale375(),
                                          y: 0,
                                          width: (tableView.frame.width - 20).scale375Height(),
                                          height: 30.scale375()))
        label.textColor = .g7
        label.font = .customFont(ofSize: 16, weight: .medium)
        return label
    }()
    
    init(store: LiveStore) {
        self.store = store
        super.init(frame: .zero)
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
    
    override func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(changeSeatModeLabel)
        addSubview(seatModeSwitch)
        addSubview(separatorLine)
        addSubview(tableView)
    }
    
    override func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
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
    }
    
    override func bindInteraction() {
        tableView.delegate = self
        tableView.dataSource = self
        seatModeSwitch.addTarget(self, action: #selector(seatModeSwitchClick(sender:)), for: .valueChanged)
        subscribeOnSeatListState()
        subscribeApplyTakeSeatState()
        subscribeSeatModeState()
    }
    
    private func subscribeOnSeatListState() {
        store.select(SeatSelectors.getSeatList)
            .receive(on: RunLoop.main)
            .sink { [weak self] seatList in
                guard let self = self else { return }
                self.onTheSeatList = seatList.filter{ [weak self] in
                    guard let self = self else { return true }
                    return !$0.userId.isEmpty && $0.userId != self.store.selectCurrent(UserSelectors.currentUserId)
                }
                self.onTheSeatHeaderLabel.text = .localizedReplace(.onSeatListText, replace: "\(onTheSeatList.count) / \(seatListCount - 1)")
                self.tableView.reloadSections(IndexSet(integer: 0), with: .fade)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeApplyTakeSeatState() {
        store.select(SeatSelectors.getSeatApplications)
            .receive(on: RunLoop.main)
            .sink { [weak self] applicationSeatList in
                guard let self = self else { return }
                self.applySeatList = applicationSeatList
                self.applySeatHeaderLabel.text = .localizedReplace(.applySeatListText, replace: "\(applySeatList.count)")
                self.tableView.reloadSections(IndexSet(integer: 1), with: .fade)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeSeatModeState() {
        store.select(RoomSelectors.getRoomSeatMode)
            .receive(on: RunLoop.main)
            .sink { [weak self] seatMode in
                guard let self = self else { return }
                self.seatModeSwitch.isOn = seatMode == .applyToTake
            }
            .store(in: &cancellableSet)
    }
}

extension AnchorSeatControlPanel {
    @objc
    private func seatModeSwitchClick(sender: UISwitch) {
        store.dispatch(action: RoomActions.setRoomSeatModeByAdmin(payload: sender.isOn ? .applyToTake : .freeToTake))
    }
}

extension AnchorSeatControlPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let headerView = UIView(frame: CGRect(x: 0, y: 0, width: tableView.frame.width, height: 30.scale375()))
        headerView.backgroundColor = .clear
        if section == 0 && onTheSeatList.count > 0 {
            headerView.addSubview(onTheSeatHeaderLabel)
        } 
        if section == 1 && applySeatList.count > 0 {
            headerView.addSubview(applySeatHeaderLabel)
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

extension AnchorSeatControlPanel: UITableViewDataSource {
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
            let cell = tableView.dequeueReusableCell(withIdentifier: OnTheSeatCell.identifier, for: indexPath)
            if let onTheSeatCell = cell as? OnTheSeatCell {
                onTheSeatCell.updateSeatInfo(seatInfo: onTheSeatList[indexPath.row])
                onTheSeatCell.kickoffEventClosure = { [weak self] seatInfo in
                    guard let self = self else { return }
                    self.store.dispatch(action: SeatActions.kickSeat(payload: seatInfo))
                }
            }
            return cell
        } else {
            let cell = tableView.dequeueReusableCell(withIdentifier: ApplyTakeSeatCell.identifier, for: indexPath)
            if let applyTakeSeatCell = cell as? ApplyTakeSeatCell {
                applyTakeSeatCell.updateSeatApplication(seatApplication: applySeatList[indexPath.row])
                applyTakeSeatCell.approveEventClosure = { [weak self] seatApplication in
                    guard let self = self else { return }
                    self.store.dispatch(action: SeatActions.responseSeatApplication(payload: (true, seatApplication.id)))
                }
                applyTakeSeatCell.rejectEventClosure = { [weak self] seatApplication in
                    guard let self = self else { return }
                    self.store.dispatch(action: SeatActions.responseSeatApplication(payload: (false, seatApplication.id)))
                }
            }
            return cell
        }
    }
}

extension String {
    fileprivate static let seatControlTitleText = localized("live.anchor.link.control.title")
    fileprivate static let needRequestText = localized("live.anchor.setting.need.request")
    fileprivate static let onSeatListText = localized("live.anchor.link.control.onSeatList.xxx")
    fileprivate static let applySeatListText = localized("live.anchor.link.control.applySeatList.xxx")
}
