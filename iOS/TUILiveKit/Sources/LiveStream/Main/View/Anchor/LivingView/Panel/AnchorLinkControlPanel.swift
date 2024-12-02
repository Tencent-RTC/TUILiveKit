//
//  AnchorLinkControlPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/25.
//

import Foundation
import Combine
import RTCCommon
import LiveStreamCore
import RTCRoomEngine

class AnchorLinkControlPanel: UIView {
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    private weak var coreView: LiveCoreView?
    private var cancellable = Set<AnyCancellable>()
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    private var linkingList: [LSSeatInfo] = []
    private var applyList: [LSSeatApplication] = []
    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .anchorLinkControlTitle
        label.sizeToFit()
        return label
    }()

    private lazy var userListTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.backgroundColor = .clear
        tableView.register(LinkMicBaseCell.self, forCellReuseIdentifier: LinkMicBaseCell.cellReuseIdentifier)
        tableView.register(UserRequestLinkCell.self, forCellReuseIdentifier: UserRequestLinkCell.cellReuseIdentifier)
        tableView.register(UserLinkCell.self, forCellReuseIdentifier: UserLinkCell.cellReuseIdentifier)
        return tableView
    }()
    
    
    init(manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        subscribeSeatState()
    }

    private func subscribeSeatState() {
        manager.subscribeCoGuestState(StateSelector(keyPath: \LSCoGuestState.requestCoGuestList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] seatApplicationList in
                guard let self = self else { return }
                debugPrint("jeremiah seatApplicationList: \(seatApplicationList)")
                applyList = Array(seatApplicationList)
                self.userListTableView.reloadData()
            }
            .store(in: &cancellable)
        manager.subscribeCoGuestState(StateSelector(keyPath: \LSCoGuestState.connectedUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] seatList in
                guard let self = self else { return }
                linkingList = seatList.filter { !$0.userId.isEmpty && $0.userId != self.manager.userState.selfInfo.userId }
                userListTableView.reloadData()
            }
            .store(in: &cancellable)
    }
}

// MARK: Layout

extension AnchorLinkControlPanel {
    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.masksToBounds = true
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(userListTableView)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.height.equalTo(718.scale375Height())
            } else {
                make.width.equalTo(375.scale375())
            }
            make.edges.equalToSuperview()
        }

        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }

        titleLabel.snp.remakeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(titleLabel.mm_w)
        }

        userListTableView.snp.remakeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
        }
    }
}

// MARK: Action

extension AnchorLinkControlPanel {
    @objc func backButtonClick(sender: UIButton) {
        routerManager.router(action: .dismiss())
    }
}

extension AnchorLinkControlPanel: UITableViewDataSource {
    internal func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if section == 0 {
            return linkingList.count
        } else if section == 1 {
            return applyList.count
        } else {
            return 0
        }
    }

    func numberOfSections(in tableView: UITableView) -> Int {
        return 2
    }
}

extension AnchorLinkControlPanel: UITableViewDelegate {
    internal func tableView(_ tableView: UITableView,
                            cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case 0:
            return configureUserLinkCell(for: indexPath, in: tableView)
        case 1:
            return configureUserRequestLinkCell(for: indexPath, in: tableView)
        default:
            return tableView.dequeueReusableCell(withIdentifier: LinkMicBaseCell.cellReuseIdentifier, for: indexPath)
        }
    }

    internal func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    }

    internal func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 50.scale375()
    }

    func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let headerView = UIView(frame: CGRect(x: 0, y: 0, width: tableView.frame.width, height: 20.scale375Height()))
        headerView.backgroundColor = .g2
        let label = UILabel(frame: CGRect(x: 24, y: 0, width: headerView.frame.width , height: headerView.frame.height))
        if section == 0 {
            let maxSeatCount = manager.roomState.maxSeatCount
            label.text = .localizedReplace(.anchorLinkControlSeatCount,
                                          replace: "\(linkingList.count)/\(max(maxSeatCount,1) - 1)")
        } else if section == 1 {
            label.text = .localizedReplace(.anchorLinkControlRequestCount,
                                          replace: "\(applyList.count)")
        }
        label.textColor = .greyColor
        headerView.addSubview(label)
        return headerView
    }

    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        if section == 0 && linkingList.count > 0 {
            return 20.scale375Height()
        }
        
        if section == 1 && applyList.count > 0 {
            return 20.scale375Height()
        }
        
        return 0
    }
    
    func tableView(_ tableView: UITableView, viewForFooterInSection section: Int) -> UIView? {
        if section == 0 , linkingList.count > 0 {
            let footerView = UIView(frame: CGRect(x: 0, y: 0, width: tableView.frame.width, height: 7.0))
            footerView.backgroundColor = .g3.withAlphaComponent(0.1)
            return footerView
        } else {
            return nil
        }
    }
    
    func tableView(_ tableView: UITableView, heightForFooterInSection section: Int) -> CGFloat {
        if section == 0 , linkingList.count > 0 {
            return 7.scale375Height()
        } else {
            return 0
        }
    }
    
    private func configureUserLinkCell(for indexPath: IndexPath, in tableView: UITableView) -> UITableViewCell {
        guard indexPath.row < linkingList.count,
              let cell = tableView.dequeueReusableCell(withIdentifier: UserLinkCell.cellReuseIdentifier, for: indexPath) as? UserLinkCell else {
            return tableView.dequeueReusableCell(withIdentifier: LinkMicBaseCell.cellReuseIdentifier, for: indexPath)
        }
        
        cell.kickoffEventClosure = { [weak self] seatInfo in
            guard let self = self else { return }
            self.coreView?.disconnectUser(userId: seatInfo.userId) {
            } onError: { [weak self] code, message in
                   guard let self = self else { return }
                   let error = InternalError(error: code, message: message)
                   self.manager.toastSubject.send(error.localizedMessage)
            }
        }
        
        cell.seatInfo = linkingList[indexPath.row]
        cell.lineView.isHidden = (linkingList.count - 1) == indexPath.row
        return cell
    }

    private func configureUserRequestLinkCell(for indexPath: IndexPath, in tableView: UITableView) -> UITableViewCell {
        guard indexPath.row < applyList.count,
              let cell = tableView.dequeueReusableCell(withIdentifier: UserRequestLinkCell.cellReuseIdentifier, for: indexPath) as? UserRequestLinkCell else {
            return tableView.dequeueReusableCell(withIdentifier: LinkMicBaseCell.cellReuseIdentifier, for: indexPath)
        }
        
        cell.respondEventClosure = { [weak self] seatApplication, isAccepted in
            guard let self = self else { return }
            self.coreView?.respondIntraRoomConnection(userId: seatApplication.userId, isAccepted: isAccepted) {
                self.manager.removeSeatApplication(userId: seatApplication.userId)
            } onError: { [weak self] code, message in
                guard let self = self else { return }
                let error = InternalError(error: code, message: message)
                self.manager.toastSubject.send(error.localizedMessage)
            }
        }
        
        cell.seatApplication = applyList[indexPath.row]
        return cell
    }
}

private extension String {
    static var anchorLinkControlTitle: String {
        localized("live.anchor.link.control.title")
    }

    static var anchorLinkControlDesc: String {
        localized("live.anchor.link.control.desc")
    }
    
    static var anchorLinkControlSeatCount: String {
        localized("live.anchor.link.control.seat.count.xxx")
    }
    
    static var anchorLinkControlRequestCount: String {
        localized("live.anchor.link.control.request.count.xxx")
    }
    
}
