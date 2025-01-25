//
//  VRSeatInvitationPanel.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/25.
//

import RTCCommon
import Combine
import TUICore
import SeatGridView
import RTCRoomEngine

class VRSeatInvitationPanel: RTCBaseView {
    private let manager: VoiceRoomManager
    private let routerManager: VRRouterManager
    private weak var coreView: SeatGridView?
    private var cancellableSet: Set<AnyCancellable> = []
    private var audienceTupleList: [(audienceInfo: VRUser, isInvited: Bool)] = []
    private let seatIndex: Int
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = UIFont.customFont(ofSize: 20)
        label.text = .inviteText
        return label
    }()
    
    private let subTitleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = UIFont.customFont(ofSize: 16, weight: .medium)
        label.text = .onlineAudienceText
        return label
    }()
    
    private let tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.backgroundColor = .clear
        tableView.register(VRInviteTakeSeatCell.self, forCellReuseIdentifier: VRInviteTakeSeatCell.identifier)
        tableView.contentInset = UIEdgeInsets(top: 10, left: 0, bottom: 0, right: 0)
        return tableView
    }()
    
    init(manager: VoiceRoomManager, routerManager: VRRouterManager, coreView: SeatGridView, seatIndex: Int) {
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        self.seatIndex = seatIndex
        super.init(frame: .zero)
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
    
    override func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(subTitleLabel)
        addSubview(tableView)
    }
    
    override func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
        }
        
        subTitleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(24.scale375())
            make.height.equalTo(30.scale375Height())
            make.top.equalTo(titleLabel.snp.bottom)
            make.width.equalToSuperview()
        }
        
        tableView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(575.scale375Height())
            make.top.equalTo(subTitleLabel.snp.bottom)
        }
    }
    
    override func bindInteraction() {
        tableView.delegate = self
        tableView.dataSource = self
        subscribeUserListState()
        subscribeToastState()
    }
    
    deinit {
        cancellableSet.forEach { $0.cancel() }
        cancellableSet.removeAll()
    }
}

extension VRSeatInvitationPanel {
    private func subscribeUserListState() {
        let userListPublisher = manager.subscribeUserState(StateSelector(keyPath: \.userList))
        let seatListPublisher = manager.subscribeSeatState(StateSelector(keyPath: \.seatList))
        let invitedUserIdsPublisher = manager.subscribeSeatState(StateSelector(keyPath: \.invitedUserIds))
        userListPublisher
            .combineLatest(seatListPublisher, invitedUserIdsPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self] userList, seatList, invitedUsers in
                guard let self = self else { return }
                let audienceList = userList.filter { [weak self] user in
                    guard let self = self else { return false }
                    return user.userId != self.manager.userState.selfInfo.userId
                }
                self.audienceTupleList = audienceList.filter { user in
                    !seatList.contains { $0.userId == user.userId }
                }.map { audience in
                    (audience, self.manager.seatState.invitedUserIds.contains(where: {$0 == audience.userId}))
                }
                self.tableView.reloadData()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeToastState() {
        manager.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] message in
                guard let self = self else { return }
                self.makeToast(message)
            }
            .store(in: &cancellableSet)
    }
}

extension VRSeatInvitationPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 55.scale375Height()
    }
}

extension VRSeatInvitationPanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return audienceTupleList.count
    }
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: VRInviteTakeSeatCell.identifier, for: indexPath)
        if let inviteTakeSeatCell = cell as? VRInviteTakeSeatCell {
            let audienceTuple = audienceTupleList[indexPath.row]
            inviteTakeSeatCell.updateUser(user: audienceTuple.audienceInfo)
            inviteTakeSeatCell.updateButtonView(isSelected: audienceTuple.isInvited)
            inviteTakeSeatCell.inviteEventClosure = { [weak self] user in
                guard let self = self, !self.manager.seatState.invitedUserIds.contains(where: { $0 == user.userId}) else { return }
                self.manager.onSentSeatInvitation(to: user.userId)
                self.coreView?.takeUserOnSeatByAdmin(index: self.seatIndex,
                                                     timeout: kSGDefaultTimeout,
                                                     userId: user.userId) { [weak self] userInfo in
                    guard let self = self else { return }
                    self.manager.onRespondedSeatInvitation(of: user.userId)
                } onRejected: { [weak self] userInfo in
                    guard let self = self else { return }
                    self.manager.onRespondedSeatInvitation(of: user.userId)
                    self.manager.toastSubject.send(.inviteSeatCancelText)
                } onCancelled: { [weak self] userInfo in
                    guard let self = self else { return }
                    self.manager.onRespondedSeatInvitation(of: user.userId)
                } onTimeout: { [weak self] userInfo in
                    guard let self = self else { return }
                    self.manager.onRespondedSeatInvitation(of: user.userId)
                    self.manager.toastSubject.send(.inviteSeatCancelText)
                } onError: { [weak self] userInfo, code, message in
                    guard let self = self else { return }
                    self.manager.onRespondedSeatInvitation(of: user.userId)
                    guard let err = TUIError(rawValue: code) else { return }
                    let error = InternalError(error: err, message: message)
                    self.manager.toastSubject.send(error.localizedMessage)
                }
                
                if self.seatIndex != -1 {
                    self.routerManager.router(action: .routeTo(.anchor))
                }
            }
            inviteTakeSeatCell.cancelEventClosure = { [weak self] user in
                guard let self = self else { return }
                coreView?.cancelRequest(userId: user.userId) { [weak self] in
                    guard let self = self else { return }
                    self.manager.onRespondedSeatInvitation(of: user.userId)
                } onError: { [weak self] code, message in
                    guard let self = self, let err = TUIError(rawValue: code) else { return }
                    let error = InternalError(error: err, message: message)
                    self.manager.toastSubject.send(error.localizedMessage)
                }
            }
        }
        return cell
    }
}

fileprivate extension String {
    static let inviteText = localized("live.seat.invite")
    static let onlineAudienceText = localized("live.recent.online.audience")
    static let inviteSeatCancelText = localized("live.seat.inviteSeatCancel")
}
