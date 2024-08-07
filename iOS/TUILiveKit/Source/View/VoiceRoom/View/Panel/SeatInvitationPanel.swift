//
//  SeatInvitationPanel.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/25.
//

import RTCCommon
import Combine
import TUICore

class SeatInvitationPanel: RTCBaseView {
    private let store: LiveStore
    private let routerStore: RouterStore
    private var cancellableSet: Set<AnyCancellable> = []
    private var audienceTupleList: [(audienceInfo: User, isInvited: Bool)] = []
    private lazy var userListPublisher = store.select(UserSelectors.getUserList)
    private lazy var seatListPublisher = store.select(SeatSelectors.getSeatList)
    private lazy var seatInvitationMapPublisher = store.select(SeatSelectors.getSeatInvitationMap)
    private let seatIndex: Int
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = UIFont.customFont(ofSize: 20)
        label.text = .inviteText
        return label
    }()
    
    private let tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.backgroundColor = .clear
        tableView.register(InviteTakeSeatCell.self, forCellReuseIdentifier: InviteTakeSeatCell.identifier)
        tableView.contentInset = UIEdgeInsets(top: 10, left: 0, bottom: 0, right: 0)
        return tableView
    }()
    
    private lazy var inviteAudienceHeaderLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 24.scale375(),
                                          y: 0,
                                          width: (tableView.frame.width - 20).scale375Height(),
                                          height: 30.scale375()))
        label.textColor = .g7
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.text = .audienceText
        return label
    }()
    
    init(store: LiveStore, routerStore: RouterStore, seatIndex: Int) {
        self.store = store
        self.routerStore = routerStore
        self.seatIndex = seatIndex
        super.init(frame: .zero)
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
    
    override func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(tableView)
    }
    
    override func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
        }
        
        tableView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(575.scale375Height())
            make.top.equalTo(titleLabel.snp.bottom)
        }
    }
    
    override func bindInteraction() {
        tableView.delegate = self
        tableView.dataSource = self
        subscribeUserListState()
        subscribeToastState()
    }
}

extension SeatInvitationPanel {
    private func subscribeUserListState() {
        userListPublisher
            .combineLatest(seatListPublisher, seatInvitationMapPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self] userList, seatList, seatInvitationMap in
                guard let self = self else { return }
                let audienceList = userList.filter { [weak self] user in
                    guard let self = self else { return false }
                    return user.userId != self.store.selectCurrent(UserSelectors.currentUserId)
                }
                self.audienceTupleList = audienceList.filter { user in
                    !seatList.contains { $0.userId == user.userId }
                }.map { audience in
                    (audience, seatInvitationMap[audience.userId] != nil)
                }
                self.tableView.reloadData()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeToastState() {
        store.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] toast in
                guard let self = self else { return }
                self.makeToast(toast.message, duration: toast.duration, position: toast.position)
            }
            .store(in: &cancellableSet)
    }
}

extension SeatInvitationPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let headerView = UIView(frame: CGRect(x: 0, y: 0, width: tableView.frame.width, height: 30.scale375()))
        headerView.backgroundColor = .clear
        if section == 0 && audienceTupleList.count > 0 {
            headerView.addSubview(inviteAudienceHeaderLabel)
        }
        return headerView
    }
    
    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        if section == 0 && audienceTupleList.count > 0 {
            return 30.scale375()
        }
        return 0
    }
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 55.scale375Height()
    }
}

extension SeatInvitationPanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return audienceTupleList.count
    }
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: InviteTakeSeatCell.identifier, for: indexPath)
        if let inviteTakeSeatCell = cell as? InviteTakeSeatCell {
            let audienceTuple = audienceTupleList[indexPath.row]
            inviteTakeSeatCell.updateUser(user: audienceTuple.audienceInfo)
            inviteTakeSeatCell.updateButtonView(isSelected: audienceTuple.isInvited)
            inviteTakeSeatCell.inviteEventClosure = { [weak self] user in
                guard let self = self else { return }
                self.store.dispatch(action: SeatActions.inviteSeat(payload: (self.seatIndex, user)))
                if self.seatIndex != -1 {
                    self.routerStore.router(action: .dismiss())
                }
            }
            inviteTakeSeatCell.cancelEventClosure = { [weak self] user in
                guard let self = self else { return }
                if let seatInvitation = self.store.selectCurrent(SeatSelectors.getSeatInvitationMap)
                    .first(where: { $0.key == user.userId })
                    .map({ $0.value }) {
                    self.store.dispatch(action: SeatActions.cancelInviteSeat(payload: seatInvitation))
                }
            }
        }
        return cell
    }
}

fileprivate extension String {
    static let inviteText = localized("live.seat.invite")
    static let audienceText = localized("live.recent.online.audience")
}
