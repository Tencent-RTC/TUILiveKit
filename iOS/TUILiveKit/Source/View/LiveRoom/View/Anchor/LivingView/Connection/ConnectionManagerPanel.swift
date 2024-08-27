//
//  ConnectionInvitePanel.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/7.
//

import Foundation
import RTCCommon
import Combine
import TUICore
import ESPullToRefresh

class ConnectionManagerPanel: RTCBaseView {
    
    private let routerStore: RouterStore
    private let store: LiveStoreProvider
    private var cancellableSet: Set<AnyCancellable> = []
    
    private lazy var recommendedUsers: [ConnectionUser] = []
    private lazy var connectedUsers: [ConnectionUser] = []
    private lazy var sendRequests: [ConnectionUser] = []
    
    private lazy var connectedUsersPublisher = store.select(ConnectionSelectors.getConnectedUsers)
    private lazy var recommendedUsersPublisher = store.select(ConnectionSelectors.getRecommendedUsers)
    private lazy var sendRequestsPublisher = store.select(ConnectionSelectors.getSentConnectionRequests)
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.font = UIFont.customFont(ofSize: 20)
        label.text = .connectionTitleText
        return label
    }()
    
    private let disconnectButton: UIButton = {
        let button = UIButton()
        button.titleLabel?.font = UIFont.customFont(ofSize: 14)
        button.setTitleColor(.redColor, for: .normal)
        button.setTitle(.disconnectText, for: .normal)
        button.setImage(.liveBundleImage("live_connection_disconnect"), for: .normal)
        button.backgroundColor = .clear
        button.isHidden = true
        return button
    }()
    
    private let tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.backgroundColor = .clear
        tableView.register(ConnectionUserCell.self, forCellReuseIdentifier: ConnectionUserCell.identifier)
        tableView.register(ConnectionUserTableHeaderView.self, forHeaderFooterViewReuseIdentifier: ConnectionUserTableHeaderView.identifier)
        tableView.contentInset = UIEdgeInsets(top: 10, left: 0, bottom: 0, right: 0)
        return tableView
    }()
    
    init(store: LiveStoreProvider, routerStore: RouterStore) {
        self.store = store
        self.routerStore = routerStore
        super.init(frame: .zero)
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
    
    override func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(disconnectButton)
        addSubview(tableView)
    }
    
    override func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(20.scale375Height())
        }
        disconnectButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-16)
            make.centerY.equalTo(titleLabel)
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
        addRefreshDataEvent()
        subscribeConnectionState()
        subscribeToastState()
        disconnectButton.addTarget(self, action: #selector(disconnect), for: .touchUpInside)
    }
}

extension ConnectionManagerPanel {
    
    private func addRefreshDataEvent() {
        
        let header = ESRefreshHeaderAnimator(frame: CGRect.zero)
        header.pullToRefreshDescription = .pullToRefreshText
        header.releaseToRefreshDescription = .releaseToRefreshText
        header.loadingDescription = .loadingText
        
        let footer = ESRefreshFooterAnimator(frame: CGRect.zero)
        footer.loadingMoreDescription = .loadingMoreText
        footer.noMoreDataDescription = .noMoreDataText
        footer.loadingDescription = .loadingText
        
        tableView.es.addPullToRefresh(animator: header) { [weak self] in
            guard let self = self else { return }
            self.refreshRoomListData()
            self.tableView.es.stopPullToRefresh()
        }
        
        tableView.es.addInfiniteScrolling(animator: footer) { [weak self] in
            guard let self = self else { return }
            let cursor = self.store.selectCurrent(ConnectionSelectors.getRecommendedCursor)
            if cursor != "" {
                self.store.dispatch(action: ConnectionActions.getRecommendedList(payload: cursor))
                self.tableView.es.stopLoadingMore()
            } else {
                self.tableView.es.noticeNoMoreData()
            }
        }
        tableView.es.startPullToRefresh()
    }
    
    private func refreshRoomListData() {
        tableView.reloadData()
        store.dispatch(action: ConnectionActions.getRecommendedList(payload: ""))
    }
    
    private func subscribeConnectionState() {
        connectedUsersPublisher
            .combineLatest(sendRequestsPublisher, recommendedUsersPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self]  connectedUsers, sendRequests, recommendedUsers in
                guard let self = self else { return }
                let cursor = self.store.selectCurrent(ConnectionSelectors.getRecommendedCursor)
                if recommendedUsers.count > 0, cursor == "" {
                    self.tableView.es.noticeNoMoreData()
                } else {
                    self.tableView.es.resetNoMoreData()
                }
                self.connectedUsers = connectedUsers.filter({ user in
                    return user.roomId != self.store.roomState.roomId
                })
                self.recommendedUsers = recommendedUsers.filter({ user in
                    return user.roomId != self.store.roomState.roomId
                })
                self.sendRequests = sendRequests
                disconnectButton.isHidden = connectedUsers.count <= 0
                tableView.reloadData()
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

// MARK: - Action
extension ConnectionManagerPanel {
    @objc
    private func disconnect() {
        let alertInfo = AlertInfo(description: .disconnectAlertText,
                                  imagePath: nil,
                                  cancelButtonInfo: (String.disconnectAlertCancelText, .g3),
                                  defaultButtonInfo: (String.disconnectAlertDisconnectText, .b1)) { alertPanel in
            alertPanel.dismiss()
        } defaultClosure: { [weak self] alertPanel in
            guard let self = self else { return }
            self.store.dispatch(action: ConnectionActions.disconnect())
            alertPanel.dismiss()
        }
        store.dispatch(action: ViewActions.alertEvent(payload: alertInfo))
    }
}

extension ConnectionManagerPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let headerId = ConnectionUserTableHeaderView.identifier
        guard let headerView = tableView.dequeueReusableHeaderFooterView(withIdentifier: headerId)
            as? ConnectionUserTableHeaderView else {
            return nil
        }
        if section == 0 && connectedUsers.count > 0 {
            headerView.titleLabel.text = .connectedTitleText + "(\(connectedUsers.count))"
        } else {
            headerView.titleLabel.text = .recommendedTitleText
        }
        return headerView
    }
    
    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        return 30.scale375()
    }
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 55.scale375Height()
    }
}

extension ConnectionManagerPanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if section == 0 && connectedUsers.count > 0 {
            return connectedUsers.count
        }
        return recommendedUsers.count
    }
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return (connectedUsers.count > 0 ? 2:1)
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: ConnectionUserCell.identifier, for: indexPath)
        if let connectionUserCell = cell as? ConnectionUserCell {
            if indexPath.section == 0 && connectedUsers.count > 0 {
                connectionUserCell.updateUser(connectedUsers[indexPath.row])
            } else {
                connectionUserCell.updateUser(recommendedUsers[indexPath.row])
                connectionUserCell.inviteEventClosure = { [weak self] user in
                    guard let self = self else { return }
                    self.store.dispatch(action: ConnectionActions.requestConnection(payload: ([user.roomId], "")))
                }
            }
        }
        return cell
    }
}

fileprivate extension String {
    static let connectionTitleText = localized("live.connection.title")
    static let connectedTitleText = localized("live.connection.connected")
    static let recommendedTitleText = localized("live.connection.recommended.list")
    static let disconnectText = localized("live.connection.disconnect")
    
    static let disconnectAlertText = localized("live.connection.disconnect.alert")
    static let disconnectAlertCancelText = localized("live.alert.cancel")
    static let disconnectAlertDisconnectText = localized("live.connection.disconnect.alert.disconnect")
}
