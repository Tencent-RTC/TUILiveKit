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
import LiveStreamCore
import RTCRoomEngine
import TUILiveResources

class LSCoHostManagerPanel: RTCBaseView {
    
    private let kCoHostTimeout = 10
    private var cancellableSet: Set<AnyCancellable> = []
    
    private lazy var recommendedUsers: [TUIConnectionUser] = []
    private lazy var connectedUsers: [TUIConnectionUser] = []
    private lazy var sendRequests: [TUIConnectionUser] = []
    private let manager: LSCoHostManager
    private weak var coreView: LiveCoreView?
    
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
        button.setImage(internalImage("live_connection_disconnect"), for: .normal)
        button.backgroundColor = .clear
        button.isHidden = true
        return button
    }()
    
    private let tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.backgroundColor = .clear
        tableView.register(LSCoHostUserCell.self, forCellReuseIdentifier: LSCoHostUserCell.identifier)
        tableView.register(LSCoHostUserTableHeaderView.self, forHeaderFooterViewReuseIdentifier: LSCoHostUserTableHeaderView.identifier)
        tableView.contentInset = UIEdgeInsets(top: 10, left: 0, bottom: 0, right: 0)
        return tableView
    }()
    
    init(manager: LSCoHostManager, coreView: LiveCoreView) {
        self.manager = manager
        self.coreView = coreView
        super.init(frame: .zero)
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        self.refreshRoomListData()
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

extension LSCoHostManagerPanel {
    
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
            let cursor = manager.state.recommendedListCursor
            if cursor != "" {
                // FIXME: 这里需不需要异步等待，后续验证
                self.manager.fetchRecommendedList(cursor: cursor)
                self.tableView.es.stopLoadingMore()
            } else {
                self.tableView.es.noticeNoMoreData()
            }
        }
        tableView.es.startPullToRefresh()
    }
    
    private func refreshRoomListData() {
        tableView.reloadData()
        self.manager.fetchRecommendedList()
    }
    
    private func subscribeConnectionState() {
        guard let coreView = coreView else { return }
        let connectedUsersSelector = StateSelector(keyPath: \LSCoHostState.connectedUsers)
        let connectedUsersPublisher = manager.subscribeCoHostState(connectedUsersSelector)
        let sendRequestsSelector = StateSelector(keyPath: \CoHostState.sentConnectionRequestList)
        let sendRequestsPublisher = coreView.subscribeState(sendRequestsSelector)
        let recommendedUsersSelector = StateSelector(keyPath: \LSCoHostState.recommendedUsers)
        let recommendedUsersPublisher = manager.subscribeCoHostState(recommendedUsersSelector)
        
        connectedUsersPublisher
            .combineLatest(sendRequestsPublisher, recommendedUsersPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self]  connectedUsers, sendRequests, recommendedUsers in
                guard let self = self else { return }
                let cursor = self.manager.state.recommendedListCursor
                if recommendedUsers.count > 0, cursor == "" {
                    self.tableView.es.noticeNoMoreData()
                } else {
                    self.tableView.es.resetNoMoreData()
                }
                self.connectedUsers = connectedUsers.filter({ user in
                    return user.roomId != self.manager.state.currentRoomId
                })
                self.recommendedUsers = recommendedUsers.filter({ user in
                    return user.roomId != self.manager.state.currentRoomId
                })
                self.sendRequests = sendRequests
                disconnectButton.isHidden = connectedUsers.count <= 0
                tableView.reloadData()
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

// MARK: - Action
extension LSCoHostManagerPanel {
    @objc
    private func disconnect() {
        let alertInfo = LSAlertInfo(description: .disconnectAlertText,
                                  imagePath: nil,
                                  cancelButtonInfo: (String.disconnectAlertCancelText, .g3),
                                  defaultButtonInfo: (String.disconnectAlertDisconnectText, .b1)) { alertPanel in
            alertPanel.dismiss()
        } defaultClosure: { [weak self] alertPanel in
            guard let self = self else { return }
            coreView?.terminateCrossRoomConnection()
            manager.onCrossRoomConnectionTerminated()
            alertPanel.dismiss()
        }
        let alertPanel = LSAlertPanel(alertInfo: alertInfo)
        alertPanel.show()
    }
}

extension LSCoHostManagerPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let headerId = LSCoHostUserTableHeaderView.identifier
        guard let headerView = tableView.dequeueReusableHeaderFooterView(withIdentifier: headerId)
            as? LSCoHostUserTableHeaderView else {
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

extension LSCoHostManagerPanel: UITableViewDataSource {
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
        let cell = tableView.dequeueReusableCell(withIdentifier: LSCoHostUserCell.identifier, for: indexPath)
        if let connectionUserCell = cell as? LSCoHostUserCell {
            if indexPath.section == 0 && connectedUsers.count > 0 {
                connectionUserCell.updateUser(connectedUsers[indexPath.row])
            } else {
                connectionUserCell.updateUser(recommendedUsers[indexPath.row])
                connectionUserCell.inviteEventClosure = { [weak self] user in
                    guard let self = self else { return }
                    manager.onRequestConnection(user: user)
                    coreView?.requestCrossRoomConnection(roomId: user.roomId, timeOut: kCoHostTimeout, onSuccess: { [weak self] code in
                        guard let self = self, let code = code else { return }
                        if code != .success {
                            manager.onRequestConnectionFailed(roomId: user.roomId)
                            let error = InternalError(error: code, message: "")
                            manager.onError(error)
                        }
                    }, onError: { [weak self] err, msg in
                        guard let self = self else { return }
                        manager.onRequestConnectionFailed(roomId: user.roomId)
                        let error = InternalError(code: err.rawValue, message: msg)
                        manager.onError(error)
                    })
                }
            }
        }
        return cell
    }
}

fileprivate extension String {
    static let connectionTitleText = internalLocalized("Start Co-hosting")
    static let connectedTitleText = internalLocalized("Connecting")
    static let recommendedTitleText = internalLocalized("Suggested Hosts")
    static let disconnectText = internalLocalized("End Co-host")
    
    static let disconnectAlertText = internalLocalized("Are you sure you want to disconnect from other streamers?")
    static let disconnectAlertCancelText = internalLocalized("Cancel")
    static let disconnectAlertDisconnectText = internalLocalized("End Co-host")
}
