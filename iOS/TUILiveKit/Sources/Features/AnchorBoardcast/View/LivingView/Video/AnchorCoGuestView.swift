//
//  CoGuestView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/25.
//

import Foundation
import Kingfisher
import Combine
import RTCRoomEngine
import RTCCommon
import AtomicXCore

class AnchorCoGuestView: UIView {
    private let manager: AnchorManager
    private let routerManager: AnchorRouterManager
    private var cancellableSet = Set<AnyCancellable>()
    private var isViewReady: Bool = false
    private var userInfo: TUIUserInfo
    
    init(userInfo: TUIUserInfo, manager: AnchorManager, routerManager: AnchorRouterManager) {
        self.userInfo = userInfo
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: .zero)
        let tapGestureRecognizer = UITapGestureRecognizer(target: self, action: #selector(handleTap))
        addGestureRecognizer(tapGestureRecognizer)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
        initViewState()
    }
    
    private lazy var userInfoView = AnchorUserStatusView(userInfo: userInfo, manager: manager)
    
    private func constructViewHierarchy() {
        addSubview(userInfoView)
    }

    private func activateConstraints() {
        userInfoView.snp.makeConstraints { make in
            make.height.equalTo(18)
            make.bottom.equalToSuperview().offset(-5)
            make.leading.equalToSuperview().offset(5)
            make.width.lessThanOrEqualTo(self).multipliedBy(0.9)
        }
    }
    
    private func initViewState() {
        if manager.coreCoHostState.connectedUserList.count > 1 || manager.coreCoGuestState.connectedUserList.count > 1 {
            userInfoView.isHidden = false
        } else {
            userInfoView.isHidden = true
        }
    }
    
    @objc private func handleTap() {
        let isSelfOwner = manager.coreUserState.selfInfo.userRole == .roomOwner
        let isSelfView = userInfo.userId == manager.coreUserState.selfInfo.userId
        let isOnlyUserOnSeat = manager.coreCoGuestState.connectedUserList.count == 1
        if !isSelfOwner && isOnlyUserOnSeat && !isSelfView { return }
        let type: AnchorUserManagePanelType = !isSelfOwner && !isSelfView ? .userInfo : .mediaAndSeat
        routerManager.router(action: .present(.userManagement(userInfo, type: type)))
    }
}

extension AnchorCoGuestView {
    func subscribeState() {
        FloatWindow.shared.subscribeShowingState()
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isShow in
                guard let self = self else { return }
                isHidden = isShow
            }
            .store(in: &cancellableSet)
    }
}
