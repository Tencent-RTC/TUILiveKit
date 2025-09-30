//
//  AudienceCoHostView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/25.
//

import Foundation
import RTCCommon
import Combine
import RTCRoomEngine
import AtomicXCore

class AudienceCoHostView: UIView {
    private let manager: AudienceManager
    private var isViewReady: Bool = false
    private var coHostUser: CoHostUser
    private var cancellableSet = Set<AnyCancellable>()
    
    init(connectionUser: CoHostUser, manager: AudienceManager) {
        self.coHostUser = connectionUser
        self.manager = manager
        super.init(frame: .zero)
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
        initViewState()
        subscribeState()
        self.isUserInteractionEnabled = false
    }
    
    private lazy var userInfoView = AudienceUserStatusView(userInfo: TUIUserInfo(coHostUser: coHostUser), manager: manager)
    
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
}

extension AudienceCoHostView {
    func subscribeState() {
        manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \CoHostState.connectedUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] connectedUsers in
                guard let self = self else { return }
                self.isHidden = connectedUsers.isEmpty
            }
            .store(in: &cancellableSet)
        
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
