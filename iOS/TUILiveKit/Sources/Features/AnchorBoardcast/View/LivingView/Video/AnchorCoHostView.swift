//
//  CoHostView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/25.
//

import Foundation
import RTCCommon
import Combine
import RTCRoomEngine
import AtomicXCore

class AnchorCoHostView: UIView {
    private let manager: AnchorManager
    private var isViewReady: Bool = false
    private var coHostUser: CoHostUser
    private var cancellableSet = Set<AnyCancellable>()
    
    init(connectionUser: CoHostUser, manager: AnchorManager) {
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
        subscribeState()
        self.isUserInteractionEnabled = false
        
        manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \CoHostState.connectedUserList))
            .combineLatest(manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \CoGuestState.connectedUserList)))
            .receive(on: RunLoop.main)
            .sink { [weak self] coHostList, coGuestList in
                guard let self = self else { return }
                userInfoView.isHidden = !(coHostList.count > 1 || coGuestList.count > 1)
            }
            .store(in: &cancellableSet)
    }
    
    private lazy var userInfoView = AnchorUserStatusView(userInfo: TUIUserInfo(coHostUser: coHostUser), manager: manager)
    
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
}

extension AnchorCoHostView {
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
