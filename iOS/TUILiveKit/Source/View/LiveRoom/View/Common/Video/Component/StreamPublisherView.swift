//
//  StreamPublisherView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation
import Combine

class StreamPublisherView: RenderView {
    private lazy var linkStatusPublisher = store.select(ViewSelectors.getLinkStatus)
    private var linkStatusCancellable: AnyCancellable?
    
    lazy var waitingLinkView: WaitLinkMicAnimationView = {
        let view = WaitLinkMicAnimationView()
        addSubview(view)
        view.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        view.isHidden = true
        return view
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        subscribeState()
    }
    
    override func updateView() {
        super.updateView()
        backgroundColor = .blackColor
        userInfoView.isHidden = true
        updateStatus()
    }

    func updateStatus() {
        store.dispatch(action: MediaActions.updateLocalVideoView(payload: self))
        switch store.viewState.linkStatus {
            case .none:
                self.avatarImageView.alpha = 0
                self.waitingLinkView.stopAnim()
            case .applying:
                self.waitingLinkView.showAnim()
                self.avatarImageView.alpha = 0
            case .linking,.pking:
                self.waitingLinkView.stopAnim()
                self.avatarImageView.alpha = 1
        }
    }
    
    deinit {
        linkStatusCancellable?.cancel()
        linkStatusCancellable = nil
        print("deinit  \(type(of: self))")
    }
}

extension StreamPublisherView {
    
    private func subscribeState() {
        linkStatusCancellable?.cancel()
        linkStatusCancellable = nil
        linkStatusCancellable = linkStatusPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                self.updateStatus()
            }
    }

}
