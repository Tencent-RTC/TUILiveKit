//
//  StreamPublisherView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation

class StreamPublisherView: RenderView {
    lazy var waitingLinkView: WaitLinkMicAnimationView = {
        let view = WaitLinkMicAnimationView()
        addSubview(view)
        view.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        view.isHidden = true
        return view
    }()

    override func updateView() {
        super.updateView()
        backgroundColor = .blackColor
        userInfoView.isHidden = true
        userInfo?.status.addObserver(self, closure: { [weak self] _, _ in
            self?.updateStatus()
        })
        roomEngineService?.setLocalVideoView(view: self)
        updateStatus()
    }

    func updateStatus() {
        
        if userInfo?.role.value == .anchor {
            waitingLinkView.stopAnim()
            avatarImageView.alpha = 1
        } else if userInfo?.role.value == .audience {
            if userInfo?.status.value == .applying {
                waitingLinkView.showAnim()
                avatarImageView.alpha = 0
            } else if userInfo?.status.value == .linking {
                roomEngineService?.setLocalVideoView(view: self)
                waitingLinkView.stopAnim()
                avatarImageView.alpha = 1
            }
        } else {
            avatarImageView.alpha = 0
            waitingLinkView.stopAnim()
        }
    }
    
    deinit {
        debugPrint("deinit \(self)")
    }
}
