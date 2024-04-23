//
//  StreamPlayerView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation

class StreamPlayerView: RenderView {
    override func updateView() {
        super.updateView()
        guard let userInfo = userInfo else { return }
        userInfo.videoInfo.isCameraOpened.addObserver(self) { [weak self] isCameraOpened, _ in
            self?.avatarImageView.isHidden = isCameraOpened
        }
        roomEngineService?.setRemoteVideoView(userId: userInfo.userId,
                                                          streamType: .cameraStream,
                                                          view: self)
        roomEngineService?.startPlayRemoteVideo(userId: userInfo.userId,
                                                            streamType: .cameraStream)
    }
}
