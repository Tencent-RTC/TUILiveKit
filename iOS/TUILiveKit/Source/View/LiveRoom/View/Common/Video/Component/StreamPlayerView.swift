//
//  StreamPlayerView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation
import Combine

class StreamPlayerView: RenderView {
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        subscribeHasVideoStream()
    }
    
    override func updateView() {
        super.updateView()
        guard let seatInfo = seatInfo else { return }
        store.dispatch(action: UserActions.updateRemoteVideoView(payload: (seatInfo.userId , .cameraStream, self)))
        store.dispatch(action: UserActions.startPlayRemoteVideo(payload: (seatInfo.userId, .cameraStream)))
    }
    
    func subscribeHasVideoStream() {
        videoStreamPublisher
            .receive(on:RunLoop.main)
            .sink{ [weak self] userList in
                guard let self = self, let seatInfo = self.seatInfo else { return }
                let hasVideoSteam = userList.contains(seatInfo.userId)
                if hasVideoSteam {
                    self.store.dispatch(action: UserActions.startPlayRemoteVideo(payload: (seatInfo.userId, .cameraStream)))
                } else {
                    self.store.dispatch(action: UserActions.stopPlayRemoteVideo(payload: (seatInfo.userId, .cameraStream)))
                }
            }
            .store(in: &cancellableSet)
    }
    
}
