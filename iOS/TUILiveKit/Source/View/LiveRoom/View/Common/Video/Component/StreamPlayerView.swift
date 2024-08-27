//
//  StreamPlayerView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation
import Combine

class StreamPlayerView: RenderView {
    
    private var videoStreamCancellable: AnyCancellable?
    private var isOpenCameraStream: Bool = false
    
    deinit {
        videoStreamCancellable?.cancel()
        videoStreamCancellable = nil
        print("deinit  \(type(of: self))")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        subscribeHasVideoStream()
        isViewReady = true
    }
    
    override func updateView() {
        super.updateView()
        guard let renderModel = renderModel else { return }
        store.dispatch(action: UserActions.updateRemoteVideoView(payload: (renderModel.userId, .cameraStream, self)))
    }
    
    func subscribeHasVideoStream() {
        videoStreamCancellable?.cancel()
        videoStreamCancellable = nil
        videoStreamCancellable = videoStreamPublisher
            .receive(on:RunLoop.main)
            .sink{ [weak self] userList in
                guard let self = self, let renderModel = self.renderModel else { return }
                let hasVideoSteam = userList.contains(renderModel.userId)
                if hasVideoSteam && !isOpenCameraStream {
                    self.store.dispatch(action: UserActions.startPlayRemoteVideo(payload: (renderModel.userId, .cameraStream)))
                    self.isOpenCameraStream = true
                }
                if !hasVideoSteam {
                    self.store.dispatch(action: UserActions.stopPlayRemoteVideo(payload: (renderModel.userId, .cameraStream)))
                    self.isOpenCameraStream = false
                }
            }
    }
    
}
