//
//  SGMediaManager.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/16.
//

import Foundation
import RTCRoomEngine

class SGMediaManager {
    private(set) var mediaState = SGMediaState()
    private typealias Context = SeatGridViewManager.Context
    private weak var context: Context?
    private let service: SeatGridViewInterface
    
    init(context: SeatGridViewManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func startMicrophone() async throws {
        try await self.service.openLocalMicrophone()
        onMicrophoneStart()
    }
    
    func stopMicrophone() {
        service.closeLocalMicrophone()
        onMicrophoneStop()
    }
    
    func muteMicrophone() {
        service.muteLocalAudio()
        onMicrophoneMute()
    }
    
    func unmuteMicrophone() async throws {
        try await self.service.unmuteLocalAudio()
        onMicrophoneUnmute()
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

extension SGMediaManager {
    private func onMicrophoneStart() {
        mediaState.isMicrophoneOpened = true
    }
    
    private func onMicrophoneStop() {
        mediaState.isMicrophoneOpened = false
    }
    
    private func onMicrophoneMute() {
        mediaState.isMicrophoneMuted = true
    }
    
    private func onMicrophoneUnmute() {
        mediaState.isMicrophoneMuted = false
    }
}
