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
        updateLocalMicrophone(isOpened: true)
    }
    
    func stopMicrophone() {
        service.closeLocalMicrophone()
        updateLocalAudio(isMuted: false)
    }
    
    func muteMicrophone() {
        service.muteLocalAudio()
        updateLocalAudio(isMuted: true)
    }
    
    func unmuteMicrophone() async throws {
        try await self.service.unmuteLocalAudio()
        updateLocalAudio(isMuted: false)
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

extension SGMediaManager {
    private func updateLocalMicrophone(isOpened: Bool) {
        mediaState.isMicrophoneOpened = isOpened
    }
    
    private func updateLocalAudio(isMuted: Bool) {
        mediaState.isMicrophoneMuted = isMuted
    }
}
