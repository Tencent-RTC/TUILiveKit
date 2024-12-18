//
//  BarrageInputManager.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/13.
//

import Combine
import TUICore

class BarrageInputManager {
    private let roomId: String
    private let service: BarrageInputService
    
    let toastSubject = PassthroughSubject<String, Never>()
    let sendBarrageSubject = PassthroughSubject<TUIBarrage, Never>()
    
    init(roomId: String) {
        self.roomId = roomId
        self.service = BarrageInputService(roomId: roomId)
    }
    
    func send(text: String) {
        let barrage = TUIBarrage()
        barrage.content = text
        let userId = TUILogin.getUserID() ?? ""
        barrage.user.userName = TUILogin.getNickName() ?? userId
        barrage.user.userId = userId
        barrage.user.avatarUrl = TUILogin.getFaceUrl() ?? ""
        barrage.user.level = "0"
        Task {
            do {
                try await service.sendBarrage(barrage)
                sendBarrageSubject.send(barrage)
            } catch let err {
                toastSubject.send(err.localizedDescription)
            }
        }
    }
}
