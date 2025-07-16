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
                LiveKitLog.info("\(#file)", "\(#line)", "sendBarrage:\(text)")
                BarrageManager.shared.sendBarrageSubject.send((roomId, barrage))
                BarrageManager.shared.inputString = ""
            } catch let err as InternalError {
                LiveKitLog.info("\(#file)", "\(#line)", "sendBarrage failed:\(text)")
                BarrageManager.shared.toastSubject.send(err.localizedMessage)
            }
        }
    }
}
