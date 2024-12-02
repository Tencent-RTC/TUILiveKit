//
//  TUIBarrageManager.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

protocol TUIBarrageManagerDelegate: AnyObject {
    func willSendBarrage(_ barrage: TUIBarrage)
    func didSendBarrage(_ barrage: TUIBarrage)
    func didReceiveBarrage(_ barrage: TUIBarrage)
}

class TUIBarrageManager {
    private var roomId: String = ""
    private weak var delegate: TUIBarrageManagerDelegate?
    private var service:TUIBarrageService?
    static func defaultCreate(roomId: String, delegate: TUIBarrageManagerDelegate) -> TUIBarrageManager {
        let manager = TUIBarrageManager()
        manager.roomId = roomId
        manager.delegate = delegate
        return manager
    }
    
    func initService() {
        service = TUIBarrageAdapter.defaultCreate(roomId: roomId, delegate: self)
        service?.initialize()
    }
    
    func sendBarrage(_ barrage: TUIBarrage) {
        service?.sendBarrage(barrage)
    }

    func onReceiveBarrage(_ barrage: TUIBarrage) {
        delegate?.didReceiveBarrage(barrage)
    }
    
    deinit {
        service?.destroy()
    }
}

extension TUIBarrageManager: TUIBarrageServiceDelegate {
    func willSendBarrage(_ barrage: TUIBarrage) {
        delegate?.willSendBarrage(barrage)
    }

    func didSendBarrage(_ barrage: TUIBarrage) {
        delegate?.didSendBarrage(barrage)
    }

    func didReceiveBarrage(_ barrage: TUIBarrage) {
        onReceiveBarrage(barrage)
    }
}
