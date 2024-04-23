//
// TUIBarrageService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

protocol TUIBarrageServiceDelegate: AnyObject {
    func willSendBarrage(_ barrage: TUIBarrage)
    func didSendBarrage(_ barrage: TUIBarrage)
    func didReceiveBarrage(_ barrage: TUIBarrage)
}

class TUIBarrageService: NSObject {
    var roomId: String = ""
    weak var delegate:TUIBarrageServiceDelegate?

    func initialize() {}

    func destroy() {}

    func sendBarrage(_ barrage: TUIBarrage) {}

    func onReceiveBarrage(_ barrage: TUIBarrage) {}
}
