//
//  BarrageDisplayManager.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/14.
//

import ImSDK_Plus
import RTCRoomEngine
import TUICore

protocol BarrageDisplayManagerDelegate: AnyObject {
    func didReceiveBarrage(_ barrage: TUIBarrage)
}

class BarrageDisplayManager: NSObject {
    private let roomId: String
    weak var delegate: BarrageDisplayManagerDelegate?
    
    private let imManager: V2TIMManager = V2TIMManager.sharedInstance()
    private let engineManager: TUIRoomEngine = TUIRoomEngine.sharedInstance()
    
    init(roomId: String, delegate: BarrageDisplayManagerDelegate? = nil) {
        self.roomId = roomId
        self.delegate = delegate
        super.init()
        imManager.addSimpleMsgListener(listener: self)
        engineManager.addObserver(self)
    }
    
    deinit {
        engineManager.removeObserver(self)
    }

    func onReceiveBarrage(_ barrage: TUIBarrage) {
        delegate?.didReceiveBarrage(barrage)
    }
}

extension BarrageDisplayManager: V2TIMSimpleMsgListener {
    func onRecvGroupTextMessage(_ msgID: String, groupID: String, sender info: V2TIMGroupMemberInfo, text: String) {
        LiveKitLog.info("\(#file)", "\(#line)", "onRecvGroupTextMessage:[msgID:\(msgID), groupID:\(groupID), sender:\(String(describing: info)), text:\(text)]")
        guard self.roomId == groupID else { return }
        let barrage = TUIBarrage()
        barrage.user.userId = info.userID ?? ""
        barrage.user.userName = info.nickName ?? ""
        barrage.user.avatarUrl = info.faceURL ?? ""
        barrage.user.level = "0"
        barrage.content = text
        onReceiveBarrage(barrage)
    }
}

extension BarrageDisplayManager: TUIRoomObserver {
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        LiveKitLog.info("\(#file)", "\(#line)", "onRemoteUserEnterRoom:[roomId:\(roomId), userId:\(userInfo.userId), userName:\(userInfo.userName)]")
        guard self.roomId == roomId else { return }
        let barrage: TUIBarrage = TUIBarrage()
        barrage.user.userId = userInfo.userId
        barrage.user.userName = userInfo.userName
        barrage.user.avatarUrl = userInfo.avatarUrl
        barrage.user.level = ""
        barrage.content = " \(String.comingText)"
        onReceiveBarrage(barrage)
    }
}

private extension String {
    static let comingText: String = localized("live.barrage.coming")
}

