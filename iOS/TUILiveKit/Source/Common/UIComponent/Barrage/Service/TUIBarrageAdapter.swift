//
//  TUIBarrageAdapter.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import ImSDK_Plus
import RTCRoomEngine
import TUICore

class TUIBarrageWrapper: Codable {
    var version: String
    var businessID: String
    var platform: String
    var data: TUIBarrage

    init(version: String = "1.0", businessID: String = "TUIBarrage", platform: String = "iOS", data: TUIBarrage) {
        self.version = version
        self.businessID = businessID
        self.platform = platform
        self.data = data
    }

    enum CodingKeys: String, CodingKey {
        case version
        case businessID
        case platform
        case data
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(version, forKey: .version)
        try container.encode(businessID, forKey: .businessID)
        try container.encode(platform, forKey: .platform)
        try container.encode(data, forKey: .data)
    }

    required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        version = try container.decode(String.self, forKey: .version)
        businessID = try container.decode(String.self, forKey: .businessID)
        platform = try container.decode(String.self, forKey: .platform)
        data = try container.decode(TUIBarrage.self, forKey: .data)
    }
}

class TUIBarrageAdapter: TUIBarrageService {
    private let imManager: V2TIMManager = {
        V2TIMManager.sharedInstance()
    }()

    private let engineManager: TUIRoomEngine = {
        TUIRoomEngine.sharedInstance()
    }()
    
    static func defaultCreate(roomId: String, delegate:TUIBarrageServiceDelegate) -> TUIBarrageAdapter {
        let service = TUIBarrageAdapter()
        service.roomId = roomId
        service.delegate = delegate
        return service
    }
    
    override func initialize() {
        imManager.addSimpleMsgListener(listener: self)
        engineManager.addObserver(self)
    }

    override func destroy() {
        imManager.removeSimpleMsgListener(listener: self)
        engineManager.removeObserver(self)
    }

    override func sendBarrage(_ barrage: TUIBarrage) {
        LiveKitLog.info("\(#file)", "\(#line)","sendBarrage:[barrage.content::\(barrage.content)]")
        delegate?.willSendBarrage(barrage)
        imManager.sendGroupTextMessage(barrage.content, to: roomId, priority: .PRIORITY_NORMAL) { [weak self] in
            guard let self = self else { return }
            self.delegate?.didSendBarrage(barrage)
            TUIBarrageStore.shared.barrage.value = barrage
        } fail: { error, message in
            debugPrint("sendGroupTextMessage failed. code:\(error), message:\(message ?? "")")
        }
    }

    override func onReceiveBarrage(_ barrage: TUIBarrage) {
        delegate?.didReceiveBarrage(barrage)
    }
}

extension TUIBarrageAdapter: V2TIMSimpleMsgListener {
    func onRecvGroupTextMessage(_ msgID: String!, groupID: String!, sender info: V2TIMGroupMemberInfo!, text: String!) {
        LiveKitLog.info("\(#file)", "\(#line)",
                        """
                        onRecvGroupTextMessage:[msgID:\(String(describing: msgID))], 
                        [groupID:\(String(describing: groupID))],
                        [sender:\(String(describing: info))],
                        [text:\(String(describing: text))]
                        """)
        if self.roomId != groupID { return }
        constructBarrageFromIMText(groupId: groupID, info: info, text: text)
    }

    private func constructBarrageFromIMText(groupId: String, info: V2TIMGroupMemberInfo!, text: String) {
        let barrage = TUIBarrage()
        barrage.user.userId = info.userID
        barrage.user.userName = info.nickName
        barrage.user.avatarUrl = info.faceURL
        barrage.user.level = "0"
        barrage.content = text
        onReceiveBarrage(barrage)
    }
}

extension TUIBarrageAdapter: TUIRoomObserver {
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        LiveKitLog.info("\(#file)", "\(#line)", 
                                                 """
                                                 onRemoteUserEnterRoom:[roomId:\(roomId)],
                                                 [userInfo:\(userInfo)], [userInfo.userId:\(userInfo.userId),
                                                 [userInfo.userName:\(userInfo.userName)]
                                                 """)
        constructBarrageFromEngine(userInfo: userInfo)
    }

    private func constructBarrageFromEngine(userInfo: TUIUserInfo) {
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
    static let comingText = {
        localized("live.barrage.coming")
    }()
}

