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

class TUIBarrageAdapter:TUIBarrageService {
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
        delegate?.willSendBarrage(barrage)
        do {
            let barrageWrapper = TUIBarrageWrapper(data: barrage)
            let encoder = JSONEncoder()
            let data = try encoder.encode(barrageWrapper)
            imManager.sendGroupCustomMessage(data, to: roomId, priority: .PRIORITY_NORMAL) { [weak self] in
                guard let self = self else { return }
                self.delegate?.didSendBarrage(barrage)
               TUIBarrageStore.shared.barrage.value = barrage
            } fail: { code, message in
                debugPrint("sendGroupCustomMessage failed. code:\(code), message:\(message ?? "")")
            }
        } catch {
            debugPrint("Encoding TUIBarrage failed. error:\(error)")
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
        constructBarrageFromIMText(groupId: groupID, info: info, text: text)
    }
    
    func onRecvGroupCustomMessage(_ msgID: String!, groupID: String!, sender info: V2TIMGroupMemberInfo!, customData data: Data!) {
        LiveKitLog.info("\(#file)", "\(#line)",
                        """
                        onRecvGroupCustomMessage:[msgID:\(String(describing: msgID))],
                        [groupID:\(String(describing: groupID))],
                        [sender:\(String(describing: info))],
                        [data:\(String(describing: data))]
                        """)
        constructBarrageFromIMCustomData(groupId: roomId, customData: data)
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
    
    private func constructBarrageFromIMCustomData(groupId: String, customData data: Data) {
        if self.roomId == groupId {
            let decoder = JSONDecoder()
            do {
                let barrageWrapper = try decoder.decode(TUIBarrageWrapper.self, from: data)
                if barrageWrapper.businessID == "TUIBarrage" {
                    let barrage = barrageWrapper.data
                    onReceiveBarrage(barrage)
                }
            } catch {
                print("Decoding TUIBarrage failed. error:\(error)")
            }
        }
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

