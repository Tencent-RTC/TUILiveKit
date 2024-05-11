//
//  EngineManager.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/11.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import TUICore
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

protocol EngineServiceProvider {
    func getRoomEngineService(roomId:String) -> RoomEngineService
    func removeRoomEngineService(roomId:String)
}

class EngineManager:EngineServiceProvider {
    private var mRoomEngineMap:[String:RoomEngineService] = [:]
    func getRoomEngineService(roomId:String) -> RoomEngineService {
        guard let service = mRoomEngineMap[roomId] else{
            let service = RoomEngineService(liveRoomInfo: LiveRoomInfo(roomId: roomId))
            mRoomEngineMap[roomId] = service
            return service
        }
        return service
    }
    
    func removeRoomEngineService(roomId:String) {
        mRoomEngineMap.removeValue(forKey: roomId)
    }
}

extension EngineManager {
    static func login(sdkAppId:Int,userId:String,userSig:String,onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LiveKitLog.info("\(#file)","\(#line)","login:[sdkAppId:\(sdkAppId),userId:\(userId),userSig:\(userSig)]")
        TUIRoomEngine.login(sdkAppId: sdkAppId, userId: userId, userSig: userSig) {
            LiveKitLog.info("\(#file)","\(#line)","login:[onSuccess][sdkAppId:\(sdkAppId),userId:\(userId),userSig:\(userSig)]")
            onSuccess()
        } onError: { code, message in
            let log = "login:[sdkAppId:\(sdkAppId),userId:\(userId),userSig:\(userSig)],code:\(code),message:\(message)]"
            LiveKitLog.error("\(#file)","\(#line)",log)
            onError(code, message)
        }
    }
    
    static func logout(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LiveKitLog.info("\(#file)","\(#line)","logout")
        TUIRoomEngine.logout  {
            LiveKitLog.info("\(#file)","\(#line)","logout:[onSuccess]")
            onSuccess()
        } onError: { code, message in
            LiveKitLog.error("\(#file)","\(#line)","logout:[code:\(code),message:\(message)]")
            onError(code, message)
        }
    }
}
