//
//  StreamDashboardService.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/21.
//

import Foundation
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

protocol StreamDashboardService {
    
    func addRoomEngineObserver(_ observer: TUIRoomObserver)
    func removeRoomEngineObserver(_ observer: TUIRoomObserver)
    func addTRTCObserver(_ observer: TRTCCloudDelegate)
    func removeTRTCObserver(_ observer: TRTCCloudDelegate)
}


class EngineStreamDashboardService {
    
    private let roomEngine: TUIRoomEngine
    private let trtcCloud: TRTCCloud
    init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
        self.trtcCloud = roomEngine.getTRTCCloud()
    }
    
}

extension EngineStreamDashboardService: StreamDashboardService {
    
    func addRoomEngineObserver(_ observer: any TUIRoomObserver) {
        roomEngine.addObserver(observer)
    }
    
    func removeRoomEngineObserver(_ observer: any TUIRoomObserver) {
        roomEngine.removeObserver(observer)
    }

    func addTRTCObserver(_ observer: any TRTCCloudDelegate) {
        trtcCloud.addDelegate(observer)
    }

    func removeTRTCObserver(_ observer: any TRTCCloudDelegate) {
        trtcCloud.removeDelegate(observer)
    }
}

