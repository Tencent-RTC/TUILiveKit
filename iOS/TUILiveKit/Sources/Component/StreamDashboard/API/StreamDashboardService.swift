//
//  StreamDashboardService.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/21.
//

import Foundation
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

protocol StreamDashboardService {
    
    func addTRTCObserver(_ observer: TRTCCloudDelegate)
    func removeTRTCObserver(_ observer: TRTCCloudDelegate)
}


class EngineStreamDashboardService {
    
    private let trtcCloud: TRTCCloud
    
    init(trtcCloud: TRTCCloud) {
        self.trtcCloud = trtcCloud
    }
    
}

extension EngineStreamDashboardService: StreamDashboardService {
    
    func addTRTCObserver(_ observer: any TRTCCloudDelegate) {
        trtcCloud.addDelegate(observer)
    }
    
    func removeTRTCObserver(_ observer: any TRTCCloudDelegate) {
        trtcCloud.removeDelegate(observer)
    }
}

