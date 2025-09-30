//
//  Constants.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/10/23.
//

import Foundation
import RTCRoomEngine

public struct Constants {
    static var component = DataReport.componentCoreView

    public struct DataReport {
        public static let kDataReportPanelShowLiveRoomBeautyEffect = 190_025
        public static let kDataReportPanelShowLiveRoomBeauty = 190_016
        
        public static let kDataReportLiveGiftSVGASendCount = 190_021
        public static let kDataReportLiveGiftSVGAPlayCount = 190_022
        public static let kDataReportLiveGiftEffectSendCount = 190_023
        public static let kDataReportLiveGiftEffectPlayCount = 190_024
        
        public static let kDataReportVoiceGiftSVGASendCount = 191_021
        public static let kDataReportVoiceGiftSVGAPlayCount = 191_022
        public static let kDataReportVoiceGiftEffectSendCount = 191_023
        public static let kDataReportVoiceGiftEffectPlayCount = 191_024

        public static let framework = 1
        public static let language = 3
        public static let componentLiveRoom = 21
        public static let componentVoiceRoom = 22
        public static let componentCoreView = 26
    }
    
    public struct URL {
        public static let defaultCover = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png"
        public static let defaultBackground = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background1.png"
    }

    struct JsonName {
        static let gridLayout = "livekit_video_layout_grid"
        static let floatLayout = "livekit_video_layout_float"
    }
}

public func reportComponent() {
    do {
        let apiParams: [String : Any] = [
            "api": "setFramework",
            "params": [
                "framework": Constants.DataReport.framework,
                "component": Constants.component,
                "language": Constants.DataReport.language,
            ],
        ]
        let jsonData = try JSONSerialization.data(withJSONObject: apiParams, options: .prettyPrinted)
        if let jsonString = String(data: jsonData, encoding: .utf8) {
            TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonStr: jsonString) { message in
            }
        } else {
            print("Error converting JSON data to string")
        }
    } catch {
        print("Error converting dictionary to JSON: \(error.localizedDescription)")
    }
}
