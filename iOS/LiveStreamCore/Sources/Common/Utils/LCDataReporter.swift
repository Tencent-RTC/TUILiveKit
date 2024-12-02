//
//  LCDataReporter.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/11/29.
//

import RTCRoomEngine

class LCDataReporter {
    enum MetricsEvent: Int {
        case panelShowLiveCoreView = 190026;
        case panelHideLiveCoreView = 190027;
        case methodCallLiveStreamStartCamera = 190028
        case methodCallLiveStreamStartMicrophone = 190029
        case methodCallLiveStreamMuteMicrophone = 190030
        case methodCallLiveStreamStopCamera = 190031
        case methodCallLiveStreamStopMicrophone = 190032
        case methodCallLiveStreamCreateRoom = 190033
        case methodCallLiveStreamDestroyRoom = 190034
        case methodCallLiveStreamJoinRoom = 190035
        case methodCallLiveStreamLeaveRoom = 190036
        case methodCallLiveStreamRequestIntraRoomConnection = 190037
        case methodCallLiveStreamCancelIntraRoomConnection = 190038
        case methodCallLiveStreamRespondIntraRoomConnection = 190039
        case methodCallLiveStreamDisconnectUser = 190040
        case methodCallLiveStreamTerminateIntraRoomConnection = 190041
        case methodCallLiveStreamRequestCrossRoomConnection = 190042
        case methodCallLiveStreamCancelCrossRoomConnection = 190043
        case methodCallLiveStreamRespondCrossRoomConnection = 190044
        case methodCallLiveStreamTerminateCrossRoomConnection = 190045
        case methodCallLiveStreamSetLayoutMode = 190046
        case methodCallLiveStreamSetVideoViewDelegate = 190047
    }
    
    static func reportEventData(event: MetricsEvent) {
        let apiParams: [String : Any] = [
            "api": "KeyMetricsStats",
            "params": [
                "key": event.rawValue,
            ],
        ]
        
        do {
            let jsonData = try JSONSerialization.data(withJSONObject: apiParams, options: .prettyPrinted)
            if let jsonString = String(data: jsonData, encoding: .utf8) {
                TUIRoomEngine.callExperimentalAPI(jsonStr: jsonString)
            } else {
                debugPrint("Error converting JSON data to string")
            }
        } catch {
            debugPrint("Error converting dictionary to JSON: \(error.localizedDescription)")
        }
    }
}
