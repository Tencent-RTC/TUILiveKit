//
//  SGDataReporter.swift
//  SeatGridView
//
//  Created by krabyu on 2024/11/29.
//

import Foundation
import RTCRoomEngine

class SGDataReporter {
    enum LiveKitMetricsEvent: Int {
        case panelShowSeatGridView = 191026
        case panelHideSeatGridView = 191027
        case methodCallSeatGridViewStartMicrophone = 191028
        case methodCallSeatGridViewStopMicrophone = 191029
        case methodCallSeatGridViewMuteMicrophone = 191030
        case methodCallSeatGridViewUnmuteMicrophone = 191031
        case methodCallSeatGridViewStartRoom = 191032
        case methodCallSeatGridViewStopRoom = 191033
        case methodCallSeatGridViewJoinRoom = 191034
        case methodCallSeatGridViewLeaveRoom = 191035
        case methodCallSeatGridViewUpdateSeatMode = 191036
        case methodCallSeatGridViewResponseRequest = 191037
        case methodCallSeatGridViewCancelRequest = 191038
        case methodCallSeatGridViewTakeSeat = 191039
        case methodCallSeatGridViewMoveToSeat = 191040
        case methodCallSeatGridViewLeaveSeat = 191041
        case methodCallSeatGridViewTakeUserOnSeat = 191042
        case methodCallSeatGridViewKickUserOffSeat = 191043
        case methodCallSeatGridViewLockSeat = 191044
        case methodCallSeatGridViewSetLayoutMode = 191045
        case methodCallSeatGridViewSetSeatViewDelegate = 191046
    }
    
    static func reportEventData(event: LiveKitMetricsEvent) {
        let apiParams: [String : Any] = [
            "api": "KeyMetricsStats",
            "params": [
                "key": event.rawValue,
            ],
          ]
        
        do {
            let jsonData = try JSONSerialization.data(withJSONObject: apiParams, options: .prettyPrinted)
            if let jsonString = String(data: jsonData, encoding: .utf8) {
                debugPrint(jsonString)
                TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonStr: jsonString) { message in
                }
            } else {
                debugPrint("Error converting JSON data to string")
            }
        } catch {
            debugPrint("Error converting dictionary to JSON: \(error.localizedDescription)")
        }
    }
}
