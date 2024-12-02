//
//  StreamDashboardManager.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/21.
//

import Foundation
import Combine
import RTCCommon

#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class StreamDashboardManager: NSObject {
 
    private let observableState: ObservableState<StreamDashboardState>
    var state: StreamDashboardState {
        observableState.state
    }
    
    private let service: StreamDashboardService
    init(service: StreamDashboardService, roomId: String) {
        self.service = service
        self.observableState = ObservableState(initialState: StreamDashboardState(roomId: roomId))
        super.init()
    }
    
 
    func addTRTCEvent() {
        service.addTRTCObserver(self)
    }
    
    func removeTRTCEvent() {
        service.removeTRTCObserver(self)
    }
    
}

extension StreamDashboardManager {
    
    func subscribe<Value>(_ selector: StateSelector<StreamDashboardState, Value>) -> AnyPublisher<Value, Never> {
        return observableState.subscribe(selector)
    }
}

extension StreamDashboardManager: TRTCCloudDelegate {
    
    func onStatistics(_ statistics: TRTCStatistics) {
        observableState.update { state in
            state.rtt = statistics.rtt
            state.downLoss = statistics.downLoss
            state.upLoss = statistics.upLoss
            state.localUsers = statistics.localStatistics.map({ data in
                return StreamDashboardUser(local: data)
            })
            state.remoteUsers = statistics.remoteStatistics.map({ data in
                return StreamDashboardUser(remote: data)
            })
        }
    }
}
