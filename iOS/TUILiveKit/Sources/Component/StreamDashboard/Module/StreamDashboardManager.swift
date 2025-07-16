//
//  StreamDashboardManager.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/21.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine

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
    
 
    func addObserver() {
        service.addTRTCObserver(self)
        service.addRoomEngineObserver(self)
    }
    
    func removeObserver() {
        service.removeTRTCObserver(self)
        service.removeRoomEngineObserver(self)
    }

}

extension StreamDashboardManager {
    
    func subscribe<Value>(_ selector: StateSelector<StreamDashboardState, Value>) -> AnyPublisher<Value, Never> {
        return observableState.subscribe(selector)
    }
}

extension StreamDashboardManager: TUIRoomObserver {
    func onUserNetworkQualityChanged(networkList: [TUINetworkInfo]) {

        let userId = TUIRoomEngine.getSelfInfo().userId
        if let matchedInfo = networkList.first(where: { $0.userId == userId }) {
            observableState.update { state in
                state.rtt = matchedInfo.delay
                state.downLoss = matchedInfo.downLoss
                state.upLoss = matchedInfo.upLoss
            }
        }
    }
}

extension StreamDashboardManager: TRTCCloudDelegate {
    
    func onStatistics(_ statistics: TRTCStatistics) {
        observableState.update { state in
            state.localUsers = statistics.localStatistics.map({ data in
                return StreamDashboardUser(local: data)
            })
            state.remoteUsers = statistics.remoteStatistics.map({ data in
                return StreamDashboardUser(remote: data)
            })
        }
    }
}
