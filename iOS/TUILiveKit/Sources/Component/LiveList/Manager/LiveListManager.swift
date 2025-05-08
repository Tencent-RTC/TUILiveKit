//
//  LiveListManager.swift
//  AFNetworking
//
//  Created by gg on 2025/4/7.
//

import RTCRoomEngine
import RTCCommon
import Combine
import TUILiveResources

class LiveListManager {
    var state: LiveListState {
        observerState.state
    }
    var router: LiveListNavigationState {
        routerState.state
    }
    
    private let service = LiveListService(roomEngine: TUIRoomEngine.sharedInstance())
    private let observerState = ObservableState<LiveListState>(initialState: LiveListState())
    private let routerState = ObservableState<LiveListNavigationState>(initialState: LiveListNavigationState())
    
    func subscribeState<Value>(_ selector: StateSelector<LiveListState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
    
    func subscribeRouter<Value>(_ selector: StateSelector<LiveListNavigationState, Value>) -> AnyPublisher<Value, Never> {
        return routerState.subscribe(selector)
    }
    
    func getLiveList(cursor: String) {
        Task {
            do {
                let result = try await service.getLiveList(cursor: cursor)
                observerState.update { state in
                    state.liveInfoListResult.cursor = result.cursor
                    state.liveInfoListResult.liveInfoList = result.liveInfoList
                    state.liveInfoListResult.isFirstFetch = result.isFirstFetch
                }
            } catch let err as InternalError {
                
            }
        }
    }
}

extension LiveListManager {
    func routeTo(_ to: LiveListNavigationState.Router) {
        routerState.update { router in
            router.currentRouter = to
        }
    }
}
