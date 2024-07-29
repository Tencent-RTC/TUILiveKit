//
//  LiveRoomViewStore.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/28.
//
import Combine

class LiveRoomViewStoreFactory: StoreFactory {

    typealias T = LiveRoomViewStoreProvider
    static var storeMap: [String : T] = [:]
    
    static func getStore(roomId: String) -> T {
        if let liveRoomViewStore = storeMap[roomId] {
            return liveRoomViewStore
        }
        let liveRoomViewStore = T()
        storeMap.updateValue(liveRoomViewStore, forKey: roomId)
        return liveRoomViewStore
    }
}

protocol LiveRoomViewStore {
    func select<Value: Equatable>(_ selector: Selector<LiveRoomViewState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<LiveRoomViewState, Value>) -> Value
    func dispatch(action: any Action)
}

class LiveRoomViewStoreProvider {
    private(set) lazy var store = Store(initialState: LiveRoomViewState())
    init() {
        initializeViewStore()
    }
    
    private func initializeViewStore() {
        store.register(reducer: liveRoomMenuReducer, for: \LiveRoomViewState.menu)
    }
}

extension LiveRoomViewStoreProvider: LiveRoomViewStore {
    func dispatch(action: any Action) {
        guard let action = action as? IdentifiableAction else { return }
        if action.id.contains(LiveRoomViewActions.key) {
            store.dispatch(action: action)
        }
    }
        
    func select<Value>(_ selector: Selector<LiveRoomViewState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return store.select(selector)
    }
    
    func selectCurrent<Value>(_ selector: Selector<LiveRoomViewState, Value>) -> Value {
        return store.selectCurrent(selector)
    }
}
