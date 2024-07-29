//
//  MusicPanelStore.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//
import Combine

class MusicPanelStoreFactory {
    static var storeMap: [String : MusicPanelStoreProvider] = [:]
    
    static func getStore(roomId: String) -> MusicPanelStoreProvider? {
        if let musicPanelStore = storeMap[roomId] {
            return musicPanelStore
        }
        return nil
    }
    
    static func addStore(roomId: String, musicPanelStore: MusicPanelStoreProvider) {
        storeMap.updateValue(musicPanelStore, forKey: roomId)
    }
    
    static func removeStore(roomId: String) {
        storeMap.removeValue(forKey: roomId)
    }
    
    static func removeAllStore() {
        storeMap.removeAll()
    }
}

protocol MusicPanelMenuDataGenerator {
    var musicPanelMenus: [MusicInfoCellItem] { get }
}

protocol MusicPanelStore {
    func dispatch(action: Action)
    
    func select<Value: Equatable>(_ selector: Selector<MusicPanelState, Value>) -> AnyPublisher<Value, Never>
    
    func selectCurrent<Value>(_ selector: Selector<MusicPanelState, Value>) -> Value
}
