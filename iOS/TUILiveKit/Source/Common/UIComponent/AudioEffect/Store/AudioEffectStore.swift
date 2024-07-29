//
//  AudioEffectStoreProvider.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/3.
//
import Combine

class AudioEffectStoreFactory {
    static var storeMap: [String : AudioEffectStoreProvider] = [:]
    
    static func getStore(roomId: String) -> AudioEffectStoreProvider? {
        if let audioEffectStore = storeMap[roomId] {
            return audioEffectStore
        }
        return nil
    }
    
    static func addStore(roomId: String, audioEffectStore: AudioEffectStoreProvider) {
        storeMap.updateValue(audioEffectStore, forKey: roomId)
    }
    
    static func removeStore(roomId: String) {
        storeMap.removeValue(forKey: roomId)
    }
    
    static func removeAllStore() {
        storeMap.removeAll()
    }
}

protocol AudioEffectStore {
    func dispatch(action: Action)
    func select<Value: Equatable>(_ selector: Selector<AudioEffectState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<AudioEffectState, Value>) -> Value
}

protocol AudioEffectMenuDateGenerator {
    typealias Section = Int
    var audioEffectMenus: [Section: [SettingItem]] { get }
    var audioEffectSectionTitles: [Section: String] { get }
}
