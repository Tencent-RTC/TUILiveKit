//
//  MusicPanelStoreImpl.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//
import Combine


protocol MusicPanelStoreProvider {
    func dispatch(action: Action)
    
    func select<Value: Equatable>(_ selector: Selector<MusicPanelState, Value>) -> AnyPublisher<Value, Never>
    
    func selectCurrent<Value>(_ selector: Selector<MusicPanelState, Value>) -> Value
}

protocol MusicPanelMenuDataGenerator {
    var musicPanelMenus: [MusicInfoCellItem] { get }
}
