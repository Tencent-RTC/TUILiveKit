//
//  MusicPanelStore.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//
import Combine

protocol MusicPanelMenuDataGenerator {
    var musicPanelMenus: [MusicInfoCellItem] { get }
}

protocol MusicPanelStore {
    func dispatch(action: Action)
    
    func select<Value: Equatable>(_ selector: Selector<MusicPanelState, Value>) -> AnyPublisher<Value, Never>
    
    func selectCurrent<Value>(_ selector: Selector<MusicPanelState, Value>) -> Value
}
