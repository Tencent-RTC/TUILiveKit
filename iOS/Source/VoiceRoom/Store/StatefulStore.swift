//
//  StatelessStore.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/1.
//

import Combine

protocol ActionDispatcher {
    func dispatch(action: Action)
}

protocol VoiceRoomStoreProvider: ActionDispatcher {
    var toastSubject: PassthroughSubject<ToastInfo, Never> { get }
    var customEventSubject: PassthroughSubject<Action, Never> { get }
    var errorSubject: PassthroughSubject<ErrorService.OperateError, Never> {
        get
    }
    
    func select<Value: Equatable>(_ selector: Selector<OperationState, Value>) -> AnyPublisher<Value, Never>
    
    func selectCurrent<Value>(_ selector: Selector<OperationState, Value>) -> Value
    
    func select<Value: Equatable>(_ selector: Selector<NavigationState, Value>) -> AnyPublisher<Value, Never>
    
    func selectCurrent<Value>(_ selector: Selector<NavigationState, Value>) -> Value
    
    func select<Value: Equatable>(_ selector: Selector<GlobalViewState, Value>) -> AnyPublisher<Value, Never>
    
    func selectCurrent<Value>(_ selector: Selector<GlobalViewState, Value>) -> Value
}
