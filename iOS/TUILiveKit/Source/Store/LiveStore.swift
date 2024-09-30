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

protocol LiveStore: ActionDispatcher {
    
    var toastSubject: PassthroughSubject<ToastInfo, Never> { get }
    var roomActionSubject: PassthroughSubject<IdentifiableAction, Never> { get }
    var seatActionSubject: PassthroughSubject<IdentifiableAction, Never> { get }
    var userActionSubject: PassthroughSubject<IdentifiableAction, Never> { get }
    
    var errorSubject: PassthroughSubject<ErrorService.OperateError, Never> { get }
    
    func select<Value: Equatable>(_ selector: Selector<OperationState, Value>) -> AnyPublisher<Value, Never>
    
    func selectCurrent<Value>(_ selector: Selector<OperationState, Value>) -> Value
    
    func select<Value: Equatable>(_ selector: Selector<ViewState, Value>) -> AnyPublisher<Value, Never>
    
    func selectCurrent<Value>(_ selector: Selector<ViewState, Value>) -> Value
}


extension LiveStore {
    
    var mediaState: MediaState {
        selectCurrent(MediaSelectors.getMediaState)
    }
    
    var roomState: RoomState {
        selectCurrent(RoomSelectors.getRoomState)
    }
    
    var seatState: SeatState {
        selectCurrent(SeatSelectors.getSeatState)
    }
    
    var userState: UserState {
        selectCurrent(UserSelectors.getUserState)
    }
    
    var beautyState: BeautyState {
        selectCurrent(BeautySelectors.getBeautyState)
    }
    
    var connectionState: ConnectionState {
        selectCurrent(ConnectionSelectors.getConnectionState)
    }
    
    var battleState: BattleState {
        selectCurrent(BattleSelectors.getBattleState)
    }
    
    var viewState: ViewState {
        selectCurrent(ViewSelectors.getViewState)
    }
}

