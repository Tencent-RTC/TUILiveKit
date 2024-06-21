//
//  Store.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//
import RTCRoomEngine
import Combine


class LiveStoreProvider {
    let toastSubject = PassthroughSubject<ToastInfo, Never>()
    let roomActionSubject = PassthroughSubject<any IdentifiableAction, Never>()
    let userActionSubject = PassthroughSubject<any IdentifiableAction, Never>()
    let seatActionSubject = PassthroughSubject<any IdentifiableAction, Never>()
    let errorSubject = PassthroughSubject<ErrorService.OperateError, Never>()
    
    private(set) lazy var operation: Store<OperationState, ServiceCenter> = {
        return Store(initialState: OperationState(), environment: ServiceCenter())
    }()

    private(set) lazy var viewStore: Store<ViewState, Void> = Store(initialState: ViewState())
    private var cancellableSet: Set<AnyCancellable> = []
    
    init() {
        initializeStore()
        debugPrint("\(type(of: self)) is init.")
    }
    
    deinit {
        operation.unregister(reducer: operationReducer)
        operation.unregister(reducer: userReducer)
        operation.unregister(reducer: roomReducer)
        operation.unregister(reducer: seatReducer)
        operation.unregister(reducer: mediaReducer)
        operation.unregister(reducer: beautyReducer)
        
        operation.unregisterEffects(withId: UserEffects.id)
        operation.unregisterEffects(withId: RoomEffects.id)
        operation.unregisterEffects(withId: SeatEffects.id)
        operation.unregisterEffects(withId: MediaEffects.id)
        operation.unregisterEffects(withId: ErrorEffects.id)
        operation.unregisterEffects(withId: BeautyEffects.id)

        debugPrint("deinit \(type(of: self))")
    }
    
    private func initializeStore() {
        initializeOperationStore()
        initializeRoomStore()
        initializeSeatStore()
        initializeUserStore()
        initializeMediaStore()
        initializeErrorEffect()
        initializedBeautyStore()
        initializedViewStore()
#if DEBUG
        operation.register(interceptor: PrintInterceptor<OperationState>())
#endif
    }
    
    private func initializeOperationStore() {
        operation.register(reducer: operationReducer)
    }
    
    private func initializeUserStore() {
        operation.register(reducer: userReducer, for: \.userState)
        operation.register(effects: UserEffects())
    }
    
    private func initializeRoomStore() {
        operation.register(reducer: roomReducer, for: \.roomState)
        operation.register(effects: RoomEffects())
    }
    
    private func initializeSeatStore() {
        operation.register(reducer: seatReducer, for: \.seatState)
        operation.register(effects: SeatEffects())
    }
    
    private func initializeMediaStore() {
        operation.register(reducer: mediaReducer, for: \.mediaState)
        operation.register(effects: MediaEffects())
    }
    
    private func initializedViewStore() {
        viewStore.register(reducer: viewReducer)
    }
    
    private func initializeErrorEffect() {
        operation.register(effects: ErrorEffects())
        // All the business layer errors are uniformly thrown upward through the Subject defined here. 
        // If special handling is required at the UI layer, just subscribe to this Subject.
        errorSubject
            .sink { [weak self] error in
                guard let self = self else { return }
                self.handle(error: error)
            }
            .store(in: &cancellableSet)
    }
    
    private func initializedBeautyStore() {
        operation.register(reducer: beautyReducer, for: \.beautyState)
        operation.register(effects: BeautyEffects())
    }
}

extension LiveStoreProvider: LiveStore {
    
    func dispatch(action: Action) {
        guard let action = action as? IdentifiableAction else { return }
        if action.id.hasPrefix(SeatResponseActions.key) {
            seatActionSubject.send(action)
        }
        if action.id.hasPrefix(RoomResponseActions.key) {
            roomActionSubject.send(action)
        }
        if action.id.hasPrefix(UserResponseActions.key) {
            userActionSubject.send(action)
        }
                
        if action.id.hasPrefix(ViewActions.key) {
            viewStore.dispatch(action: action)
        } else if action.id.hasPrefix(ViewActions.toastEventKey) {
            handleToast(action: action)
        } else {
            operation.dispatch(action: action)
        }
    }
    
    func select<Value: Equatable>(_ selector: Selector<OperationState, Value>) -> AnyPublisher<Value, Never> {
        return operation.select(selector)
            .removeDuplicates()
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<OperationState, Value>) -> Value {
        return operation.selectCurrent(selector)
    }
    
    func select<Value: Equatable>(_ selector: Selector<ViewState, Value>) -> AnyPublisher<Value, Never> {
        return viewStore.select(selector)
            .removeDuplicates()
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<ViewState, Value>) -> Value {
        return viewStore.selectCurrent(selector)
    }
}

extension LiveStoreProvider {
    private func handle(error: ErrorService.OperateError) {
        let toast = ToastInfo(message: error.localizedMessage, position: .bottom, duration: 2.0)
        toastSubject.send(toast)
        error.actions.forEach { action in
            guard let action = action as? IdentifiableAction else { return }
            if !action.id.contains(ErrorActions.key) {
                dispatch(action: action)
            }
        }
    }
    
    private func handleToast(action: Action) {
        if let viewAction = action as? AnonymousAction<ToastInfo> {
            toastSubject.send(viewAction.payload)
        }
    }
}
