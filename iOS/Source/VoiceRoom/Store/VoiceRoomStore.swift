//
//  Store.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//
import RTCRoomEngine
import Combine

class VoiceRoomStore {
    
    var environment = ServiceCenter()
    
    private(set) lazy var operation: Store<OperationState, ServiceCenter> = Store(initialState: OperationState(), environment: self.environment)
    
    private(set) lazy var navigator: Store<NavigationState, Void> = Store(initialState: NavigationState())
    
    private(set) lazy var viewStore: Store<GlobalViewState, Void> = Store(initialState: GlobalViewState())
    private var cancellabels: Set<AnyCancellable> = []
    
    let toastSubject = PassthroughSubject<ToastInfo, Never>()
    let customEventSubject = PassthroughSubject<Action, Never>()
    let errorSubject = PassthroughSubject<ErrorService.OperateError, Never>()
    
    init() {
        initializeStore()
        debugPrint("\(type(of: self)) is init.")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    private func initializeStore() {
        initializeRoomStore()
        initializeSeatStore()
        initializeUserStore()
        initializeMediaStore()
        initializeErrorEffect()
        initializeNavigationStore()
        initializeViewStore()
#if DEBUG
        operation.register(interceptor: PrintInterceptor<OperationState>())
#endif
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
    
    private func initializeErrorEffect() {
        operation.register(effects: ErrorEffects())
        errorSubject
            .sink { [weak self] error in
                guard let self = self else { return }
                self.handle(error: error)
            }
            .store(in: &cancellabels)
    }
    
    private func initializeNavigationStore() {
        navigator.register(reducer: navigationReducer)
#if DEBUG
//        media.register(interceptor: PrintInterceptor<NavigationState>())
#endif
    }
    
    private func initializeViewStore() {
        viewStore.register(reducer: rootViewReducer, for: \GlobalViewState.rootViewState)
        viewStore.register(reducer: menuReducer, for: \GlobalViewState.menu)
    }
}

extension VoiceRoomStore: VoiceRoomStoreProvider {
    
    func dispatch(action: Action) {
        guard let action = action as? IdentifiableAction else { return }
        if action.id.contains(NavigatorActions.key) {
            navigator.dispatch(action: action)
        } else if action.id.contains(ViewActions.key) {
            viewStore.dispatch(action: action)
        } else if action.id.contains(ViewActions.subjectKey) {
            if let toastAction = action as? AnonymousAction<ToastInfo> {
                toastSubject.send(toastAction.payload)
            }
        } else if action.id.contains(ViewActions.customEventKey) {
            customEventSubject.send(action)
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
    
    func select<Value: Equatable>(_ selector: Selector<NavigationState, Value>) -> AnyPublisher<Value, Never> {
        return navigator.select(selector)
            .removeDuplicates()
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<NavigationState, Value>) -> Value {
        return navigator.selectCurrent(selector)
    }
    
    func select<Value: Equatable>(_ selector: Selector<GlobalViewState, Value>) -> AnyPublisher<Value, Never> {
        return viewStore.select(selector)
            .removeDuplicates()
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<GlobalViewState, Value>) -> Value {
        return viewStore.selectCurrent(selector)
    }
}

extension VoiceRoomStore {
    private func handle(error: ErrorService.OperateError) {
        if error.actions.isEmpty {
            let toast = ToastInfo(message: error.localizedMessage, position: .bottom, duration: 2.0)
            toastSubject.send(toast)
        } else {
            error.actions.forEach { action in
                guard let action = action as? IdentifiableAction else { return }
                if !action.id.contains(ErrorActions.key) {
                    dispatch(action: action)
                }
            }
        }
    }
}
