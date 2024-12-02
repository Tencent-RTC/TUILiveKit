//
//  VRRoomEngineService.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//
import RTCRoomEngine
import ImSDK_Plus
import Combine

protocol BaseServiceProtocol {
    var roomEngine: TUIRoomEngine { get set }
    init(roomEngine: TUIRoomEngine)
}

class VRRoomEngineService {
    let roomService: VRRoomService
    let userService: VRUserService
    let seatService: VRSeatService
    
    let roomEngine: TUIRoomEngine
    let imManager: V2TIMManager
    
    init() {
        self.roomEngine = TUIRoomEngine.sharedInstance()
        self.imManager = V2TIMManager.sharedInstance()
        roomService = VRRoomService(roomEngine: roomEngine)
        userService = VRUserService(roomEngine: roomEngine)
        seatService = VRSeatService(roomEngine: roomEngine)
    }
    
    func addEngineObserver(_ engineObserver: TUIRoomObserver) {
        roomEngine.addObserver(engineObserver)
    }
    
    func removeEngineObserver(_ engineObserver: TUIRoomObserver) {
        roomEngine.removeObserver(engineObserver)
    }
    
    func addLiveListObserver(_ observer: TUILiveListManagerObserver) {
        guard let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else { return }
        liveListManager.addObserver(observer)
    }
    
    func removeLiveListObserver(_ observer: TUILiveListManagerObserver) {
        guard let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else { return }
        liveListManager.removeObserver(observer)
    }
    
    func addIMFriendshipObserver(_ observer: V2TIMFriendshipListener) {
        // IM observers do not needed to be removed
        imManager.addFriendListener(listener: observer)
    }
}
