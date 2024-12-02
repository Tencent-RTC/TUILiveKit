//
//  BeautyService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/5/28.
//

import RTCRoomEngine
import Combine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class BeautyService {
 
    var roomEngine: TUIRoomEngine
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func getBeautyManager() -> TXBeautyManager {
        roomEngine.getTRTCCloud().getBeautyManager()
    }
    
    func setBeautyLevel(_ beautyLevel: Float) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","setBeautyLevel:[level: \(beautyLevel)]")
            getBeautyManager().setBeautyLevel(beautyLevel)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func setWhitenessLevel(_ whitenessLevel: Float) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","setWhitenessLevel:[level: \(whitenessLevel)]")
            getBeautyManager().setWhitenessLevel(whitenessLevel)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func setRuddyLevel(_ ruddyLevel: Float) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","setRuddyLevel:[level: \(ruddyLevel)]")
            getBeautyManager().setRuddyLevel(ruddyLevel)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func setLocalVideoView(_ view: UIView) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            roomEngine.setLocalVideoView(view: view)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func openLocalCamera() -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            roomEngine.openLocalCamera(isFront: true, quality: .quality1080P) {
            } onError: { code, message in
                debugPrint("openLocalCamera error: \(code) \(message)")
            }
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
}
