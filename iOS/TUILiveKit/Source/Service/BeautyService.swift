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
    private let engine = TUIRoomEngine.sharedInstance()
    private var trtcCloud: TRTCCloud {
        return engine.getTRTCCloud()
    }
    
    func setBeautyLevel(_ beautyLevel: Float) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            self.trtcCloud.getBeautyManager().setBeautyLevel(beautyLevel)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func setWhitenessLevel(_ whitenessLevel: Float) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            self.trtcCloud.getBeautyManager().setWhitenessLevel(whitenessLevel)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func setRuddyLevel(_ ruddyLevel: Float) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            self.trtcCloud.getBeautyManager().setRuddyLevel(ruddyLevel)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
}
