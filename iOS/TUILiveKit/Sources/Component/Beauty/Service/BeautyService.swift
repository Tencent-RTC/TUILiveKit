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
import TUILiveResources

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
    
    func setBeautyStyle(_ style: TXBeautyStyle) {
        LiveKitLog.info("\(#file)", "\(#line)", "setBeautyStyle:[style: \(style.rawValue)]")
        getBeautyManager().setBeautyStyle(style)
    }
    
    func setBeautyLevel(_ beautyLevel: Float) {
        LiveKitLog.info("\(#file)", "\(#line)","setBeautyLevel:[level: \(beautyLevel)]")
        getBeautyManager().setBeautyLevel(beautyLevel)

    }
    
    func setWhitenessLevel(_ whitenessLevel: Float) {
        LiveKitLog.info("\(#file)", "\(#line)","setWhitenessLevel:[level: \(whitenessLevel)]")
        getBeautyManager().setWhitenessLevel(whitenessLevel)
    }
    
    func setRuddyLevel(_ ruddyLevel: Float) {
        LiveKitLog.info("\(#file)", "\(#line)","setRuddyLevel:[level: \(ruddyLevel)]")
        getBeautyManager().setRuddyLevel(ruddyLevel)
    }
    
    func setLocalVideoView(_ view: UIView) {
        roomEngine.setLocalVideoView(view: view)
    }
    
    func openLocalCamera() async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            roomEngine.openLocalCamera(isFront: true, quality: .quality1080P) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
            }
        }
    }
    
    func closeBeauty() {
        setWhitenessLevel(0)
        setBeautyLevel(0)
        setRuddyLevel(0)
    }
    
    static func closeBeauty() {
        let service = BeautyService(roomEngine: TUIRoomEngine.sharedInstance())
        service.closeBeauty()
    }
}
