//
//  LiveStreamInitializer.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/30.
//

import TUICore
import RTCRoomEngine

extension NSObject {
    @objc static func liveStreamExtensionLoad() {
        LiveStreamInitializer.shared.registerObserver()
    }
}

class LiveStreamInitializer {
    static let shared = LiveStreamInitializer()
    private init() {}
    
    private var isLoginEngine: Bool = false
    
    func registerObserver() {
        NotificationCenter.default.addObserver(self, selector: #selector(logoutSuccess(_:)),
                                               name: NSNotification.Name(rawValue: NSNotification.Name.TUILogoutSuccess.rawValue),
                                               object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(loginSuccess(_:)),
                                               name: NSNotification.Name(rawValue: NSNotification.Name.TUILoginSuccess.rawValue),
                                               object: nil)
    }

    @objc private func logoutSuccess(_ notification: Notification) {
        logout(onSuccess: nil, onError: nil)
    }

    @objc private func loginSuccess(_ notification: Notification) {
        login(onSuccess: nil, onError: nil)
    }

    func login(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        if isLoginEngine {
            onSuccess?()
        } else {
            let sdkAppId = Int(TUILogin.getSdkAppID())
            let userId = TUILogin.getUserID() ?? ""
            let userSig = TUILogin.getUserSig() ?? ""
            LiveStreamInitializer.login(sdkAppId: sdkAppId, userId: userId, userSig: userSig) { [weak self] in
                guard let self = self else { return }
                self.isLoginEngine = true
                onSuccess?()
            } onError: { code, message in
                onError?(code, message)
            }
        }
    }

    func logout(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        if isLoginEngine {
            LiveStreamInitializer.logout {
                onSuccess?()
            } onError: { code, message in
                onError?(code, message)
            }
            self.isLoginEngine = false
        } else {
            onSuccess?()
        }
    }
        
    deinit {
        NotificationCenter.default.removeObserver(self)
        debugPrint("deinit:\(self)")
    }
}

extension LiveStreamInitializer {
    static func login(sdkAppId:Int,userId:String,userSig:String,onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LiveStreamLog.info("\(#file)","\(#line)","login:[sdkAppId:\(sdkAppId),userId:\(userId),userSig:\(userSig)]")
        TUIRoomEngine.login(sdkAppId: sdkAppId, userId: userId, userSig: userSig) {
            LiveStreamLog.info("\(#file)","\(#line)","login:[onSuccess]")
            onSuccess()
        } onError: { code, message in
            LiveStreamLog.error("\(#file)","\(#line)","login:[onError:[code:\(code),message:\(message)]]")
            onError(code, message)
        }
    }
    
    static func logout(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LiveStreamLog.info("\(#file)","\(#line)","logout")
        TUIRoomEngine.logout  {
            LiveStreamLog.info("\(#file)","\(#line)","logout:[onSuccess]")
            onSuccess()
        } onError: { code, message in
            LiveStreamLog.error("\(#file)","\(#line)","logout:[onError:[code:\(code),message:\(message)]]")
            onError(code, message)
        }
    }
}
