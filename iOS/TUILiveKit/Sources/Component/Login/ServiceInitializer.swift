//
//  ServiceInitializer.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/19.
//

import Foundation
import TUICore
import RTCRoomEngine

extension NSObject {
    @objc class func liveKitExtensionLoad() {
        ServiceInitializer.shared.registerObserver()
        TestTool.shared.initialize()
    }
}

class ServiceInitializer {
    
    private var isLoginEngine: Bool = false
    static let shared = ServiceInitializer()

    private init() {
    }

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
            ServiceInitializer.login(sdkAppId: sdkAppId, userId: userId, userSig: userSig) { [weak self] in
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
            ServiceInitializer.logout {
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
    }
}


extension ServiceInitializer {
    static func login(sdkAppId:Int,userId:String,userSig:String,onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LiveKitLog.info("\(#file)","\(#line)","login:[sdkAppId:\(sdkAppId),userId:\(userId)]")
        TUIRoomEngine.login(sdkAppId: sdkAppId, userId: userId, userSig: userSig) {
            LiveKitLog.info("\(#file)","\(#line)","login:[onSuccess]")
            onSuccess()
        } onError: { code, message in
            LiveKitLog.error("\(#file)","\(#line)","login:[onError:[code:\(code),message:\(message)]]")
            onError(code, message)
        }
    }
    
    static func logout(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LiveKitLog.info("\(#file)","\(#line)","logout")
        TUIRoomEngine.logout  {
            LiveKitLog.info("\(#file)","\(#line)","logout:[onSuccess]")
            onSuccess()
        } onError: { code, message in
            LiveKitLog.error("\(#file)","\(#line)","logout:[onError:[code:\(code),message:\(message)]]")
            onError(code, message)
        }
    }
}
