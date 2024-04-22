//
//  PermissionManager.swift
//  TUIKitCommon
//
//  Created by WesleyLei on 2023/9/27.
//  Copyright © 2023 Tencent. All rights reserved.
//

import AVFoundation
import Foundation

public class PermissionManager {
    private static let shared = PermissionManager()

    public class func cameraRequestAccess(completion: @escaping (Bool) -> Void) {
        if AVCaptureDevice.authorizationStatus(for: .video) == .notDetermined {
            AVCaptureDevice.requestAccess(for: .video) { granted in
                completion(granted)
            }
        } else {
            completion(AVCaptureDevice.authorizationStatus(for: .video) == .authorized)
        }
    }

    public class func microphoneRequestAccess(completion: @escaping (Bool) -> Void) {
        if AVCaptureDevice.authorizationStatus(for: .audio) == .notDetermined {
            AVCaptureDevice.requestAccess(for: .audio) { granted in
                completion(granted)
            }
        } else {
            completion(AVCaptureDevice.authorizationStatus(for: .audio) == .authorized)
        }
    }

    public class func gotoPermissionSettings() {
        guard let openSettingsURL = URL(string: UIApplication.openSettingsURLString) else { return }
        if UIApplication.shared.canOpenURL(openSettingsURL) {
            UIApplication.shared.open(openSettingsURL)
        }
    }
}
