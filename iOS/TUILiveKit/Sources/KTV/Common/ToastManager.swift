//
//  ToastManager.swift
//  TUILiveKit
//
//  Created by CY zhao on 2025/8/4.
//

import Foundation
import TUICore

class ToastManager {
    static func showToast(_ message: String, duration: TimeInterval = 1, position: String = TUICSToastPositionCenter ) {
        if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
           let window = windowScene.windows.first {
            window.makeToast(message, duration: duration, position: position)
        }
    }
}
