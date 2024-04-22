//
//  WindowUtils.swift
//  TUIKitCommon
//
//  Created by 于西巍 on 2023/10/16.
//

import Foundation

public class WindowUtils {
    public static func getCurrentWindow() -> UIWindow? {
        if #available(iOS 13.0, *) {
            if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene {
                if let keyWindow = windowScene.windows.first {
                    return keyWindow
                }
            }
        }
        return UIApplication.shared.windows.first
    }

    public static func getCurrentWindowViewController() -> UIViewController? {
        var keyWindow: UIWindow?
        for window in UIApplication.shared.windows {
            if window.isMember(of: UIWindow.self), window.isKeyWindow {
                keyWindow = window
                break
            }
        }
        guard let rootController = keyWindow?.rootViewController else {
            return nil
        }
        func findCurrentController(from vc: UIViewController?) -> UIViewController? {
            if let nav = vc as? UINavigationController {
                return findCurrentController(from: nav.topViewController)
            } else if let tabBar = vc as? UITabBarController {
                return findCurrentController(from: tabBar.selectedViewController)
            } else if let presented = vc?.presentedViewController {
                return findCurrentController(from: presented)
            }
            return vc
        }
        let viewController = findCurrentController(from: rootController)
        return viewController
    }
    
    public static var bottomSafeHeight: CGFloat {
        getCurrentWindow()?.safeAreaInsets.bottom ?? 0
    }
    
    public static var topSafeHeight: CGFloat {
        getCurrentWindow()?.safeAreaInsets.top ?? 0
    }
    
    public static var isPortrait: Bool {
        guard let isPortrait = UIApplication.shared.windows.first?.windowScene?.interfaceOrientation.isPortrait as? Bool
        else { return UIDevice.current.orientation.isPortrait }
        return isPortrait
    }
}
