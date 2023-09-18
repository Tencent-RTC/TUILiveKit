//
//  appUtil.swift
//  trtcScenesDemo
//
//  Created by xcoderliu on 12/24/19.
//  Copyright © 2022 Tencent. All rights reserved.
//
// 用于TRTC_SceneDemo

import UIKit

@objcMembers
public class AppUtils: NSObject {
    
    public static let shared = AppUtils()
    private override init() {}
    
    public var appDelegate: NSObject? {
        return UIApplication.shared.delegate as? NSObject
    }
    
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
}

// MARK: - AppInfo
extension AppUtils {
    
    public static var applicationBundle: Bundle {
        return Bundle.main
    }
    
    /// app名称
    public static var displayName: String {
        guard let name = applicationBundle.object(forInfoDictionaryKey: "CFBundleDisplayName") as? String else {
            return ""
        }
        return name
    }
    /// App版本，带构建版本
    public static var appVersionWithBuild: String {
        return "\(appVersion).\(buildNumber)"
    }
    /// App版本号 eg. 9.4.0
    public static var appVersion: String {
        guard let version = applicationBundle.object(forInfoDictionaryKey: "CFBundleShortVersionString") as? String else {
            return ""
        }
        return version
    }
    /// App构建版本号 eg. 10765
    public static var buildNumber: String {
        guard let number = applicationBundle.object(forInfoDictionaryKey: "CFBundleVersion") as? String else {
            return ""
        }
        return number
    }
    /// App主版本号 eg. 9
    public static var majorAppVersion: String {
        return appVersion.components(separatedBy: ".").first ?? ""
    }
}

