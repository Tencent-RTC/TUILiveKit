//
//  appUtil.swift
//  trtcScenesDemo
//
//  Created by xcoderliu on 12/24/19.
//  Copyright © 2022 Tencent. All rights reserved.
//

import UIKit

@objcMembers
public class ApplicationUtils: NSObject {
    
    public static let shared = ApplicationUtils()
    private override init() {}
    
    public var appDelegate: NSObject? {
        return UIApplication.shared.delegate as? NSObject
    }
}

// MARK: - AppInfo
extension ApplicationUtils {
    
    public static var applicationBundle: Bundle {
        return Bundle.main
    }
    
    public static var displayName: String {
        guard let name = applicationBundle.object(forInfoDictionaryKey: "CFBundleDisplayName") as? String else {
            return ""
        }
        return name
    }
    
    public static var appVersionWithBuild: String {
        return "\(appVersion).\(buildNumber)"
    }
    
    public static var appVersion: String {
        guard let version = applicationBundle.object(forInfoDictionaryKey: "CFBundleShortVersionString") as? String else {
            return ""
        }
        return version
    }
    
    public static var buildNumber: String {
        guard let number = applicationBundle.object(forInfoDictionaryKey: "CFBundleVersion") as? String else {
            return ""
        }
        return number
    }
    
    public static var majorAppVersion: String {
        return appVersion.components(separatedBy: ".").first ?? ""
    }
}

