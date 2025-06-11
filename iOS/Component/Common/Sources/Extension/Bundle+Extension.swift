//
//  Bundle+Extension.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation
import UIKit

public extension Bundle {
    static var liveBundle: Bundle {
        return SharedBundle.sharedBundle
    }
    
    static func moduleBundle(for aClass: AnyClass = ErrorLocalized.self, bundleName: String = "TUILiveComponentBundle", moduleName: String = "TUILiveComponent") -> Bundle? {
        if let url = Bundle(for: aClass).url(forResource: bundleName, withExtension: "bundle") {
            return Bundle(url: url)
        }
        if let url = Bundle.main.url(forResource: bundleName, withExtension: "bundle") {
            return Bundle(url: url)
        }
        var url = Bundle.main.url(forResource: "Frameworks", withExtension: nil)
        url = url?.appendingPathComponent(moduleName)
        url = url?.appendingPathComponent("framework")
        if let associateBundleURL = url {
            let bundle = Bundle(url: associateBundleURL)
            url = bundle?.url(forResource: bundleName, withExtension: "bundle")
            if let associateBundleURL = url {
                return Bundle(url: associateBundleURL)
            }
        }
        return nil
    }
}

class SharedBundle {
    static var sharedBundle: Bundle {
        if let bundle = getLiveBundle() {
            return bundle
        } else {
            return .main
        }
    }
    
    private static func getLiveBundle() -> Bundle? {
        var url: NSURL? = Bundle.main.url(forResource: "TUILiveComponentBundle", withExtension: "bundle") as NSURL?
        if let associateBundleURL = url {
            return Bundle(url: associateBundleURL as URL)
        }
        url = Bundle.main.url(forResource: "Frameworks", withExtension: nil) as NSURL?
        url = url?.appendingPathComponent("TUILiveComponent") as NSURL?
        url = url?.appendingPathComponent("framework") as NSURL?
        if let associateBundleURL = url {
            let bundle = Bundle(url: associateBundleURL as URL)
            url = bundle?.url(forResource: "TUILiveComponentBundle", withExtension: "bundle") as NSURL?
            if let associateBundleURL = url {
                return Bundle(url: associateBundleURL as URL)
            }
        }
        return nil
    }
}
