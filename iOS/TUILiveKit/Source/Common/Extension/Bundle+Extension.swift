//
//  Bundle+Extension.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation

extension Bundle {
    static var liveBundle: Bundle {
        return SharedBundle.sharedBundle
    }
}

class SharedBundle {
    static var sharedBundle: Bundle {
        if let bundle = getLiveBundle() {
            return bundle
        } else {
            return Bundle()
        }
    }
    
    private static func getLiveBundle() -> Bundle? {
        var url: NSURL? = Bundle.main.url(forResource: "TUILiveKitBundle", withExtension: "bundle") as NSURL?
        if let associateBundleURL = url {
            return Bundle(url: associateBundleURL as URL)
        }
        url = Bundle.main.url(forResource: "Frameworks", withExtension: nil) as NSURL?
        url = url?.appendingPathComponent("TUILiveKit") as NSURL?
        url = url?.appendingPathComponent("framework") as NSURL?
        if let associateBundleURL = url {
            let bundle = Bundle(url: associateBundleURL as URL)
            url = bundle?.url(forResource: "TUILiveKitBundle", withExtension: "bundle") as NSURL?
            if let associateBundleURL = url {
                return Bundle(url: associateBundleURL as URL)
            }
        }
        return nil
    }
}
