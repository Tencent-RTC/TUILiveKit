//
//  Bundle+Extension.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/23.
//

import Foundation

extension Bundle {
    static var liveStreamCoreBundle: Bundle {
        if let bundle = getLiveStreamCoreBundle() {
            return bundle
        } else {
            return Bundle()
        }
    }
    
    private static func getLiveStreamCoreBundle() -> Bundle? {
        var url: NSURL? = Bundle.main.url(forResource: "LiveStreamCoreBundle", withExtension: "bundle") as NSURL?
        if let associateBundleURL = url {
            return Bundle(url: associateBundleURL as URL)
        }
        url = Bundle.main.url(forResource: "Frameworks", withExtension: nil) as NSURL?
        url = url?.appendingPathComponent("LiveStreamCore") as NSURL?
        url = url?.appendingPathComponent("framework") as NSURL?
        if let associateBundleURL = url {
            let bundle = Bundle(url: associateBundleURL as URL)
            url = bundle?.url(forResource: "LiveStreamCoreBundle", withExtension: "bundle") as NSURL?
            if let associateBundleURL = url {
                return Bundle(url: associateBundleURL as URL)
            }
        }
        return nil

    }
}
