//
//  Bundle+Extension.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation
import UIKit

extension Bundle {
    static var barrageBundle: Bundle {
        if let bundle = getBarrageBundle() {
            return bundle
        } else {
            return .main
        }
    }
    
    private static func getBarrageBundle() -> Bundle? {
        var url: NSURL? = Bundle.main.url(forResource: "TUIBarrageBundle", withExtension: "bundle") as NSURL?
        if let associateBundleURL = url {
            return Bundle(url: associateBundleURL as URL)
        }
        url = Bundle.main.url(forResource: "Frameworks", withExtension: nil) as NSURL?
        url = url?.appendingPathComponent("TUIBarrage") as NSURL?
        url = url?.appendingPathComponent("framework") as NSURL?
        if let associateBundleURL = url {
            let bundle = Bundle(url: associateBundleURL as URL)
            url = bundle?.url(forResource: "TUIBarrageBundle", withExtension: "bundle") as NSURL?
            if let associateBundleURL = url {
                return Bundle(url: associateBundleURL as URL)
            }
        }
        return nil
    }
}
