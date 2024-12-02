//
//  Bundle+Extension.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/18.
//

import Foundation

extension Bundle {
    static var sg_seatGridViewBundle: Bundle {
        if let bundle = getSeatGridViewBundle() {
            return bundle
        } else {
            return Bundle()
        }
    }
    
    private static func getSeatGridViewBundle() -> Bundle? {
        var url: NSURL? = Bundle.main.url(forResource: "SeatGridViewBundle", withExtension: "bundle") as NSURL?
        if let associateBundleURL = url {
            return Bundle(url: associateBundleURL as URL)
        }
        url = Bundle.main.url(forResource: "Frameworks", withExtension: nil) as NSURL?
        url = url?.appendingPathComponent("SeatGridView") as NSURL?
        url = url?.appendingPathComponent("framework") as NSURL?
        if let associateBundleURL = url {
            let bundle = Bundle(url: associateBundleURL as URL)
            url = bundle?.url(forResource: "SeatGridViewBundle", withExtension: "bundle") as NSURL?
            if let associateBundleURL = url {
                return Bundle(url: associateBundleURL as URL)
            }
        }
        return nil
    }
}


