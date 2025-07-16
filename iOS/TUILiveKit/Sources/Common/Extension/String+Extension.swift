//
//  String+Extension.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/11.
//

import TUICore

public extension String {
    static func localized(_ key: String, inBundle: Bundle, table: String = "TUILiveComponentLocalized") -> String {
        if let bundlePath = inBundle.path(forResource: TUIGlobalization.getPreferredLanguage() ?? "", ofType: "lproj"),
           let bundle = Bundle(path: bundlePath) {
            return bundle.localizedString(forKey: key, value: "", table: table)
        }
        return inBundle.localizedString(forKey: key, value: "", table: table)
    }

    static func localizedReplace(_ origin: String, replace: String) -> String {
        return origin.replacingOccurrences(of: "xxx", with: replace)
    }
    
    static func localizedReplaceTwoCharacter(origin: String, firstReplace: String, secondReplace: String) -> String {
        return localizedReplace(origin, replace: firstReplace).replacingOccurrences(of: "yyy", with: secondReplace)
    }
}

