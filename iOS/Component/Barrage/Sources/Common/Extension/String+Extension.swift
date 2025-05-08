//
//  String+Extension.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/11.
//

import TUICore

extension String {
    static func localized(_ key: String) -> String {
        if let bundlePath = Bundle.barrageBundle.path(forResource: TUIGlobalization.getPreferredLanguage() ?? "", ofType: "lproj"),
           let bundle = Bundle(path: bundlePath) {
            return bundle.localizedString(forKey: key, value: "", table: "TUIBarrageLocalized")
        }
        return Bundle.barrageBundle.localizedString(forKey: key, value: "", table: "TUIBarrageLocalized")
    }

    static func localizedReplace(_ origin: String, replace: String) -> String {
        return origin.replacingOccurrences(of: "xxx", with: replace)
    }
    
    static func localizedReplaceTwoCharacter(origin: String, firstReplace: String, secondReplace: String) -> String {
        return localizedReplace(origin, replace: firstReplace).replacingOccurrences(of: "yyy", with: secondReplace)
    }
}

