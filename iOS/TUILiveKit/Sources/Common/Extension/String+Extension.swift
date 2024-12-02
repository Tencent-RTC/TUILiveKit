//
//  String+Extension.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/11.
//

import TUICore

extension String {
    static func localized(_ key: String) -> String {
        if let bundlePath = Bundle.liveBundle.path(forResource: TUIGlobalization.getPreferredLanguage() ?? "", ofType: "lproj"),
           let bundle = Bundle(path: bundlePath) {
            return bundle.localizedString(forKey: key, value: "", table: "TUILiveKitLocalized")
        }
        return SharedBundle.sharedBundle.localizedString(forKey: key, value: "", table: "TUILiveKitLocalized")
    }

    static func localizedReplace(_ origin: String, replace: String) -> String {
        return origin.replacingOccurrences(of: "xxx", with: replace)
    }
}

