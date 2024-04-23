//
//  LocalizedLanguage.swift
//  TUIKitCommon
//
//  Created by krabyu on 2024/4/16.
//

import Foundation

public class LocalizedLanguage {
    public static func getCurrentLanguage() -> String {
        guard let languages: [String] = UserDefaults.standard.object(forKey: "AppleLanguages") as? [String] else { return "en-CN" }
        let currentLanguage = languages.first
        return currentLanguage ?? "en-CN"
    }
    
    public static var isChinese: Bool {
        let languageCode = LocalizedLanguage.getCurrentLanguage()
        return languageCode.hasPrefix("zh")
    }
}
