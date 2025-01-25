//
//  String+Extension.swift
//  TEBeautyKit
//
//  Created by jack on 2024/12/31.
//

import Foundation
import TUICore

extension String {
    
    static func localize(_ key: String) -> String {
        guard let path = Bundle.beautyKitBundle.path(forResource: TUIGlobalization.getPreferredLanguage(), ofType: "lproj") else {
            return ""
        }
        guard let bundle = Bundle(path: path) else {
            return ""
        }
        return bundle.localizedString(forKey: key, value: "", table: "BeautyKitLocalized")
    }
}
