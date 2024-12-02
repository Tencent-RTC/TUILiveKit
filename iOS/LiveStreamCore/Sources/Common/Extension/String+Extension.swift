//
//  String+Extension.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/23.
//

import Foundation
import TUICore

extension String: LiveStreamWrapper {}

extension LiveStreamExtension where Base == String {
    public var localized: String {
        if let bundlePath = Bundle.liveStreamCoreBundle.path(forResource: TUIGlobalization.getPreferredLanguage() ?? "", ofType: "lproj"),
           let bundle = Bundle(path: bundlePath) {
            return bundle.localizedString(forKey: base, value: "", table: "LiveStreamCoreLocalized")
        }
        return Bundle.liveStreamCoreBundle.localizedString(forKey: base, value: "", table: "LiveStreamCoreLocalized")
    }
    
    func localizedReplace(replace: String) -> String {
        return base.replacingOccurrences(of: "xxx", with: replace)
    }
}
