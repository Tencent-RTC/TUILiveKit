//
//  InternalBundle.swift
//  TUIAudienceList
//
//  Created by gg on 2025/4/10.
//

import Foundation
import TUILiveResources

let internalBundle: Bundle = Bundle.moduleBundle(for: GiftPlayView.self, bundleName: "TUIGiftBundle", moduleName: "TUIGift") ?? .main

func internalLocalized(_ key: String) -> String {
    return .localized(key, inBundle: internalBundle, table: "TUIGiftLocalized")
}

func internalImage(_ named: String) -> UIImage? {
    return UIImage(named: named, in: internalBundle, with: nil) ?? UIImage(named: named)
}
