//
//  InternalBundle.swift
//  TUIAudienceList
//
//  Created by gg on 2025/4/10.
//

import Foundation

let internalBundle: Bundle = Bundle.moduleBundle(for: ErrorLocalized.self, bundleName: "TUILiveComponentBundle", moduleName: "TUILiveComponent") ?? .main

let avatarPlaceholderImage: UIImage? = UIImage(named: "live_seat_placeholder_avatar", in: internalBundle, with: nil)

func internalLocalized(_ key: String) -> String {
    return .localized(key, inBundle: internalBundle, table: "TUILiveComponentLocalized")
}

func internalImage(_ named: String) -> UIImage? {
    return UIImage(named: named, in: internalBundle, with: nil) ?? UIImage(named: named)
}
