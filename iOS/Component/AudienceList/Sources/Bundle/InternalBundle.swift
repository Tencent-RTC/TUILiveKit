//
//  InternalBundle.swift
//  TUIAudienceList
//
//  Created by gg on 2025/4/10.
//

import Foundation
import TUILiveResources

let internalBundle: Bundle = Bundle.moduleBundle(for: AudienceListView.self, bundleName: "TUIAudienceListBundle", moduleName: "TUIAudienceList") ?? .main

let avatarPlaceholderImage: UIImage? = UIImage(named: "live_seat_placeholder_avatar", in: internalBundle, with: nil)

func internalLocalized(_ key: String) -> String {
    return .localized(key, inBundle: internalBundle, table: "TUIAudienceListLocalized")
}

func internalImage(_ named: String) -> UIImage? {
    return UIImage(named: named, in: internalBundle, with: nil) ?? UIImage(named: named)
}
