//
//  AnchorRouteState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/20.
//

import Foundation
import AtomicXCore
import RTCRoomEngine
import RTCCommon

struct AnchorRouterState {
    var routeStack: [AnchorRoute] = []
    var dismissEvent: (() -> Void)?
}

enum AnchorDismissType {
    case panel
    case alert
}

enum AnchorRouterAction {
    case routeTo(_ route: AnchorRoute)
    case present(_ route: AnchorRoute)
    case dismiss(_ type: AnchorDismissType = .panel, completion: (() -> Void)? = nil)
    case exit
}

enum AnchorRoute {
    case anchor
    case liveLinkControl
    case connectionControl
    case featureSetting(_ settingModel: AnchorFeatureClickPanelModel)
    case listMenu(_ data: ActionPanelData)
    case audioEffect
    case beauty
    case giftView
    case battleCountdown(_ countdownTime: TimeInterval)
    case alert(info: AnchorAlertInfo)
    case streamDashboard
    case userManagement(_ user: TUIUserInfo, type: AnchorUserManagePanelType)
    case netWorkInfo(_ manager: NetWorkInfoManager, isAudience: Bool)
}

extension AnchorRoute: Equatable {
    static func == (lhs: AnchorRoute, rhs: AnchorRoute) -> Bool {
        switch (lhs, rhs) {
            case (.anchor,.anchor),
                (.liveLinkControl,.liveLinkControl),
                (.connectionControl,.connectionControl),
                (.audioEffect,.audioEffect),
                (.beauty, .beauty),
                (.giftView, .giftView),
                (.alert, .alert),
                (.streamDashboard, .streamDashboard):
                return true
            case let (.featureSetting(l), .featureSetting(r)):
                return l == r
            case let (.listMenu(l), .listMenu(r)):
                return l == r
            case let (.battleCountdown(l), .battleCountdown(r)):
                return l == r
            case let (.userManagement(l1, l2), .userManagement(r1, r2)):
                return l1 == r1 && l2 == r2
            case let (.netWorkInfo(l1, l2), .netWorkInfo(r1, r2)):
                return l1 == r1 && l2 == r2
            case (.anchor, _),
                (.liveLinkControl, _),
                (.connectionControl, _),
                (.featureSetting, _),
                (.listMenu, _),
                (.audioEffect, _),
                (.beauty, _),
                (.giftView, _),
                (.battleCountdown, _),
                (.alert, _),
                (.streamDashboard, _),
                (.userManagement, _),
                (.netWorkInfo, _):
                return false
            default:
                break
        }
    }
}

extension AnchorRoute: Hashable {
    func convertToString() -> String {
        switch self {
            case .anchor:
                return "anchor"
            case .liveLinkControl:
                return "liveLinkControl"
            case .connectionControl:
                return "connectionControl"
            case .featureSetting(let settingModel):
                return "featureSetting" + settingModel.id.uuidString
            case .listMenu(let data):
                var result = "listMenu"
                data.items.forEach { item in
                    result += item.id.uuidString
                }
                return result
            case .audioEffect:
                return "audioEffect"
            case .beauty:
                return "beauty"
            case .giftView:
                return "giftView"
            case .battleCountdown(let countdownTime):
                return "battleCountdown \(countdownTime)"
            case .alert(let alertInfo):
                return "alert \(alertInfo.description)"
            case .streamDashboard:
                return "streamDashboard"
            case .userManagement(let user, let type):
                return "userManagement \(user.userId) type: \(type)"
            case .netWorkInfo:
                return "netWorkInfo"
        }
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(self.convertToString())
    }
}
