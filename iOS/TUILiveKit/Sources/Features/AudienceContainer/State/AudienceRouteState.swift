//
//  AudienceRouteState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/20.
//

import Foundation
import LiveStreamCore
import RTCRoomEngine
import RTCCommon

struct AudienceRouterState {
    var routeStack: [AudienceRoute] = []
    var dismissEvent: (() -> Void)?
}

enum AudienceDismissType {
    case panel
    case alert
}

enum AudienceRouterAction {
    case routeTo(_ route: AudienceRoute)
    case present(_ route: AudienceRoute)
    case dismiss(_ type: AudienceDismissType = .panel, completion: (() -> Void)? = nil)
    case exit
}

enum AudienceRoute {
    case audience
    case linkType(_ data: [LinkMicTypeCellData])
    case linkSetting
    case listMenu(_ data: ActionPanelData)
    case audioEffect
    case beauty
    case giftView
    case alert(info: AudienceAlertInfo)
    case streamDashboard
    case userManagement(_ user: TUIUserInfo, type: AudienceUserManagePanelType)
    case netWorkInfo(_ manager: NetWorkInfoManager, isAudience: Bool)
}

extension AudienceRoute: Equatable {
    static func == (lhs: AudienceRoute, rhs: AudienceRoute) -> Bool {
        switch (lhs, rhs) {
            case (.audience,.audience),
                (.linkType, .linkType),
                (.linkSetting, .linkSetting),
                (.audioEffect,.audioEffect),
                (.beauty, .beauty),
                (.giftView, .giftView),
                (.alert, .alert),
                (.streamDashboard, .streamDashboard):
                return true
            case let (.listMenu(l), .listMenu(r)):
                return l == r
            case let (.userManagement(l1, l2), .userManagement(r1, r2)):
                return l1 == r1 && l2 == r2
            case let (.netWorkInfo(l1, l2), .netWorkInfo(r1, r2)):
                return l1 == r1 && l2 == r2
            case (.audience, _),
                (.linkType, _),
                (.linkSetting, _),
                (.listMenu, _),
                (.audioEffect, _),
                (.beauty, _),
                (.giftView, _),
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

extension AudienceRoute: Hashable {
    func convertToString() -> String {
        switch self {
            case .audience:
                return "audience"
            case .linkType:
                return "linkType"
            case .linkSetting:
                return "linkSetting"
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
