//
//  VRRouterState.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/18.
//

import Foundation
import LiveStreamCore

enum VRDismissType {
    case panel
    case alert
}

enum VRRouterAction {
    case routeTo(_ route: VRRoute)
    case present(_ route: VRRoute)
    case dismiss(_ type: VRDismissType = .panel, completion: (() -> Void)? = nil)
    case exit
}

enum VRRoute {
    case anchor
    case audience
    case roomInfo
    case recentViewer
    case voiceLinkControl(_ coreView: SeatGridView)
    case linkInviteControl(_ coreView: SeatGridView, _ index: Int)
    case userControl(_ coreView: SeatGridView, _ user: VRSeatInfo)
    case featureSetting(_ settingModel: VRFeatureClickPanelModel)
    case listMenu(_ data: ActionPanelData)
    case audioEffect
    case giftView
    case systemImageSelection(_ imageType: VRImageType)
    case prepareSetting
    case alert(info: VRAlertInfo)
}

extension VRRoute: Equatable {
    static func == (lhs: VRRoute, rhs: VRRoute) -> Bool {
        switch (lhs, rhs) {
            case (.anchor,.anchor),
                (.audience,.audience),
                (.roomInfo,.roomInfo),
                (.recentViewer,.recentViewer),
                (.voiceLinkControl,.voiceLinkControl),
                (.audioEffect,.audioEffect),
                (.giftView, .giftView),
                (.prepareSetting, .prepareSetting),
                (.alert, .alert):
                return true
            case let (.featureSetting(l), .featureSetting(r)):
                return l == r
            case let (.listMenu(l), .listMenu(r)):
                return l == r
            case let (.systemImageSelection(l), .systemImageSelection(r)):
                return l == r
            case let (.linkInviteControl(l), .linkInviteControl(r)):
                return l == r
            case let (.userControl(l), .userControl(r)):
                return l == r
            case (.anchor, _),
                (.audience, _),
                (.roomInfo, _),
                (.recentViewer, _),
                (.voiceLinkControl, _),
                (.linkInviteControl, _),
                (.userControl, _),
                (.featureSetting, _),
                (.listMenu, _),
                (.audioEffect, _),
                (.giftView, _),
                (.systemImageSelection, _),
                (.prepareSetting, _),
                (.alert, _):
                return false
            default:
                break
        }
    }
}

extension VRRoute: Hashable {
    func convertToString() -> String {
        switch self {
            case .anchor:
                return "anchor"
            case .audience:
                return "audience"
            case .roomInfo:
                return "roomInfo"
            case .recentViewer:
                return "recentViewer"
            case .voiceLinkControl:
                return "voiceLinkControl"
            case .linkInviteControl(let index):
                return "linkInviteControl \(index)"
            case .userControl(let coreView, let seatInfo):
                return "linkInviteControl \(coreView) \(seatInfo.userId)"
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
            case .giftView:
                return "giftView"
            case .systemImageSelection(let imageType):
                return "systemImageSelection" + imageType.rawValue
            case .prepareSetting:
                return "prepareSetting"
            case .alert(let alertInfo):
                return "alert \(alertInfo.description)"
        }
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(self.convertToString())
    }
}

struct VRRouterState {
    var routeStack: [VRRoute] = []
    var dismissEvent: (() -> Void)?
}
