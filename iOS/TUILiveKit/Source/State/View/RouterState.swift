//
//  RouterState.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/14.
//

import UIKit

enum RouterAction {
    case routeTo(_ route: Route)
    case present(_ route: Route)
    case dismiss(_ animated: Bool = true, completion: (() -> Void)? = nil)
    case exit
}

enum Route {
    case anchor
    case audience
    case roomInfo
    case recentViewer
    case liveLinkControl
    case connectionControl
    case voiceLinkControl
    case linkInviteControl(_ index: Int)
    case userControl(_ user: SeatInfo)
    case linkType // audience apply take seat.
    case linkSetting // audience take seat setting.
    case featureSetting(_ settingModel: FeatureClickPanelModel)
    case listMenu(_ data: ActionPanelData)
    case musicList
    case audioEffect
    case beauty
    case videoSetting
    case giftView
    case systemImageSelection(_ imageType: ImageType)
    case prepareSetting
    case battleCountdown(_ countdownTime: TimeInterval)
}

extension Route: Equatable {
    static func == (lhs: Route, rhs: Route) -> Bool {
        switch (lhs, rhs) {
            case (.anchor,.anchor),
                (.audience,.audience),
                (.roomInfo,.roomInfo),
                (.recentViewer,.recentViewer),
                (.liveLinkControl,.liveLinkControl),
                (.connectionControl,.connectionControl),
                (.voiceLinkControl,.voiceLinkControl),
                (.linkType, .linkType),
                (.linkSetting, .linkSetting),
                (.musicList,.musicList),
                (.audioEffect,.audioEffect),
                (.beauty, .beauty),
                (.videoSetting,.videoSetting),
                (.giftView, .giftView),
                (.prepareSetting, .prepareSetting):
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
            case let (.battleCountdown(l), .battleCountdown(r)):
                return l == r
            case (.anchor, _),
                (.audience, _),
                (.roomInfo, _),
                (.recentViewer, _),
                (.liveLinkControl, _),
                (.connectionControl, _),
                (.voiceLinkControl, _),
                (.linkInviteControl, _),
                (.userControl, _),
                (.linkType, _),
                (.linkSetting, _),
                (.featureSetting, _),
                (.listMenu, _),
                (.musicList, _),
                (.audioEffect, _),
                (.beauty, _),
                (.videoSetting, _),
                (.giftView, _),
                (.systemImageSelection, _),
                (.prepareSetting, _),
                (.battleCountdown, _):
                return false
            default:
                break
        }
    }
}

extension Route: Hashable {
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
            case .liveLinkControl:
                return "liveLinkControl"
            case .connectionControl:
                return "connectionControl"
            case .voiceLinkControl:
                return "voiceLinkControl"
            case .linkInviteControl(let index):
                return "linkInviteControl \(index)"
            case .userControl(let seatInfo):
                return "linkInviteControl \(seatInfo.userId)"
            case .linkType:
                return "linkType"
            case .linkSetting:
                return "linkSetting"
            case .featureSetting(let settingModel):
                return "featureSetting" + settingModel.id.uuidString
            case .listMenu(let data):
                var result = "listMenu"
                data.items.forEach { item in
                    result += item.id.uuidString
                }
                return result
            case .musicList:
                return "musicList"
            case .audioEffect:
                return "audioEffect"
            case .beauty:
                return "beauty"
            case .videoSetting:
                return "videoSetting"
            case .giftView:
                return "giftView"
            case .systemImageSelection(let imageType):
                return "systemImageSelection" + imageType.rawValue
            case .prepareSetting:
                return "prepareSetting"
            case .battleCountdown(let countdownTime):
                return "battleCountdown \(countdownTime)"
        }
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(self.convertToString())
    }
}

struct RouterState {
    var routeStack: [Route] = []
    var dismissEvent: (animated: Bool, completion: (() -> Void)?)?
}

class PrintRouterStateInterceptor: Interceptor {
    typealias State = RouterState
    func actionDispatched(action: any Action, oldState: RouterState, newState: RouterState) {
        let name = String(describing: type(of: self))
        let actionName = String(describing: type(of: action))
        let actionLog = "\(name) - action dispatched: \(actionName)"
        print("action: \(actionLog), navigation stack: old: \(oldState.routeStack)")
        print("action: \(actionLog), navigation stack: new: \(newState.routeStack)")
    }
}
