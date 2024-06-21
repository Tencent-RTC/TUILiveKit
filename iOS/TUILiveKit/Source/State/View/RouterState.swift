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
    case dismiss
    case exit
}

enum Route {
    case anchor
    case audience
    case roomInfo
    case recentViewer
    case linkControl
    case linkType // audience apply take seat.
    case linkSetting // audience take seat setting.
    case setting
    case listMenu(_ menus: [ActionItem])
    case musicList
    case audioEffect
    case beauty(_ hasRenderView: Bool)
    case videoSetting
    case giftView
    case systemImageSelection
}

extension Route: Equatable {
    static func == (lhs: Route, rhs: Route) -> Bool {
        switch (lhs, rhs) {
        case (.anchor,.anchor),
            (.audience,.audience),
            (.roomInfo,.roomInfo),
            (.recentViewer,.recentViewer),
            (.linkControl,.linkControl),
            (.linkType, .linkType),
            (.linkSetting, .linkSetting),
            (.setting,.setting),
            (.musicList,.musicList),
            (.audioEffect,.audioEffect),
            (.beauty, .beauty),
            (.videoSetting,.videoSetting),
            (.giftView, .giftView),
            (.systemImageSelection, .systemImageSelection):
            return true
        case let (.listMenu(l), .listMenu(r)):
            return l == r
        case (.anchor, _),
            (.audience, _),
            (.roomInfo, _),
            (.recentViewer, _),
            (.linkControl, _),
            (.linkType, _),
            (.linkSetting, _),
            (.setting, _),
            (.listMenu, _),
            (.musicList, _),
            (.audioEffect, _),
            (.beauty, _),
            (.videoSetting, _),
            (.giftView, _),
            (.systemImageSelection, _):
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
        case .linkControl:
            return "linkControl"
        case .linkType:
            return "linkType"
        case .linkSetting:
            return "linkSetting"
        case .setting:
            return "setting"
        case .listMenu(let items):
            var result = "listMenu"
            items.forEach { item in
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
        case .systemImageSelection:
            return "systemImageSelection"
        }
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(self.convertToString())
    }
}

struct RouterState {
    var routeStack: [Route] = []
}

class PrintRouterStateInterceptor: Interceptor {
    typealias State = RouterState
    func actionDispatched(action: any Action, oldState: RouterState, newState: RouterState) {
        let name = String(describing: type(of: self))
        let actionName = String(describing: type(of: action))
        var actionLog = "\(name) - action dispatched: \(actionName)"
        print("action: \(actionLog), navigation stack: old: \(oldState.routeStack)")
        print("action: \(actionLog), navigation stack: new: \(newState.routeStack)")
    }
}
