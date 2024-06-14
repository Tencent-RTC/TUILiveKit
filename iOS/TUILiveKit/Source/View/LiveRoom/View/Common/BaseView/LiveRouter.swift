//
//  ViewControllerRouteHelper.swift
//  TUILiveKit
//
//  Created by aby on 2024/6/4.
//

import Foundation
import Combine

protocol RouteViewProvider: AnyObject {
    func getRouteView(route: LiveRouter.Route) -> UIView?
}

class LiveRouter {
    
    enum Action {
        case present(_ route: LiveRouter.Route)
        case pop
        case popToRoute(_ route: LiveRouter.Route)
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
    // MARK: - public property.
    let rootRoute: Route
    weak var viewProvider: RouteViewProvider?
    // MARK: - private property.
    private weak var rootViewController: UIViewController?
    private var cancellableSet = Set<AnyCancellable>()
    private var presentedRouteStack: [Route] = []
    private var presentedViewControllerMap: [Route: UIViewController] = [:]
    lazy var routeStackPublisher = viewStore.select(LiveRoomViewSelectors.routeStack)
    @Injected private var viewStore: LiveRoomViewStore
    var cancellable: AnyCancellable?
    init(rootViewController: UIViewController, rootRoute: Route) {
        self.rootViewController = rootViewController
        self.rootRoute = rootRoute
        viewStore.dispatch(action: LiveRoomNavigatorActions.setRootRoute(payload: rootRoute))
    }
}

extension LiveRouter {
    func subscribeRouter() {
        routeStackPublisher
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] routeStack in
                guard let self = self else { return }
                self.comparePresentedVCWith(routeStack: routeStack)
            }
            .store(in: &cancellableSet)
    }
}

extension LiveRouter {
    private func comparePresentedVCWith(routeStack: [Route]) {
        if routeStack.count > presentedRouteStack.count + 1 {
            if let lastRoute = routeStack.last {
                handleRouteAction(route: lastRoute)
            }
        } else {
            if routeStack.isEmpty {
                presentedRouteStack.removeAll()
                presentedViewControllerMap.removeAll()
                exitLiveKit()
            } else {
                while routeStack.last != presentedRouteStack.last {
                    if presentedRouteStack.isEmpty {
                        break
                    }
                    if let route = presentedRouteStack.popLast(), let vc = presentedViewControllerMap[route] {
                        vc.dismiss(animated: true)
                        presentedViewControllerMap.removeValue(forKey: route)
                    }
                }
            }
        }
    }
    
    private func handleRouteAction(route: Route) {
        var view: UIView? = viewProvider?.getRouteView(route: route)
        if route == rootRoute {
            rootViewController?.presentedViewController?.dismiss(animated: true)
        }
        if view == nil {
            view = getRouteDefaultView(route: route)
        }
        if let view = view {
            let presentedVC = self.presentPopup(view: view)
            presentedRouteStack.append(route)
            presentedViewControllerMap[route] = presentedVC
        } else {
            viewStore.navigate(action: .pop)
        }
    }
    
    private func exitLiveKit() {
        if let navigationController = rootViewController?.navigationController {
            navigationController.popViewController(animated: true)
        } else {
            rootViewController?.dismiss(animated: true)
        }
    }
    
    private func getPresentedViewController(_ rootViewController: UIViewController) -> UIViewController {
        if let vc = rootViewController.presentedViewController {
            return getPresentedViewController(vc)
        } else {
            return rootViewController
        }
    }
    
    private func presentPopup(view: UIView) -> UIViewController {
        let menuContainerView = MenuContainerView(contentView: view)
        let viewController = PopupViewController(contentView: menuContainerView)
        menuContainerView.blackAreaClickClosure = { [weak self] in
            guard let self = self else { return }
            self.viewStore.navigate(action: .pop)
        }
        guard let rootViewController = rootViewController else { return UIViewController()}
        let vc = getPresentedViewController(rootViewController)
        vc.present(viewController, animated: true)
        return viewController
    }
    
    private func getRouteDefaultView(route: Route) -> UIView? {
        var view: UIView?
        switch route {
            case .roomInfo:
                view = RoomInfoPanelView()
            case .recentViewer:
                view = RecentWatchMemberPanel()
            case .linkControl:
                view = AnchorLinkControlPanel()
            case .setting:
                view = AnchorSettingPanel()
            case .musicList:
                view = MusicPanelView(frame: .zero)
            case .audioEffect:
                let audioEffect = AudioEffectView()
                audioEffect.backButtonClickClosure = { [weak self] _ in
                    guard let self = self else { return }
                    self.viewStore.navigate(action: .pop)
                }
                view = audioEffect
            case let .beauty(hasRenderView):
                view = BeautyPanel(hasRenderView: hasRenderView)
            case .videoSetting:
                view = AnchorVideoParametersSettingPanel()
            case .linkType:
                let dataHelper = BottomPopupListViewDataHelper()
                let data = dataHelper.generateLinkTypeMenuData()
                view = LinkMicTypePanel(data: data)
            case .listMenu(let items):
                let actionPanel = ActionPanel(items: items)
                actionPanel.cancelActionClosure = { [weak self] in
                    guard let self = self else { return }
                    self.viewStore.navigate(action: .pop)
                }
                view = actionPanel
            case .linkSetting:
                view = VideoLinkSettingPanel()
            case .systemImageSelection:
                let systemImageSelectionPanel = SystemImageSelectionPanel(configs: SystemImageModel.configs())
                systemImageSelectionPanel.backButtonClickClosure = { [weak self] in
                    guard let self = self else { return }
                    self.viewStore.navigate(action: .pop)
                }
                view = systemImageSelectionPanel
            default:
                break
        }
        return view
    }
}

extension LiveRouter.Route: Equatable {
    static func == (lhs: LiveRouter.Route, rhs: LiveRouter.Route) -> Bool {
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

extension LiveRouter.Route: Hashable {
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


