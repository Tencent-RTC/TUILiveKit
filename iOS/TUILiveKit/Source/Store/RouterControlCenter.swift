//
//  RouterControlCenter.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/14.
//

import Foundation
import Combine

protocol RouterViewProvider: NSObjectProtocol {
    func getRouteView(route: Route) -> UIView?
}

class RouterControlCenter {
    let rootRoute: Route
    @Injected private var routerStore: RouterStoreProvider
    
    weak var routerProvider: RouterViewProvider?
    private weak var rootViewController: UIViewController?
    private lazy var routeStackPublisher = routerStore.select(RouterSelectors.routeStack)
    private var cancellableSet = Set<AnyCancellable>()
    private var presentedRouteStack: [Route] = []
    private var presentedViewControllerMap: [Route: UIViewController] = [:]
    
    init(rootViewController: UIViewController, rootRoute: Route) {
        self.rootViewController = rootViewController
        self.rootRoute = rootRoute
        routerStore.dispatch(action: RouterActions.setRootRoute(payload: rootRoute))
    }
}


extension RouterControlCenter {
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
    
    func unSubscribeRouter() {
        cancellableSet.forEach { cancellable in
            cancellable.cancel()
        }
        cancellableSet.removeAll()
    }
}


extension RouterControlCenter {
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
        var view: UIView? = routerProvider?.getRouteView(route: route)
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
            routerStore.router(action: .dismiss)
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
            self.routerStore.router(action: .dismiss)
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
                    self.routerStore.router(action: .dismiss)
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
                    self.routerStore.router(action: .dismiss)
                }
                view = actionPanel
            case .linkSetting:
                view = VideoLinkSettingPanel()
            case .systemImageSelection:
                let systemImageSelectionPanel = SystemImageSelectionPanel(configs: SystemImageModel.configs())
                systemImageSelectionPanel.backButtonClickClosure = { [weak self] in
                    guard let self = self else { return }
                    self.routerStore.router(action: .dismiss)
                }
                view = systemImageSelectionPanel
            default:
                break
        }
        return view
    }
}
