//
//  RouterControlCenter.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/14.
//

import Foundation
import Combine
import RTCCommon

protocol RouterViewProvider: NSObjectProtocol {
    func getRouteView(route: Route) -> UIView?
}

class RouterControlCenter {
    private var rootRoute: Route
    private var store: LiveStoreProvider
    private var routerStore: RouterStore
    
    weak var routerProvider: RouterViewProvider?
    private weak var rootViewController: UIViewController?
    private lazy var routeStackPublisher = routerStore.select(RouterSelectors.routeStack)
    private var cancellableSet = Set<AnyCancellable>()
    private var presentedRouteStack: [Route] = []
    private var presentedViewControllerMap: [Route: UIViewController] = [:]
    
    init(rootViewController: UIViewController, store: LiveStoreProvider, rootRoute: Route, routerStore: RouterStore) {
        self.rootViewController = rootViewController
        self.rootRoute = rootRoute
        self.store = store
        self.routerStore = routerStore
        routerStore.dispatch(action: RouterActions.setRootRoute(payload: rootRoute))
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
    
    func updateStore(rootRoute: Route, store: LiveStoreProvider) {
        self.rootRoute = rootRoute
        self.store = store
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
            let presentedVC = presentPopup(view: view, route: route)
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
    
    private func presentPopup(view: UIView, route: Route) -> UIViewController {
        let menuContainerView = MenuContainerView(contentView: view)
        let viewController = PopupViewController(contentView: menuContainerView,
                                                 supportBlurView: supportBlurView(route: route))
        menuContainerView.blackAreaClickClosure = { [weak self] in
            guard let self = self else { return }
            self.routerStore.router(action: .dismiss)
        }
        guard let rootViewController = rootViewController else { return UIViewController()}
        let vc = getPresentedViewController(rootViewController)
        vc.present(viewController, animated: true)
        return viewController
    }
    
    private func supportBlurView(route: Route) -> Bool {
        switch route {
        case .beauty:
            return false
        case .featureSetting(_):
            if let _ = rootViewController as? TUILiveRoomAnchorViewController {
                return false
            } else {
                return true
            }
        default:
            return true
        }
    }
    
    private func getRouteDefaultView(route: Route) -> UIView? {
        var view: UIView?
        switch route {
            case .roomInfo:
                view = RoomInfoPanelView(store: store)
            case .recentViewer:
                view = RecentWatchMemberPanel(store: store, routerStore: routerStore)
            case .liveLinkControl:
                view = AnchorLinkControlPanel(store: store, routerStore: routerStore)
            case .voiceLinkControl:
                view = AnchorSeatControlPanel(store: store)
            case .featureSetting(let settingPanelModel):
                view = SettingPanel(settingPanelModel: settingPanelModel)
            case .musicList:
                view = MusicPanelView(roomId: store.selectCurrent(RoomSelectors.getRoomId),
                                      trtcCloud: store.roomEngine.getTRTCCloud())
            case .audioEffect:
                let audioEffect = AudioEffectView(roomId: store.selectCurrent(RoomSelectors.getRoomId),
                                                  trtcCloud: store.roomEngine.getTRTCCloud())
                audioEffect.backButtonClickClosure = { [weak self] _ in
                    guard let self = self else { return }
                    self.routerStore.router(action: .dismiss)
                }
                view = audioEffect
            case .videoSetting:
                view = AnchorVideoParametersSettingPanel(store: store, routerStore: routerStore)
            case .linkType:
                let dataHelper = LiveRoomRootMenuDataHelper()
                let data = dataHelper.generateLinkTypeMenuData(store: store, routerStore: routerStore)
                view = LinkMicTypePanel(data: data, routerStore: routerStore)
            case .listMenu(let items):
                let actionPanel = ActionPanel(items: items)
                actionPanel.cancelActionClosure = { [weak self] in
                    guard let self = self else { return }
                    self.routerStore.router(action: .dismiss)
                }
                view = actionPanel
            case .linkSetting:
                view = VideoLinkSettingPanel(store: store, routerStore: routerStore)
            case .systemImageSelection(let imageType):
                let imageConfig = SystemImageFactory.getImageAssets(imageType: imageType)
                let systemImageSelectionPanel = SystemImageSelectionPanel(configs: imageConfig,
                                                                          store: store,
                                                                          panelMode: imageType == .cover ? .cover : .background)
                systemImageSelectionPanel.backButtonClickClosure = { [weak self] in
                    guard let self = self else { return }
                    self.routerStore.router(action: .dismiss)
                }
                view = systemImageSelectionPanel
            case .prepareSetting:
                view = PrepareSettingPanel(store: store, routerStore: routerStore)
            default:
                break
        }
        return view
    }
}
