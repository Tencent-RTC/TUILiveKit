//
//  AudienceRouterControlCenter.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/20.
//

import Combine
import TUICore
import RTCCommon
import RTCRoomEngine
import AtomicXCore

class AudienceRouterControlCenter {
    private var coreView: LiveCoreView?
    private var rootRoute: AudienceRoute
    private var routerManager: AudienceRouterManager
    private var manager: AudienceManager?
    
    private weak var rootViewController: UIViewController?
    private var cancellableSet = Set<AnyCancellable>()
    private var presentedRouteStack: [AudienceRoute] = []
    private var presentedViewControllerMap: [AudienceRoute: UIViewController] = [:]

    init(rootViewController: UIViewController, rootRoute: AudienceRoute, routerManager: AudienceRouterManager, manager: AudienceManager? = nil, coreView: LiveCoreView? = nil) {
        self.rootViewController = rootViewController
        self.rootRoute = rootRoute
        self.routerManager = routerManager
        self.manager = manager
        self.coreView = coreView
        routerManager.setRootRoute(route: rootRoute)
    }
    
    func handleScrollToNewRoom(manager: AudienceManager, coreView: LiveCoreView) {
        self.manager = manager
        self.coreView = coreView
        self.presentedViewControllerMap.removeAll()
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}

// MARK: - Subscription
extension AudienceRouterControlCenter {
    func subscribeRouter() {
        routerManager.subscribeRouterState(StateSelector(keyPath: \AudienceRouterState.routeStack))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] routeStack in
                guard let self = self else { return }
                self.comparePresentedVCWith(routeStack: routeStack)
            }
            .store(in: &cancellableSet)
    }
}

// MARK: - Route Handler
extension AudienceRouterControlCenter {
    private func comparePresentedVCWith(routeStack: [AudienceRoute]) {
        if routeStack.isEmpty {
            handleExitAction()
            return
        }
        
        if routeStack.count > presentedRouteStack.count + 1 {
            if let lastRoute = routeStack.last {
                handleRouteAction(route: lastRoute)
            }
            return
        }
        
        handleDismisAndRouteToAction(routeStack: routeStack)
    }
    
    private func handleExitAction() {
        presentedRouteStack.removeAll()
        presentedViewControllerMap.removeAll()
        exitLiveKit()
    }
    
    private func exitLiveKit() {
        if let navigationController = rootViewController?.navigationController {
            navigationController.popViewController(animated: true)
        } else {
            rootViewController?.dismiss(animated: true)
        }
    }
    
    private func handleRouteAction(route: AudienceRoute) {
        if route == rootRoute {
            rootViewController?.presentedViewController?.dismiss(animated: true)
        }
        
        if tryToPresentCachedViewController(route: route) {
            return
        }
                
        if let view = getRouteDefaultView(route: route) {
            var presentedViewController: UIViewController = UIViewController()
            switch route {
            case .alert(_):
                presentedViewController = presentAlert(alertView: view)
            default:
                presentedViewController = presentPopup(view: view, route: route)
            }
            presentedRouteStack.append(route)
            presentedViewControllerMap[route] = presentedViewController
        } else {
            routerManager.router(action: .dismiss())
        }
    }
    
    private func tryToPresentCachedViewController(route: AudienceRoute) -> Bool {
        var isSuccess = false
        if presentedViewControllerMap.keys.contains(route) {
            if let rootViewController = rootViewController,
               let presentedController = presentedViewControllerMap[route] {
                let presentingViewController = getPresentingViewController(rootViewController)
                presentingViewController.present(presentedController, animated: supportAnimation(route: route))
                presentedRouteStack.append(route)
                isSuccess = true
            }
        }
        return isSuccess
    }
    
    private func handleDismisAndRouteToAction(routeStack: [AudienceRoute]) {
        if routeStack.count == 1 && routeStack.contains(.audience) {
            if BeautyView.isDownloading {
                if let rootVC = rootViewController {
                    let vc = getPresentingViewController(rootVC)
                    if String(describing: type(of: vc)) == "TransparentPresentationController" {
                        BeautyView.isDownloading = false
                        vc.dismiss(animated: false) { [weak self] in
                            guard let self = self else { return }
                            handleDismisAndRouteToAction(routeStack: routeStack)
                        }
                        return
                    }
                }
            }
        }
        
        while routeStack.last != presentedRouteStack.last {
            if presentedRouteStack.isEmpty {
                break
            }
            
            if let route = presentedRouteStack.popLast(), let vc = presentedViewControllerMap[route] {
                if let dismissEvent = routerManager.routerState.dismissEvent {
                    vc.dismiss(animated: supportAnimation(route: route)) { [weak self] in
                        guard let self = self else { return }
                        dismissEvent()
                        self.routerManager.clearDismissEvent()
                    }
                } else {
                    vc.dismiss(animated: supportAnimation(route: route))
                }
                if isTempPanel(route: route) {
                    presentedViewControllerMap[route] = nil
                }
            }
        }
    }
}

// MARK: - Presenting ViewController
extension AudienceRouterControlCenter {
    private func getPresentingViewController(_ rootViewController: UIViewController) -> UIViewController {
        if let vc = rootViewController.presentedViewController {
            return getPresentingViewController(vc)
        } else {
            return rootViewController
        }
    }
}

// MARK: - Default Route View
extension AudienceRouterControlCenter {
    private func getRouteDefaultView(route: AudienceRoute) -> UIView? {
        guard let coreView = coreView, let manager = manager else { return nil }
        var view: UIView?
        switch route {
        case .audioEffect:
            let audioEffect = AudioEffectView()
            audioEffect.backButtonClickClosure = { [weak self] _ in
                guard let self = self else { return }
                self.routerManager.router(action: .dismiss())
            }
            view = audioEffect
        case .linkType(let data):
            view = LinkMicTypePanel(data: data, routerManager: routerManager, manager: manager)
        case .listMenu(let data):
            let actionPanel = ActionPanel(panelData: data)
            actionPanel.cancelActionClosure = { [weak self] in
                guard let self = self else { return }
                self.routerManager.router(action: .dismiss())
            }
            view = actionPanel
        case .linkSetting:
            view = VideoLinkSettingPanel(manager: manager, routerManager: routerManager, coreView: coreView)
        case .featureSetting:
            view = AudienceSettingPanel(manager: manager, routerManager: routerManager)
        case .videoQualitySelection(let resolutions, let selectedClosure):
            let selection = VideoQualitySelectionPanel(resolutions: resolutions, selectedClosure: selectedClosure)
            selection.cancelClosure = { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .dismiss())
            }
            view = selection
        case .alert(let info):
            view = AudienceAlertPanel(alertInfo: info)
        case .streamDashboard:
            view = StreamDashboardPanel(roomId: manager.roomState.roomId,
                                        roomEngine: TUIRoomEngine.sharedInstance())
        case .beauty:
            if BeautyView.checkIsNeedDownloadResource() {
                return nil
            }
            let beautyView = BeautyView.shared()
            beautyView.backClosure = { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .dismiss())
            }
            view = beautyView
        case .giftView:
            view = GiftListPanel(roomId: manager.roomState.roomId)
        case .userManagement(let user, let type):
            if type == .userInfo {
                view = AudienceUserInfoPanelView(user: user, manager: manager)
            } else {
                view = AudienceUserManagePanelView(user: user, manager: manager, routerManager: routerManager, coreView: coreView, type: type)
            }
        case .netWorkInfo(let manager,let isAudience):
            let netWorkInfoView = NetWorkInfoView(manager: manager, isAudience: isAudience)
            view = netWorkInfoView
        default:
            break
        }
        return view
    }
}

// MARK: - Route Staus
extension AudienceRouterControlCenter {
    private func isTempPanel(route: AudienceRoute) -> Bool {
        switch route {
        case .alert(_),
             .streamDashboard,
             .linkType(_),
             .userManagement(_, _),
             .videoQualitySelection(_, _):
            return true
        default:
            return false
        }
    }
    
    private func supportBlurView(route: AudienceRoute) -> Bool {
        switch route {
        case .beauty,
             .alert(_),
             .streamDashboard,
             .giftView,
             .listMenu(_),
             .userManagement(_, _),
             .netWorkInfo(_, _),
             .videoQualitySelection(_, _):
            return false
        default:
            return true
        }
    }
    
    private func supportAnimation(route: AudienceRoute) -> Bool{
        switch route {
        case .alert(_):
            return false
        default:
            return true
        }
    }
    
    private func getSafeBottomViewBackgroundColor(route: AudienceRoute) -> UIColor {
        var safeBottomViewBackgroundColor = UIColor.g2
        switch route {
        case .listMenu(_):
            safeBottomViewBackgroundColor = .white
        case .streamDashboard, .giftView, .beauty, .featureSetting, .videoQualitySelection:
            safeBottomViewBackgroundColor = .bgOperateColor
        default:
            break
        }
        return safeBottomViewBackgroundColor
    }
}

// MARK: - Popup
extension AudienceRouterControlCenter {
    private func presentPopup(view: UIView, route: AudienceRoute) -> UIViewController {
        let safeBottomViewBackgroundColor = getSafeBottomViewBackgroundColor(route: route)
        let menuContainerView = MenuContainerView(contentView: view, safeBottomViewBackgroundColor: safeBottomViewBackgroundColor)
        let popupViewController = PopupViewController(contentView: menuContainerView,
                                                  supportBlurView: supportBlurView(route: route))
        menuContainerView.blackAreaClickClosure = { [weak self] in
            guard let self = self else { return }
            self.routerManager.router(action: .dismiss())
        }
        guard let rootViewController = rootViewController else { return UIViewController()}
        let presentingViewController = getPresentingViewController(rootViewController)
        presentingViewController.present(popupViewController, animated: true)
        return popupViewController
    }
}

// MARK: - Alert
extension AudienceRouterControlCenter {
    private func presentAlert(alertView: UIView) -> UIViewController {
        let alertContainerView = AudienceAlertContainerView(contentView: alertView)
        let alerViewController = PopupViewController(contentView: alertContainerView,
                                                 supportBlurView: false)
        guard let rootViewController = rootViewController else { return UIViewController()}
        let presentingViewController = getPresentingViewController(rootViewController)
        presentingViewController.present(alerViewController, animated: false)
        
        return alerViewController
    }
}


