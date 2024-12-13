//
//  VRRouterControlerCenter.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/18.
//

import Combine
import RTCCommon
import RTCRoomEngine

protocol VRRouterViewProvider: NSObjectProtocol {
    func getRouteView(route: VRRoute) -> UIView?
}

class VRRouterControlCenter {
    private var rootRoute: VRRoute
    private var routerManager: VRRouterManager
    private let manager: VoiceRoomManager
    
    weak var routerProvider: VRRouterViewProvider?
    private weak var rootViewController: UIViewController?
    private var cancellableSet = Set<AnyCancellable>()
    private var presentedRouteStack: [VRRoute] = []
    private var presentedViewControllerMap: [VRRoute: UIViewController] = [:]
    
    init(rootViewController: UIViewController, rootRoute: VRRoute, routerManager: VRRouterManager, manager: VoiceRoomManager) {
        self.rootViewController = rootViewController
        self.rootRoute = rootRoute
        self.routerManager = routerManager
        self.manager = manager
        routerManager.setRootRoute(route: rootRoute)
    }
    
    func updateRootRoute(rootRoute: VRRoute) {
        self.rootRoute = rootRoute
        routerManager.setRootRoute(route: rootRoute)
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}

// MARK: - Subscription
extension VRRouterControlCenter {
    func subscribeRouter() {
        routerManager.subscribeRouterState(StateSelector(keyPath: \VRRouterState.routeStack))
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

// MARK: - Route Handler
extension VRRouterControlCenter {
    private func comparePresentedVCWith(routeStack: [VRRoute]) {
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
    
    private func handleRouteAction(route: VRRoute) {
        if route == rootRoute {
            rootViewController?.presentedViewController?.dismiss(animated: true)
        }
        
        if tryToPresentCachedViewController(route: route) {
            return
        }
        
        var view: UIView? = routerProvider?.getRouteView(route: route)
        
        if view == nil {
            view = getRouteDefaultView(route: route)
        }
        
        if let view = view {
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
    
    private func tryToPresentCachedViewController(route: VRRoute) -> Bool {
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
    
    private func handleDismisAndRouteToAction(routeStack: [VRRoute]) {
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
extension VRRouterControlCenter {
    private func getPresentingViewController(_ rootViewController: UIViewController) -> UIViewController {
        if let vc = rootViewController.presentedViewController {
            return getPresentingViewController(vc)
        } else {
            return rootViewController
        }
    }
}

// MARK: - Default Route View
extension VRRouterControlCenter {
    private func getRouteDefaultView(route: VRRoute) -> UIView? {
        var view: UIView?
        switch route {
        case .voiceLinkControl(let coreView):
            view = VRSeatManagerPanel(manager: manager, routerManager: routerManager, coreView: coreView)
        case .linkInviteControl(let coreView, let index):
            view = VRSeatInvitationPanel(manager: manager, routerManager: routerManager, coreView: coreView, seatIndex: index)
        case .userControl(let coreView, let seatInfo):
            view = VRUserManagerPanel(manager: manager, routerMangear: routerManager, coreView: coreView, seatInfo: seatInfo)
        case .featureSetting(let settingPanelModel):
            view = VRSettingPanel(settingPanelModel: settingPanelModel)
        case .musicList:
            view = MusicView(roomId: manager.roomState.roomId,trtcCloud: TUIRoomEngine.sharedInstance().getTRTCCloud())
        case .audioEffect:
            let audioEffect = AudioEffectView()
            audioEffect.backButtonClickClosure = { [weak self] _ in
                guard let self = self else { return }
                self.routerManager.router(action: .dismiss())
            }
            view = audioEffect
        case .listMenu(let data):
            let actionPanel = ActionPanel(panelData: data)
            actionPanel.cancelActionClosure = { [weak self] in
                guard let self = self else { return }
                self.routerManager.router(action: .dismiss())
            }
            view = actionPanel
        case .systemImageSelection(let imageType):
            let imageConfig = VRSystemImageFactory.getImageAssets(imageType: imageType)
            let systemImageSelectionPanel = VRImageSelectionPanel(configs: imageConfig,
                                                                  panelMode: imageType == .cover ? .cover : .background, manager: manager)
            systemImageSelectionPanel.backButtonClickClosure = { [weak self] in
                guard let self = self else { return }
                self.routerManager.router(action: .dismiss())
            }
            view = systemImageSelectionPanel
        case .prepareSetting:
            view = VRPrepareSettingPanel(manager: manager, routerManager: routerManager)
        case .alert(let info):
            view = VRAlertPanel(alertInfo: info)
        default:
            break
        }
        return view
    }
}

// MARK: - Route Staus
extension VRRouterControlCenter {
    private func isTempPanel(route: VRRoute) -> Bool {
        switch route {
        case .alert(_):
            return true
        default:
            return false
        }
    }
    
    private func supportBlurView(route: VRRoute) -> Bool {
        return true
    }
    
    private func supportAnimation(route: VRRoute) -> Bool{
        switch route {
        case .alert(_):
            return false
        default:
            return true
        }
    }
    
    private func getSafeBottomViewBackgroundColor(route: VRRoute) -> UIColor {
        var safeBottomViewBackgroundColor = UIColor.g2
        switch route {
        case .listMenu(_):
            safeBottomViewBackgroundColor = .white
        default:
            break
        }
        return safeBottomViewBackgroundColor
    }
}

// MARK: - Popup
extension VRRouterControlCenter {
    private func presentPopup(view: UIView, route: VRRoute) -> UIViewController {
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
extension VRRouterControlCenter {
    private func presentAlert(alertView: UIView) -> UIViewController {
        let alertContainerView = VRAlertContainerView(contentView: alertView)
        let alerViewController = PopupViewController(contentView: alertContainerView,
                                                 supportBlurView: false)
        guard let rootViewController = rootViewController else { return UIViewController()}
        let presentingViewController = getPresentingViewController(rootViewController)
        presentingViewController.present(alerViewController, animated: false)
        
        return alerViewController
    }
}
