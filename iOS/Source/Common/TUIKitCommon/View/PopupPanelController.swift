//
//  PopUpPanel.swift
//  TUIKitCommon
//
//  Created by krabyu on 2023/10/12.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import SnapKit
import UIKit

public enum PopupPanelAction {
    case `default`
    case close
    case displayed
    case disappear
    case dismiss
}

public protocol PopupPanelSubViewProtocol: UIView {
    func setAction(_ action: Observable<PopupPanelAction>)
    func updateRootViewOrientation(isPortrait: Bool)
    func isSupportTouchToExit() -> Bool
    func isSupportBackgroundBlur() -> Bool
}

public extension PopupPanelSubViewProtocol {
    func isSupportTouchToExit() -> Bool {
        return true
    }

    func isSupportBackgroundBlur() -> Bool {
        return false
    }

}

class PopupPanelView: UIView {
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var blurView: UIVisualEffectView = {
        let view = UIVisualEffectView(effect: UIBlurEffect(style: .dark))
        view.alpha = 0
        return view
    }()

    let action: Observable<PopupPanelAction> = Observable(.default)
    var panelSubView: PopupPanelSubViewProtocol
    init(panelSubView: PopupPanelSubViewProtocol) {
        self.panelSubView = panelSubView
        self.panelSubView.setAction(action)
        super.init(frame: .zero)
        action.addObserver(self) { [weak self] action, _ in
            guard let self = self else { return }
            if action == .close {
                self.blurView.alpha = 0
            } else if action == .displayed {
                self.showBlurAnim()
            }
        }
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private lazy var panelControl: UIControl = {
        let control = UIControl()
        control.backgroundColor = .clear
        control.addTarget(self, action: #selector(panelControlAction), for: .touchUpInside)
        return control
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }

    private func constructViewHierarchy() {
        addSubview(blurView)
        addSubview(panelControl)
        addSubview(panelSubView)
    }

    private func activateConstraints() {
        blurView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        panelControl.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        panelSubView.updateRootViewOrientation(isPortrait: isPortrait)
    }

    private func showBlurAnim() {
        if panelSubView.isSupportBackgroundBlur() {
            UIView.animate(withDuration: 0.3) {
                self.blurView.alpha = 0.5
            }
        } else {
            blurView.alpha = 0
        }
    }

    @objc func panelControlAction() {
        if panelSubView.isSupportTouchToExit() {
            action.value = .close
        }
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        panelSubView.updateRootViewOrientation(isPortrait: isPortrait)
    }
}

public class PopupPanelController: UIViewController {
    private var rootView: PopupPanelView
    private var transitionAnimator: AlertTransitionAnimator? // 转场控制器
    init(rootView: PopupPanelView) {
        self.rootView = rootView
        super.init(nibName: nil, bundle: nil)
        modalPresentationStyle = .custom
        transitioningDelegate = self
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override public func viewDidLoad() {
        view.addSubview(rootView)
        rootView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        rootView.action.addObserver(self) { [weak self] action, _ in
            if action == .close {
                self?.dismiss(animated: true)
            }
        }
    }

    override public func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        rootView.action.value = .displayed
    }

    override public func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        rootView.action.value = .disappear
    }

    override public func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
        super.viewWillTransition(to: size, with: coordinator)
        let isPortrait = size.width < size.height
        rootView.updateRootViewOrientation(isPortrait: isPortrait)
    }

    public static func alertView(rootController: UIViewController? = nil, _ panelSubView: PopupPanelSubViewProtocol) {
        let controller = PopupPanelController(rootView: PopupPanelView(panelSubView: panelSubView))
        (rootController ?? WindowUtils.getCurrentWindowViewController())?.present(controller, animated: true)
    }

    override public func dismiss(animated flag: Bool, completion: (() -> Void)? = nil) {
        super.dismiss(animated: flag) { [weak self] in
            completion?()
            self?.rootView.action.value = .dismiss
        }
    }

}

extension PopupPanelController: UIViewControllerTransitioningDelegate {
    public func animationController(forPresented presented: UIViewController, presenting: UIViewController, source: UIViewController) ->
        UIViewControllerAnimatedTransitioning? {
        transitionAnimator = AlertTransitionAnimator()
        transitionAnimator?.alertTransitionStyle = .present
        if WindowUtils.isPortrait {
            transitionAnimator?.alertTransitionPosition = .bottom
        } else {
            transitionAnimator?.alertTransitionPosition = .right
        }
        return transitionAnimator
    }

    public func animationController(forDismissed dismissed: UIViewController) -> UIViewControllerAnimatedTransitioning? {
        transitionAnimator?.alertTransitionStyle = .dismiss
        if WindowUtils.isPortrait {
            transitionAnimator?.alertTransitionPosition = .bottom
        } else {
            transitionAnimator?.alertTransitionPosition = .right
        }

        return transitionAnimator
    }
}
