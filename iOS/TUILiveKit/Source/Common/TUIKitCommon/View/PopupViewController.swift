//
//  PopupViewController.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/20.
//

import SnapKit
import UIKit

public class PopupViewController: UIViewController {
    private let contentView: UIView
    public init(contentView: UIView) {
        self.contentView = contentView
        super.init(nibName: nil, bundle: nil)
        modalPresentationStyle = .custom
        transitioningDelegate = self
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public override func loadView() {
        self.view = contentView
    }
    
    public override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        return .portrait
    }
}

extension PopupViewController: UIViewControllerTransitioningDelegate {
    public func animationController(forPresented presented: UIViewController, presenting: UIViewController, source: UIViewController) ->
    UIViewControllerAnimatedTransitioning? {
        let transitionAnimator = AlertTransitionAnimator()
        transitionAnimator.alertTransitionStyle = .present
        if WindowUtils.isPortrait {
            transitionAnimator.alertTransitionPosition = .bottom
        } else {
            transitionAnimator.alertTransitionPosition = .right
        }
        return transitionAnimator
    }
    
    public func animationController(forDismissed dismissed: UIViewController) -> UIViewControllerAnimatedTransitioning? {
        let transitionAnimator = AlertTransitionAnimator()
        transitionAnimator.alertTransitionStyle = .dismiss
        if WindowUtils.isPortrait {
            transitionAnimator.alertTransitionPosition = .bottom
        } else {
            transitionAnimator.alertTransitionPosition = .right
        }
        return transitionAnimator
    }
}
