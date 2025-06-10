//
//  TUILiveListViewController.swift
//  Alamofire
//
//  Created by adamsfliu on 2024/5/30.
//

import UIKit
import RTCCommon
import TUICore
import Combine
import RTCRoomEngine
import TUILiveResources

public class TUILiveListViewController: UIViewController {
    // MARK: - Internal property.
    private var needRestoreNavigationBarHiddenState: Bool = false
    
    private lazy var rootView: LiveListView = {
        let view = LiveListView(style: currentStyle)
        view.itemClickDelegate = self
        return view
    }()
    
    private var currentStyle = LiveListViewStyle.doubleColumn
    
    private var cancellableSet = Set<AnyCancellable>()
    private var popupViewController: UIViewController?
    
    public override func loadView() {
        super.loadView()
        view = rootView
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        initNavigationItemTitleView()
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }
    
    public override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        rootView.refreshLiveList()
    }
    
    public override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        rootView.onRouteToNextPage()
    }
    
    private func initNavigationItemTitleView() {
        let backBtn = UIButton(type: .custom)
        backBtn.setImage(internalImage("live_back")?.withTintColor(.white), for: .normal)
        backBtn.addTarget(self, action: #selector(backBtnClick), for: .touchUpInside)
        backBtn.sizeToFit()
        let backItem = UIBarButtonItem(customView: backBtn)
        navigationItem.leftBarButtonItem = backItem
        
        let titleView = UILabel()
        titleView.text = .liveTitleText
        titleView.textColor = .white
        titleView.textAlignment = .center
        titleView.font = UIFont.boldSystemFont(ofSize: 17)
        titleView.adjustsFontSizeToFitWidth = true
        let width = titleView.sizeThatFits(CGSize(width: CGFloat.greatestFiniteMagnitude,
                                                  height: CGFloat.greatestFiniteMagnitude)).width
        titleView.frame = CGRect(origin:CGPoint.zero, size:CGSize(width: width, height: 500))
        self.navigationItem.titleView = titleView
        
        let helpBtn = UIButton(type: .custom)
        helpBtn.setImage(internalImage("help_small")?.withTintColor(.white), for: .normal)
        helpBtn.addTarget(self, action: #selector(helpBtnClick), for: .touchUpInside)
        helpBtn.sizeToFit()
        let rightItem = UIBarButtonItem(customView: helpBtn)
        rightItem.tintColor = .black
        navigationItem.rightBarButtonItem = rightItem
    }
    
    public func setColumnStyle(style: LiveListViewStyle) {
        currentStyle = style
        rootView.setColumnStyle(style: style)
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}

extension TUILiveListViewController {
    @objc
    private func backBtnClick(sender: UIButton) {
        if let nav = navigationController {
            nav.popViewController(animated: true)
        } else {
            dismiss(animated: true)
        }
    }
    
    @objc
    private func helpBtnClick(sender: UIButton) {
        if let url = URL(string: "https://cloud.tencent.com/document/product/647/105441") {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }

}

extension TUILiveListViewController: OnItemClickDelegate {
    public func onItemClick(liveInfo: LiveInfo, frame: CGRect) {
        if FloatWindow.shared.isShowingFloatWindow() {
            if FloatWindow.shared.getCurrentRoomId() == liveInfo.roomId {
                FloatWindow.shared.resumeLive(atViewController: self.navigationController ?? self)
                return
            } else if let ownerId = FloatWindow.shared.getRoomOwnerId(), ownerId == TUILogin.getUserID() {
                view.makeToast(.pushingToReturnText)
                return
            } else if FloatWindow.shared.getIsLinking() {
                view.makeToast(.pushingToReturnText)
                return
            } else {
                FloatWindow.shared.releaseFloatWindow()
            }
        }
        let roomType = LiveIdentityGenerator.shared.getIDType(liveInfo.roomId)
        let isOwner = liveInfo.ownerId == TUILogin.getUserID()
        switch roomType {
        case .voice:
            let vc = TUIVoiceRoomViewController(roomId: liveInfo.roomId, behavior: isOwner ? .autoCreate : .join)
            vc.modalPresentationStyle = .custom
            let transitionDelegate = LiveTransitioningDelegate(originFrame: frame)
            vc.transitioningDelegate = transitionDelegate
            present(vc, animated: true)
        default:
            // How to determine room type without roomId
            if isOwner {
                let vc = TUILiveRoomAnchorViewController(roomId: liveInfo.roomId, needPrepare: false)
                vc.modalPresentationStyle = .custom
                let transitionDelegate = LiveTransitioningDelegate(originFrame: frame)
                vc.transitioningDelegate = transitionDelegate
                present(vc, animated: true)
            } else {
                let vc = TUILiveRoomAudienceViewController(roomId: liveInfo.roomId)
                vc.modalPresentationStyle = .custom
                let transitionDelegate = LiveTransitioningDelegate(originFrame: frame)
                vc.transitioningDelegate = transitionDelegate
                present(vc, animated: true)
            }
        }
    }
}

class LivePresentationController: UIPresentationController {
    override var shouldRemovePresentersView: Bool {
        return true
    }
}

class LivePresentAnimation: NSObject, UIViewControllerAnimatedTransitioning {
    var originFrame: CGRect

    init(originFrame: CGRect) {
        self.originFrame = originFrame
    }

    func transitionDuration(using transitionContext: UIViewControllerContextTransitioning?) -> TimeInterval {
        return 0.5
    }

    func animateTransition(using transitionContext: UIViewControllerContextTransitioning) {
        guard let toVC = transitionContext.viewController(forKey: .to) else { return }
        let containerView = transitionContext.containerView

        containerView.addSubview(toVC.view)

        let finalFrame = transitionContext.finalFrame(for: toVC)

        toVC.view.frame = originFrame
        toVC.view.layoutIfNeeded()
        toVC.view.clipsToBounds = true

        UIView.animate(withDuration: transitionDuration(using: transitionContext), animations: {
            toVC.view.frame = finalFrame
        }, completion: { _ in
            transitionContext.completeTransition(!transitionContext.transitionWasCancelled)
        })
    }
}

class LiveTransitioningDelegate: NSObject, UIViewControllerTransitioningDelegate {
    var originFrame: CGRect

    init(originFrame: CGRect) {
        self.originFrame = originFrame
    }

    func animationController(forPresented presented: UIViewController, presenting: UIViewController, source: UIViewController) -> UIViewControllerAnimatedTransitioning? {
        return LivePresentAnimation(originFrame: originFrame)
    }
    
    func presentationController(forPresented presented: UIViewController,
                                presenting: UIViewController?,
                                source: UIViewController) -> UIPresentationController? {
        return LivePresentationController(presentedViewController: presented, presenting: presenting)
    }
}

extension String {
    fileprivate static let liveTitleText = internalLocalized("Live Video")
    fileprivate static let pushingToReturnText = internalLocalized("Live streaming in progress. Please try again later.")
}
