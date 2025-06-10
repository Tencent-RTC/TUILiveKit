//
//  TUILiveRoomAnchorPrepareViewController.swift
//  TUILiveKit
//
//  Created by gg on 2025/4/17.
//

import LiveStreamCore
import RTCCommon
import Combine
import TUICore
import TUILiveResources

public class TUILiveRoomAnchorPrepareViewController: UIViewController {
    private let roomId: String
    public init(roomId: String) {
        self.roomId = roomId
        super.init(nibName: nil, bundle: nil)
    }
    
    deinit {
        LiveKitLog.info("\(#file)", "\(#line)", "deinit TUILiveRoomAnchorPrepareViewController \(self)")
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private let coreView = LiveCoreView()
    
    private lazy var rootView: AnchorPrepareView = {
        let view = AnchorPrepareView(coreView: coreView)
        view.delegate = self
        return view
    }()
    
    private lazy var anchorVC: TUILiveRoomAnchorViewController = {
        let vc = TUILiveRoomAnchorViewController(roomId: roomId, needPrepare: true, coreView: coreView)
        vc.modalPresentationStyle = .fullScreen
        return vc
    }()
    
    var willStartLive: ((_ vc: TUILiveRoomAnchorViewController) -> ())?
    
    public override func viewDidLoad() {
        super.viewDidLoad()
    }
    
    public override func loadView() {
        view = rootView
    }
    
    public override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
        super.viewWillTransition(to: size, with: coordinator)
        let isPortrait = size.width < size.height
        rootView.updateRootViewOrientation(isPortrait: isPortrait)
    }
}

extension TUILiveRoomAnchorPrepareViewController : AnchorPrepareViewDelegate {
    public func prepareView(_ view: AnchorPrepareView, didClickBack button: UIButton) {
        if let nav = navigationController {
            nav.popViewController(animated: true)
        } else {
            dismiss(animated: true)
        }
        StateCache.shared.clear()
    }
    
    public func prepareView(_ view: AnchorPrepareView, didClickStart button: UIButton, editInfo: EditInfo) {
        guard let rootVC = TUITool.applicationKeywindow().rootViewController else { return }
        
        willStartLive?(anchorVC)
        anchorVC.startLiveStream(roomName: editInfo.roomName, privacyMode: editInfo.privacyMode, coverUrl: editInfo.coverUrl)
        
        let tmpView: UIView
        if let snapshot = rootView.snapshotView(afterScreenUpdates: true) {
            tmpView = snapshot
        } else {
            tmpView = rootView
        }
        rootVC.view.addSubview(tmpView)
        tmpView.frame = rootView.bounds
        
        dismiss(animated: false) { [weak self, weak tmpView, weak rootVC] in
            guard let self = self, let tmpView = tmpView, let rootVC = rootVC else { return }
            rootVC.present(anchorVC, animated: false) { [weak tmpView, weak rootVC] in
                guard let tmpView = tmpView, let rootVC = rootVC else { return }
                rootVC.view.bringSubviewToFront(tmpView)
                UIView.animate(withDuration: 0.3) {
                    tmpView.alpha = 0
                } completion: { _ in
                    tmpView.removeFromSuperview()
                }
            }
        }
    }
}
