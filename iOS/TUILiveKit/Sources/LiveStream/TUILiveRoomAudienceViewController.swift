//
//  TUILiveRoomAudienceViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/11.
//
import UIKit
import LiveStreamCore
import RTCRoomEngine

public class TUILiveRoomAudienceViewController: UIViewController {
    
    private lazy var audienceContainerView: AudienceContainerView = {
        let view = AudienceContainerView(roomId: roomId)
        view.delegate = self
        view.rotateScreenDelegate = self
        return view
    }()
    
    // MARK: - private property.
    var roomId: String
    private var orientation: UIDeviceOrientation = .portrait
    public init(roomId: String) {
        self.roomId = roomId
        super.init(nibName: nil, bundle: nil)
    }
    
    public init(liveInfo: TUILiveInfo) {
        self.roomId = liveInfo.roomInfo.roomId
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        LiveKitLog.info("\(#file)", "\(#line)", "deinit TUILiveRoomAudienceViewController \(self)")
        
#if DEV_MODE
        TestTool.shared.unregisterCaseFrom(self)
#endif
        TUIGiftStore.shared.reset()
        unregisterApplicationObserver()
    }
    
    public func leaveLive(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        audienceContainerView.leaveLive(onSuccess: onSuccess, onError: onError)
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        constructViewHierarchy()
        activateConstraints()
        registerApplicationObserver()
        
#if DEV_MODE
        let scrolling = TestCaseItemModel(title: "禁用滑动", view: audienceContainerView, sel: #selector(AudienceContainerView.disableSlidingForTest(_:)))
        let floatWin = TestCaseItemModel(title: "禁用悬浮窗按钮", view: audienceContainerView, sel: #selector(AudienceContainerView.disableHeaderFloatWinForTest(_:)))
        let liveData = TestCaseItemModel(title: "禁用房间信息", view: audienceContainerView, sel: #selector(AudienceContainerView.disableHeaderLiveDataForTest(_:)))
        let visitor = TestCaseItemModel(title: "禁用观众列表", view: audienceContainerView, sel: #selector(AudienceContainerView.disableHeaderVisitorCntForTest(_:)))
        let coGuest = TestCaseItemModel(title: "禁用连麦按钮", view: audienceContainerView, sel: #selector(AudienceContainerView.disableFooterCoGuestForTest(_:)))
        let model = TestCaseModel(list: [scrolling, floatWin, liveData, visitor, coGuest], obj: self)
        TestTool.shared.registerCase(model)
#endif
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(true, animated: true)
        UIApplication.shared.isIdleTimerDisabled = true
    }
    
    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        UIApplication.shared.isIdleTimerDisabled = false
        navigationController?.setNavigationBarHidden(false, animated: true)
    }
    
    public override var shouldAutorotate: Bool {
        return false
    }
    
    public override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        return [.portrait, .landscapeRight, .landscapeLeft]
    }
    
    public override func viewWillTransition(to size: CGSize, with coordinator: any UIViewControllerTransitionCoordinator) {
        NotificationCenter.default.post(name: Notification.Name.TUILiveKitRotateScreenNotification, object: nil)
    }
        
    public func forceLandscapeMode() {
        if #available(iOS 16.0, *) {
            guard let scene = UIApplication.shared.connectedScenes.first as? UIWindowScene else {
                return
            }
            let preferences = UIWindowScene.GeometryPreferences.iOS(interfaceOrientations: .landscape)
            scene.requestGeometryUpdate(preferences) { error in
                debugPrint("forceLandscapeMode: \(error.localizedDescription)")
            }
        } else {
            let orientation: UIDeviceOrientation = .landscapeRight
            UIDevice.current.setValue(orientation.rawValue, forKey: "orientation")
            UIViewController.attemptRotationToDeviceOrientation()
        }
        
        orientation = .landscapeRight
    }
    
    public func forcePortraitMode() {
        if #available(iOS 16.0, *) {
            guard let scene = UIApplication.shared.connectedScenes.first as? UIWindowScene else {
                return
            }
            let preferences = UIWindowScene.GeometryPreferences.iOS(interfaceOrientations: .portrait)
            scene.requestGeometryUpdate(preferences) { error in
                debugPrint("forcePortraitMode: \(error.localizedDescription)")
            }
        } else {
            let orientation: UIDeviceOrientation = .portrait
            UIDevice.current.setValue(orientation.rawValue, forKey: "orientation")
            UIViewController.attemptRotationToDeviceOrientation()
        }
        
        orientation = .portrait
    }

    @objc private func applicationWillEnterForeground() {
        if orientation == .landscapeRight {
            forceLandscapeMode()
        } else {
            forcePortraitMode()
        }
    }
}

extension TUILiveRoomAudienceViewController {
    private func constructViewHierarchy() {
        view.addSubview(audienceContainerView)
    }
    
    private func activateConstraints() {
        audienceContainerView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func registerApplicationObserver() {
        NotificationCenter.default.addObserver(self,
                                            selector: #selector(applicationWillEnterForeground),
                                            name: UIApplication.willEnterForegroundNotification,
                                            object: nil)

    }
    
    private func unregisterApplicationObserver() {
        NotificationCenter.default.removeObserver(self)

    }
}

// MARK: - AudienceEndStatisticsViewDelegate
extension TUILiveRoomAudienceViewController: AudienceEndStatisticsViewDelegate {
    func onCloseButtonClick() {
        if let nav = navigationController {
            nav.popViewController(animated: true)
        } else {
            dismiss(animated: true)
        }
    }
}

extension TUILiveRoomAudienceViewController: AudienceContainerViewDelegate {
    public func onLiveEnded(roomId: String, avatarUrl: String, userName: String) {
        let audienceEndView = AudienceEndStatisticsView(roomId: roomId, avatarUrl: avatarUrl, userName: userName)
        audienceEndView.delegate = self
        view.addSubview(audienceEndView)
        audienceEndView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    public func onClickFloatWindow() {
        FloatWindow.shared.showFloatWindow(controller: self, provider: audienceContainerView)
    }
}

extension TUILiveRoomAudienceViewController: RotateScreenDelegate {
    public func rotateScreen(isPortrait: Bool) {
        if isPortrait  {
            forcePortraitMode()
        } else {
            forceLandscapeMode()
        }
    }
}

extension Notification.Name {
    static let TUILiveKitRotateScreenNotification = Notification.Name("TUILiveKitRotateScreenNotification")
}
