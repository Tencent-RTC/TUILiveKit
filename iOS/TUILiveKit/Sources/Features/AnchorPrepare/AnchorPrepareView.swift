//
//  AnchorPrepareView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/16.
//

import Foundation
import RTCCommon
import TUICore
import TUILiveComponent
import LiveStreamCore

public class AnchorPrepareView: UIView {
    public weak var delegate: AnchorPrepareViewDelegate?
    
    private let coreView: LiveCoreView
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    
    private weak var popupViewController: PopupViewController?
    
    private lazy var topGradientView: UIView = {
        var view = UIView()
        return view
    }()
    
    private lazy var bottomGradientView: UIView = {
        var view = UIView(frame: CGRect(x: 0, y: 0, width: self.mm_w, height: 300.scale375()))
        view.gradient(colors: [.g1.withAlphaComponent(0), .g1,], isVertical: true)
        return view
    }()
    
    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(internalImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()
    
    private lazy var editInfo = EditInfo(roomName: getDefaultRoomName(), coverUrl: Constants.URL.defaultCover, privacyMode: .public)
    
    private lazy var editView = LSLiveInfoEditView(editInfo: &editInfo)
    
    private lazy var defaultPanelModelItems: [PrepareFeatureItem] = {
        var designConfig = PrepareFeatureItemDesignConfig()
        designConfig.type = .imageAboveTitle
        designConfig.imageSize = CGSize(width: 36.scale375(), height: 36.scale375())
        designConfig.titleHeight = 20.scale375Height()
        var items: [PrepareFeatureItem] = []
        items.append(PrepareFeatureItem(normalTitle: .beautyText,
                                        normalImage: internalImage("live_prepare_beauty_icon"),
                                        designConfig: designConfig,
                                        actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.beautyClick()
        }))
        items.append(PrepareFeatureItem(normalTitle: .audioText,
                                        normalImage: internalImage("live_prepare_audio_icon"),
                                        designConfig: designConfig,
                                        actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.audioEffectsClick()
        }))
        items.append(PrepareFeatureItem(normalTitle: .flipText,
                                        normalImage: internalImage("live_prepare_flip_icon"),
                                        designConfig: designConfig,
                                        actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.flipClick()
        }))
        return items
    }()
    
    private lazy var currentPanelModelItems: [PrepareFeatureItem] = defaultPanelModelItems
    
    private lazy var featureClickPanel: PrepareFeatureClickPanel = {
        let model = PrepareFeatureClickPanelModel()
        model.itemSize = CGSize(width: 63.scale375(), height: 56.scale375Height())
        model.itemDiff = 25.scale375()
        model.items = currentPanelModelItems
        return PrepareFeatureClickPanel(model: model)
    }()
    
    private lazy var startButton: UIButton = {
        let view = UIButton()
        view.layer.cornerRadius = 26.scale375()
        view.setTitle(.startLivingTitle, for: .normal)
        view.titleLabel?.font = .customFont(ofSize: 20, weight: .semibold)
        view.addTarget(self, action: #selector(startButtonClick), for: .touchUpInside)
        view.backgroundColor = .b1
        return view
    }()
    
    private var isViewReady: Bool = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .black
        constructViewHierarchy()
        activateConstraints()
        setupViewStyle()
        startCameraAndMicrophone()
        isViewReady = true
    }
    
    public init(coreView: LiveCoreView) {
        self.coreView = coreView
        super.init(frame: .zero)
        registerObserver()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        unRegisterObserver()
        LiveKitLog.info("\(#file)", "\(#line)", "deinit AnchorPrepareView \(self)")
    }
    
    private func registerObserver() {
        NotificationCenter.default.addObserver(self,
                                               selector: #selector(keyboardWillShow),
                                               name: UIResponder.keyboardWillShowNotification,
                                               object: nil)
        NotificationCenter.default.addObserver(self,
                                               selector: #selector(keyboardWillHide),
                                               name: UIResponder.keyboardWillHideNotification,
                                               object: nil)
    }
    
    private func unRegisterObserver() {
        NotificationCenter.default.removeObserver(self)
    }
    
    private func setupViewStyle() {
        coreView.layer.cornerRadius = 16.scale375()
        coreView.layer.masksToBounds = true
    }
    
    private func startCameraAndMicrophone() {
        coreView.startCamera(useFrontCamera: true) {
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code.rawValue, message: message)
            makeToast(error.localizedMessage)
        }
        coreView.startMicrophone() {
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code.rawValue, message: message)
            makeToast(error.localizedMessage)
        }
    }
    
    private func getDefaultRoomName() -> String {
        let userState: UserState = coreView.getState()
        let selfInfo = userState.selfInfo
        return selfInfo.userName.isEmpty ? selfInfo.userId : selfInfo.userName
    }
    
    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        endEditing(true)
    }
    
    public func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

extension AnchorPrepareView {
    public func disableFeatureMenu(_ isDisable: Bool) {
        if isDisable {
            currentPanelModelItems = []
        } else {
            currentPanelModelItems = defaultPanelModelItems
        }
        featureClickPanel.updateFeatureItems(newItems: currentPanelModelItems)
    }
    
    public func disableMenuSwitchCameraBtn(_ isDisable: Bool) {
        if isDisable {
            currentPanelModelItems.removeAll(where: { $0.normalTitle == .flipText })
        } else {
            if let item = defaultPanelModelItems.first(where: { $0.normalTitle == .flipText }) {
                if !currentPanelModelItems.contains(where: { $0.normalTitle == .flipText }) {
                    currentPanelModelItems.append(item)
                }
                sortedPanelModelItems()
            }
        }
        featureClickPanel.updateFeatureItems(newItems: currentPanelModelItems)
    }
    
    public func disableMenuBeautyBtn(_ isDisable: Bool) {
        if isDisable {
            currentPanelModelItems.removeAll(where: { $0.normalTitle == .beautyText })
        } else {
            if let item = defaultPanelModelItems.first(where: { $0.normalTitle == .beautyText }) {
                if !currentPanelModelItems.contains(where: { $0.normalTitle == .beautyText }) {
                    currentPanelModelItems.append(item)
                }
                sortedPanelModelItems()
            }
        }
        featureClickPanel.updateFeatureItems(newItems: currentPanelModelItems)
    }
    
    public func disableMenuAudioEffectBtn(_ isDisable: Bool) {
        if isDisable {
            currentPanelModelItems.removeAll(where: { $0.normalTitle == .audioText })
        } else {
            if let item = defaultPanelModelItems.first(where: { $0.normalTitle == .audioText }) {
                if !currentPanelModelItems.contains(where: { $0.normalTitle == .audioText }) {
                    currentPanelModelItems.append(item)
                }
                sortedPanelModelItems()
            }
        }
        featureClickPanel.updateFeatureItems(newItems: currentPanelModelItems)
    }
    
    private func sortedPanelModelItems() {
        let orderTitles = defaultPanelModelItems.map { $0.normalTitle }
        currentPanelModelItems.sort { a, b in
            let aIndex = orderTitles.firstIndex(of: a.normalTitle) ?? Int.max
            let bIndex = orderTitles.firstIndex(of: b.normalTitle) ?? Int.max
            return aIndex < bIndex
        }
    }
}

extension AnchorPrepareView {
    public func setIcon(_ icon: UIImage?, for feature: Feature) {
        switch feature {
        case .beauty:
            if let item = defaultPanelModelItems.first(where: { $0.normalTitle == .beautyText }) {
                item.normalImage = icon
            }
            if let item = currentPanelModelItems.first(where: { $0.normalTitle == .beautyText }) {
                item.normalImage = icon
            }
        case .audioEffect:
            if let item = defaultPanelModelItems.first(where: { $0.normalTitle == .audioText }) {
                item.normalImage = icon
            }
            if let item = currentPanelModelItems.first(where: { $0.normalTitle == .audioText }) {
                item.normalImage = icon
            }
        case .flipCamera:
            if let item = defaultPanelModelItems.first(where: { $0.normalTitle == .flipText }) {
                item.normalImage = icon
            }
            if let item = currentPanelModelItems.first(where: { $0.normalTitle == .flipText }) {
                item.normalImage = icon
            }
        }
        featureClickPanel.updateFeatureItems(newItems: currentPanelModelItems)
    }
}

// MARK: Layout

extension AnchorPrepareView {
    func constructViewHierarchy() {
        addSubview(coreView)
        addSubview(topGradientView)
        addSubview(bottomGradientView)
        addSubview(backButton)
        addSubview(editView)
        addSubview(featureClickPanel)
        addSubview(startButton)
    }
    
    func activateConstraints() {
        coreView.snp.makeConstraints({ make in
            make.leading.trailing.equalToSuperview()
            make.top.equalToSuperview().inset(36.scale375Height())
            make.bottom.equalToSuperview().inset(96.scale375Height())
        })
        
        topGradientView.snp.remakeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview()
            make.height.equalTo((isPortrait ? 129 : 70).scale375())
            make.width.equalToSuperview()
        }
        
        bottomGradientView.snp.remakeConstraints { make in
            make.centerX.equalToSuperview()
            make.bottom.equalToSuperview()
            make.height.equalTo((isPortrait ? 300 : 160).scale375())
            make.width.equalToSuperview()
        }
        
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.topGradientView.gradient(colors: [.g1.withAlphaComponent(0.5),
                                                   .g1.withAlphaComponent(0),], isVertical: true)
            self.bottomGradientView.gradient(colors: [.g1.withAlphaComponent(0),
                                                      .g1,], isVertical: true)
        }
        
        backButton.snp.remakeConstraints { make in
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
            make.leading.equalToSuperview().inset(14)
            if self.isPortrait {
                make.top.equalToSuperview().offset(64.scale375Height())
            } else {
                make.top.equalToSuperview().offset(16)
            }
        }
        
        editView.snp.remakeConstraints { make in
            make.width.equalTo(343.scale375())
            make.height.equalTo(112.scale375())
            if self.isPortrait {
                make.centerX.equalToSuperview()
                make.top.equalToSuperview().offset(CGFloat(120.0).scale375Height())
            } else {
                make.leading.equalToSuperview().offset(16)
                make.bottom.equalTo(startButton)
            }
        }
        
        startButton.snp.remakeConstraints { make in
            make.height.equalTo(52.scale375())
            if self.isPortrait {
                make.leading.equalToSuperview().offset(15)
                make.trailing.equalToSuperview().offset(-15)
                make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 30.scale375Height())
            } else {
                make.width.equalTo(101.scale375())
                make.trailing.equalToSuperview().inset(16)
                make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 30.scale375Height())
            }
        }
        
        featureClickPanel.snp.remakeConstraints { make in
            if self.isPortrait {
                make.centerX.equalToSuperview()
                make.bottom.equalTo(startButton.snp.top).offset(-30.scale375Height())
            } else {
                make.trailing.equalTo(startButton.snp.leading).offset(-12)
                make.centerY.equalTo(startButton)
            }
        }
    }
}

// MARK: Action

extension AnchorPrepareView {
    private func switchOrientation(isPortrait: Bool) {
        if #available(iOS 16.0, *) {
            WindowUtils.getCurrentWindowViewController()?.setNeedsUpdateOfSupportedInterfaceOrientations()
            guard let scene = UIApplication.shared.connectedScenes.first as? UIWindowScene else {
                return
            }
            let orientation: UIInterfaceOrientationMask = isPortrait ? .portrait : .landscape
            let preferences = UIWindowScene.GeometryPreferences.iOS(interfaceOrientations: orientation)
            scene.requestGeometryUpdate(preferences) { error in
                debugPrint("switchOrientation: \(error.localizedDescription)")
            }
        } else {
            let orientation: UIDeviceOrientation = isPortrait ? .portrait : .landscapeRight
            UIDevice.current.setValue(orientation.rawValue, forKey: "orientation")
            UIViewController.attemptRotationToDeviceOrientation()
        }
    }
    
    @objc func backButtonClick() {
        isUserInteractionEnabled = false
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
            guard let self = self else { return }
            isUserInteractionEnabled = true
        }
        coreView.stopCamera()
        coreView.stopMicrophone()
        delegate?.prepareView(self, didClickBack: backButton)
    }
    
    @objc func startButtonClick() {
        isUserInteractionEnabled = false
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
            guard let self = self else { return }
            isUserInteractionEnabled = true
        }
        delegate?.prepareView(self, didClickStart: startButton, editInfo: editInfo)
    }
    
    @objc func keyboardWillShow(notification: Notification) {
        guard let userInfo = notification.userInfo else {
            return
        }
        guard let keyboardRect = userInfo[UIView.keyboardFrameEndUserInfoKey] as? CGRect else { return }
        guard let animationDuration = userInfo[UIView.keyboardAnimationDurationUserInfoKey] as? Double else { return }
        UIView.animate(withDuration: animationDuration) { [weak self] in
            guard let self = self else { return }
            self.updateSettingsCardConstraint(offset: keyboardRect.size.height * 0.5)
        }
    }
    
    @objc func keyboardWillHide(notification: Notification) {
        guard let userInfo = notification.userInfo else {
            return
        }
        guard let animationDuration = userInfo[UIView.keyboardAnimationDurationUserInfoKey] as? Double else { return }
        UIView.animate(withDuration: animationDuration) { [weak self] in
            guard let self = self else { return }
            self.updateSettingsCardConstraint(offset: 0)
        }
    }
    
    func updateSettingsCardConstraint(offset: CGFloat) {
        editView.snp.updateConstraints { make in
            if self.isPortrait {
                make.top.equalToSuperview().offset(CGFloat(120.0).scale375Height())
            } else {
                make.leading.equalToSuperview().offset(16)
                if offset > 0 {
                    make.bottom.equalTo(startButton).offset(-offset)
                } else {
                    make.bottom.equalTo(startButton)
                }
            }
        }
    }
    
    private func beautyClick() {
        if BeautyView.checkIsNeedDownloadResource() {
            return
        }
        let beautyView = BeautyView.shared()
        beautyView.backClosure = { [weak self] in
            guard let self = self else { return }
            self.popupViewController?.dismiss(animated: true)
        }
        let safeBottomViewBackgroundColor = UIColor.bgOperateColor
        let menuContainerView = MenuContainerView(contentView: beautyView, safeBottomViewBackgroundColor: safeBottomViewBackgroundColor)
        let popupViewController = PopupViewController(contentView: menuContainerView, supportBlurView: false)
        menuContainerView.blackAreaClickClosure = { [weak self] in
            guard let self = self else { return }
            self.popupViewController?.dismiss(animated: true)
        }
        guard let presentingViewController = getCurrentViewController() else { return }
        presentingViewController.present(popupViewController, animated: true)
        self.popupViewController = popupViewController
        
        let isEffectBeauty = (TUICore.getService(TUICore_TEBeautyService) != nil)
        DataReporter.reportEventData(eventKey: isEffectBeauty ? Constants.DataReport.kDataReportPanelShowLiveRoomBeautyEffect :
                                        Constants.DataReport.kDataReportPanelShowLiveRoomBeauty)
    }
    
    private func audioEffectsClick() {
        let audioEffect = AudioEffectView()
        audioEffect.backButtonClickClosure = { [weak self] _ in
            guard let self = self else { return }
            self.popupViewController?.dismiss(animated: true)
        }
        let menuContainerView = MenuContainerView(contentView: audioEffect, safeBottomViewBackgroundColor: .g2)
        let popupViewController = PopupViewController(contentView: menuContainerView, supportBlurView: true)
        menuContainerView.blackAreaClickClosure = { [weak self] in
            guard let self = self else { return }
            self.popupViewController?.dismiss(animated: true)
        }
        guard let presentingViewController = getCurrentViewController() else { return }
        presentingViewController.present(popupViewController, animated: true)
        self.popupViewController = popupViewController
        
    }
    
    private func flipClick() {
        let mediaState: MediaState = coreView.getState()
        coreView.switchCamera(isFront: !mediaState.isFrontCamera)
    }
}

private extension String {
    static let startLivingTitle: String = internalLocalized("Start Live")
    static let beautyText: String = internalLocalized("Beauty")
    static let audioText: String = internalLocalized("Audio")
    static let flipText: String = internalLocalized("Flip")
}
