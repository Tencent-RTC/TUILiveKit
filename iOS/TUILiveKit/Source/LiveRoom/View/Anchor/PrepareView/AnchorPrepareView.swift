//
//  AnchorPrepareView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/16.
//

import Foundation

class AnchorPrepareView: UIView {
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
    }

    private var liveRoomInfo: LiveRoomInfo {
        engineService.liveRoomInfo
    }

    private var engineService: RoomEngineService
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
        registerObserver()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    deinit {
        NotificationCenter.default.removeObserver(self)
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

    private let topGradientView: UIView = {
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
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private lazy var settingsCard: LiveStreamSettingsCard = {
        var settingsCard = LiveStreamSettingsCard(engineService: self.engineService)
        return settingsCard
    }()

    private lazy var featureClickPanel: FeatureClickPanel = {
        let designConfig = FeatureItemDesignConfig()
        designConfig.backgroundColor = UIColor.g2.withAlphaComponent(0.4)
        designConfig.cornerRadius = 36.scale375() * 0.5
        designConfig.imageScale = 0.6
        var model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 36.scale375(), height: 36.scale375())
        model.itemDiff = 20.scale375()
        model.items.append(FeatureItem(image: .liveBundleImage("live_videoSetting_beauty"),
                                       designConfig: designConfig,
                                       action: PrepareViewActionEvent.beautyClick))
        model.items.append(FeatureItem(image: .liveBundleImage("live_setting_audio_effects"),
                                       designConfig: designConfig,
                                       action: PrepareViewActionEvent.audioEffectsClick))
        model.items.append(FeatureItem(image: .liveBundleImage("live_videoSetting_flip"),
                                       designConfig: designConfig,
                                       action: PrepareViewActionEvent.flipClick))
        model.items.append(FeatureItem(image: .liveBundleImage("live_videoSetting_mirror"),
                                       designConfig: designConfig,
                                       action: PrepareViewActionEvent.mirrorClick))
        var featureClickPanel = FeatureClickPanel(model: model)
        featureClickPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            guard let action = action as? PrepareViewActionEvent else { return }
            switch action {
            case .beautyClick:
                self?.beautyClick()
            case .audioEffectsClick:
                self?.audioEffectsClick()
            case .flipClick:
                self?.flipClick()
            case .mirrorClick:
                self?.mirrorClick()
            case .moreSettingClick:
                self?.showMoreSettingsPanel()
            default:
                break
            }
        }
        return featureClickPanel
    }()

    private lazy var startButton: UIButton = {
        let view = UIButton()
        view.layer.cornerRadius = 10
        view.layer.masksToBounds = true
        view.setTitle(.startLivingTitle, for: .normal)
        view.titleLabel?.font = .customFont(ofSize: 16)
        view.addTarget(self, action: #selector(startButtonClick), for: .touchUpInside)
        view.backgroundColor = .brandBlueColor
        return view
    }()
}

// MARK: Layout
extension AnchorPrepareView {
    func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(topGradientView)
        addSubview(bottomGradientView)
        addSubview(backButton)
        addSubview(settingsCard)
        addSubview(featureClickPanel)
        addSubview(startButton)
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }

    func activateConstraints() {
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

        DispatchQueue.main.async {
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

        settingsCard.snp.remakeConstraints { make in
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
            make.width.equalTo((isPortrait ? 260 : 101).scale375())
            if self.isPortrait {
                make.centerX.equalToSuperview()
                make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 12)
            } else {
                make.trailing.equalToSuperview().inset(16)
                make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 12)
            }
        }

        featureClickPanel.snp.remakeConstraints { make in
            make.height.equalTo(featureClickPanel.mm_h)
            make.width.equalTo(featureClickPanel.mm_w)
            if self.isPortrait {
                make.centerX.equalToSuperview()
                make.bottom.equalTo(startButton.snp.top).offset(-24.scale375Height())
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
        WindowUtils.getCurrentWindowViewController()?.backToPreviousPage()
    }

    @objc func startButtonClick() {
        engineService.changeUserLiveStatus(userLiveStatus: .pushing)
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
        settingsCard.snp.updateConstraints { make in
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
        PopupPanelController.alertView(BeautyPanel(engineService: engineService, hasRenderView: false))
    }

    private func audioEffectsClick() {
        let view = AudioEffectView(frame: .zero)
        let menuContainerView = MenuContainerView(contentView: view)
        let viewController = PopupViewController(contentView: menuContainerView)
        menuContainerView.blackAreaClickClosure = { [weak viewController] in
            viewController?.dismiss(animated: true)
        }
        view.backButtonClickClosure = { [weak viewController] _ in
            viewController?.dismiss(animated: true)
        }
        WindowUtils.getCurrentWindowViewController()?.present(viewController, animated: true)
    }

    private func mirrorClick() {
        engineService.switchMirror()
    }

    private func flipClick() {
        engineService.switchCamera()
    }

    private func showMoreSettingsPanel() {
        PopupPanelController.alertView(MoreSettingsPanel(list: liveRoomInfo.roomConfig.getListConfigModel()))
    }
}

private extension String {
    static var startLivingTitle: String {
        localized("live.start.living.title")
    }

    static var portraitTitle: String {
        localized("live.portrait.title")
    }

    static var landscapeTitle: String {
        localized("live.landscape.title")
    }
}
