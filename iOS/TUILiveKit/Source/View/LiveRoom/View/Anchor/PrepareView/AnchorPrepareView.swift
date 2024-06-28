//
//  AnchorPrepareView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/16.
//

import Foundation
protocol AnchorPrepareViewDelegate: AnyObject {
    func prepareView(_ view: AnchorPrepareView, didClickStart button: UIButton)
}

class AnchorPrepareView: UIView {
    private let store: LiveStore
    private let routerStore: RouterStore
    weak var delegate: AnchorPrepareViewDelegate?
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    
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
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private lazy var settingsCard: LiveStreamSettingsCard = {
        var settingsCard = LiveStreamSettingsCard(store: store, routerStore: routerStore)
        return settingsCard
    }()

    private lazy var featureClickPanel: FeatureClickPanel = {
        let designConfig = FeatureItemDesignConfig()
        designConfig.type = .imageAboveTitle
        designConfig.imageSize = CGSize(width: 36.scale375(), height: 36.scale375())
        designConfig.titleHeight = 20.scale375Height()
        let model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 36.scale375(), height: 56.scale375Height())
        model.itemDiff = 44.scale375()
        model.items.append(FeatureItem(title: .beautyText,
                                       image: .liveBundleImage("live_prepare_beauty_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] in
                                           guard let self = self else { return }
                                           self.beautyClick()
                                       }))
        model.items.append(FeatureItem(title: .audioText,
                                       image: .liveBundleImage("live_prepare_audio_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] in
                                           guard let self = self else { return }
                                           self.audioEffectsClick()
                                       }))
        model.items.append(FeatureItem(title: .flipText,
                                       image: .liveBundleImage("live_prepare_flip_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] in
                                           guard let self = self else { return }
                                           self.flipClick()
                                       }))
        model.items.append(FeatureItem(title: .mirrorText,
                                       image: .liveBundleImage("live_prepare_mirror_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] in
                                           guard let self = self else { return }
                                           self.mirrorClick()
                                       }))
        let featureClickPanel = FeatureClickPanel(model: model)
        return featureClickPanel
    }()

    private lazy var startButton: UIButton = {
        let view = UIButton()
        view.layer.cornerRadius = 10.0
        view.setTitle(.startLivingTitle, for: .normal)
        view.titleLabel?.font = .customFont(ofSize: 16)
        view.addTarget(self, action: #selector(startButtonClick), for: .touchUpInside)
        view.backgroundColor = .b1
        return view
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    init(store: LiveStore, routerStore: RouterStore) {
        self.store = store
        self.routerStore = routerStore
        super.init(frame: .zero)
        registerObserver()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    deinit {
        NotificationCenter.default.removeObserver(self)
        print("deinit \(type(of: self))")
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
            make.width.equalTo((isPortrait ? 275 : 101).scale375())
            if self.isPortrait {
                make.centerX.equalToSuperview()
                make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 30.scale375Height())
            } else {
                make.trailing.equalToSuperview().inset(16)
                make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 30.scale375Height())
            }
        }

        featureClickPanel.snp.remakeConstraints { make in
            make.width.equalTo(276.scale375())
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
        WindowUtils.getCurrentWindowViewController()?.backToPreviousPage()
    }

    @objc func startButtonClick() {
        delegate?.prepareView(self, didClickStart: startButton)
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
        routerStore.router(action: .present(.beauty(false)))
    }

    private func audioEffectsClick() {
        routerStore.router(action: .present(.audioEffect))
    }

    private func mirrorClick() {
        let isMirror = !store.mediaState.isMirror
        store.dispatch(action: MediaActions.switchMirror(payload: isMirror))
    }

    private func flipClick() {
        let isFrontCamera = !store.mediaState.isFrontCamera
        store.dispatch(action: MediaActions.switchCamera(payload: isFrontCamera ? .front : .rear))
    }

    private func showMoreSettingsPanel() {
    }

    private func presentPopup(view: UIView) {
        if let vc = WindowUtils.getCurrentWindowViewController() {
            let menuContainerView = MenuContainerView(contentView: view)
            menuContainerView.blackAreaClickClosure = {
                vc.dismiss(animated: true)
            }
            let viewController = PopupViewController(contentView: menuContainerView)
            vc.present(viewController, animated: true)
        }
    }
}

private extension String {
    static let startLivingTitle: String = localized("live.start.living.title")
    static let portraitTitle: String = localized("live.portrait.title")
    static let landscapeTitle: String = localized("live.landscape.title")
    static let beautyText: String = localized("live.anchor.setting.beauty")
    static let audioText: String = localized("live.anchor.setting.audio.effects")
    static let flipText: String = localized("live.anchor.setting.flip")
    static let mirrorText: String = localized("live.anchor.setting.mirror")
}
