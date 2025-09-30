//
//  VoiceRoomPrepareView.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/7.
//
import UIKit
import RTCRoomEngine
import Combine
import RTCCommon
import AtomicXCore
import AtomicX
import TUICore

protocol VoiceRoomPrepareViewDelegate: AnyObject {
    func prepareView(_ view: VoiceRoomPrepareView, didClickStart button: UIButton)
    func prepareView(_ view: VoiceRoomPrepareView, didClickBack button: UIButton)
}

class VoiceRoomPrepareView: RTCBaseView {
    weak var delegate: VoiceRoomPrepareViewDelegate?
    
    private let manager: VoiceRoomManager
    private let routerManager: VRRouterManager
    private var cancellableSet = Set<AnyCancellable>()
    private let backgroundImageView: UIImageView = {
        let backgroundImageView = UIImageView(frame: .zero)
        backgroundImageView.contentMode = .scaleAspectFill
        return backgroundImageView
    }()
    
    private let backgroundGradientView: UIView = {
        var view = UIView()
        return view
    }()
    
    private let seatPreviewView: VRSeatPreviewView = {
        let view = VRSeatPreviewView(frame: .zero)
        view.itemSize = CGSize(width: 70, height: 70)
        view.verticalMargin = 0
        view.alpha = 0.3
        return view
    }()

    private lazy var ktvView: UIImageView = {
        let view = UIImageView()
        let imageName = TUIGlobalization.getPreferredLanguage() == "en" ? "live_prepare_song_en" : "live_prepare_song_zh"
        view.image = internalImage(imageName)
        view.isHidden = true
        view.isUserInteractionEnabled = false
        return view
    }()

    private let backButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setBackgroundImage(internalImage("live_back_icon"), for: .normal)
        return button
    }()
    
    private lazy var editView: VRLiveInfoEditView = {
        let view = VRLiveInfoEditView(manager: manager, routerManager: routerManager)
        return view
    }()
    
    private lazy var featureClickPanel: VRFeatureClickPanel = {
        var designConfig = VRFeatureItemDesignConfig()
        designConfig.type = .imageAboveTitle
        designConfig.imageSize = CGSize(width: 36.scale375(), height: 36.scale375())
        designConfig.titleHeight = 20.scale375Height()
        designConfig.titileColor = .g9
        let model = VRFeatureClickPanelModel()
        model.itemSize = CGSize(width: 63.scale375(), height: 56.scale375Height())
        model.itemDiff = 25.scale375()
        model.items.append(VRFeatureItem(normalTitle: .backgroundText,
                                       normalImage: internalImage("live_prepare_background_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.systemImageSelection(.background)))
        }))
        model.items.append(VRFeatureItem(normalTitle: .audioEffectsText,
                                       normalImage: internalImage("live_prepare_audio_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.audioEffect))
        }))
        model.items.append(VRFeatureItem(normalTitle: .settingText,
                                       normalImage: internalImage("live_prepare_setting_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.prepareSetting))
        }))

        model.items.append(VRFeatureItem(normalTitle: .layoutText,
                                         normalImage: internalImage("ktv_layout"),
                                         designConfig: designConfig,
                                         actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.layout))
        }))
        let featureClickPanel = VRFeatureClickPanel(model: model)
        return featureClickPanel
    }()
    
    let startButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setTitle(.startText, for: .normal)
        button.layer.cornerRadius = 26.scale375()
        button.layer.masksToBounds = true
        button.titleLabel?.font = .customFont(ofSize: 20, weight: .semibold)
        button.setBackgroundImage(UIColor.b1.trans2Image(), for: .normal)
        return button
    }()
    
    init(frame: CGRect, manager: VoiceRoomManager, routerManager: VRRouterManager) {
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: frame)
        registerObserver()
    }
    
    deinit {
        unRegisterObserver()
        debugPrint("deinit \(type(of: self))")
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
    
    override func constructViewHierarchy() {
        addSubview(backgroundImageView)
        addSubview(backgroundGradientView)
        addSubview(backButton)
        addSubview(editView)
        addSubview(featureClickPanel)
        addSubview(startButton)
        addSubview(seatPreviewView)
        addSubview(ktvView)
    }
    
    override func activateConstraints() {
        backgroundImageView.snp.makeConstraints { (make) in
            make.edges.equalToSuperview()
        }
        backgroundGradientView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        backButton.snp.remakeConstraints { make in
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
            make.leading.equalToSuperview().inset(14)
            make.top.equalToSuperview().offset(56.scale375Height())
        }
        editView.snp.makeConstraints { make in
            make.width.equalTo(343.scale375())
            make.height.equalTo(112.scale375())
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(CGFloat(96.0).scale375Height())
        }
        seatPreviewView.snp.makeConstraints { make in
            make.top.equalTo(editView.snp.bottom).offset(36.scale375())
            make.height.equalTo(seatPreviewView.getHeight())
            make.left.equalToSuperview()
            make.right.equalToSuperview()
        }
        startButton.snp.makeConstraints { make in
            make.height.equalTo(48.scale375())
            if WindowUtils.isPortrait {
                make.width.equalTo(327.scale375())
                make.centerX.equalToSuperview()
                make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 6.scale375Height())
            } else {
                make.width.equalTo(101.scale375())
                make.trailing.equalToSuperview().inset(16)
                make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 30.scale375Height())
            }
        }
        featureClickPanel.snp.remakeConstraints { make in
            if WindowUtils.isPortrait {
                make.centerX.equalToSuperview()
                make.bottom.equalTo(startButton.snp.top).offset(-36.scale375Height())
            } else {
                make.trailing.equalTo(startButton.snp.leading).offset(-12)
                make.centerY.equalTo(startButton)
            }
        }
    }
    
    override func bindInteraction() {
        backButton.addTarget(self, action: #selector(clickBack(sender:)), for: .touchUpInside)
        startButton.addTarget(self, action: #selector(clickStart(sender:)), for: .touchUpInside)
        subscribeRoomBackgroundState()
        subscribeRoomLayoutState()
    }
}

// MARK: - subscribe view state.
extension VoiceRoomPrepareView {
    private func subscribeRoomBackgroundState() {
        manager.subscribeState(StateSelector(keyPath: \VRRoomState.backgroundURL))
            .receive(on: RunLoop.main)
            .sink { [weak self] url in
                guard let self = self else { return }
                self.backgroundImageView.kf.setImage(with: URL(string: url), placeholder: UIImage.placeholderImage)
            }
            .store(in: &cancellableSet)
    }

    private func subscribeRoomLayoutState() {
        manager.subscribeState(StateSelector(keyPath: \VRRoomState.layoutType))
            .receive(on: RunLoop.main)
            .dropFirst()
            .sink { [weak self] layoutType in
                guard let self = self else { return }
                self.onSeatLayoutChanged(isKTV: layoutType == .KTVRoom)
            }
            .store(in: &cancellableSet)
    }
}

extension VoiceRoomPrepareView {
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
            if WindowUtils.isPortrait {
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

    private func onSeatLayoutChanged(isKTV: Bool) {
        if isKTV == true {
            ktvView.isHidden = false
            seatPreviewView.snp.remakeConstraints { make in
                make.top.equalTo(ktvView.snp.bottom).offset(24.scale375())
                make.height.equalTo(seatPreviewView.getHeight())
                make.left.equalToSuperview()
                make.right.equalToSuperview()
            }
            ktvView.snp.makeConstraints { make in
                make.top.equalTo(editView.snp.bottom).offset(26.scale375())
                make.width.equalTo(343.scale375())
                make.centerX.equalToSuperview()
                make.height.equalTo(120.scale375())
            }
        }else {
            ktvView.isHidden = true
            seatPreviewView.snp.remakeConstraints { make in
                make.top.equalTo(editView.snp.bottom).offset(36.scale375())
                make.height.equalTo(seatPreviewView.getHeight())
                make.left.equalToSuperview()
                make.right.equalToSuperview()
            }
        }
    }
}

extension VoiceRoomPrepareView {
    @objc
    func clickBack(sender: UIButton) {
        delegate?.prepareView(self, didClickBack: sender)
    }
    
    @objc
    func clickStart(sender: UIButton) {
        delegate?.prepareView(self, didClickStart: sender)
    }
}

private extension String {
    static let startText = internalLocalized("Start Live")
    static let backgroundText: String = internalLocalized("Background")
    static let audioEffectsText: String = internalLocalized("Audio")
    static let settingText: String = internalLocalized("Settings")
    static let layoutText: String = internalLocalized("Layout")
}
