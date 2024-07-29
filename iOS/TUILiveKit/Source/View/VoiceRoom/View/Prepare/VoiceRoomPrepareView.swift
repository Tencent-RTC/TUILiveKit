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

protocol VoiceRoomPrepareViewDelegate: AnyObject {
    func prepareView(_ view: VoiceRoomPrepareView, didClickStart button: UIButton)
}

class VoiceRoomPrepareView: RTCBaseView {
    
    weak var delegate: VoiceRoomPrepareViewDelegate?
    
    private var cancellableSet = Set<AnyCancellable>()
    
    let store: LiveStore
    let routerStore: RouterStore
    
    private lazy var liveStatusPublisher = store.select(ViewSelectors.getLiveStatus)
    private lazy var backgroundUrlPublisher = store.select(RoomSelectors.getRoomBackgroundUrl)
    
    private let backgroundImageView: UIImageView = {
        let backgroundImageView = UIImageView(frame: .zero)
        backgroundImageView.contentMode = .scaleAspectFill
        return backgroundImageView
    }()
    
    private let backgroundGradientView: UIView = {
        var view = UIView()
        return view
    }()
    
    private let seatListView: SeatListView = {
        let view = SeatListView(frame: .zero)
        view.itemSize = CGSize(width: 70, height: 70)
        view.verticalMargin = 0
        return view
    }()
    
    private let backButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        return button
    }()
    
    private lazy var editView: LiveInfoEditView = {
        let view = LiveInfoEditView(store: store, routerStore: routerStore)
        return view
    }()
    
    private lazy var featureClickPanel: FeatureClickPanel = {
        let designConfig = FeatureItemDesignConfig()
        designConfig.type = .imageAboveTitle
        designConfig.imageSize = CGSize(width: 36.scale375(), height: 36.scale375())
        designConfig.titleHeight = 20.scale375Height()
        designConfig.titileColor = .g9
        let model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 63.scale375(), height: 56.scale375Height())
        model.itemDiff = 25.scale375()
        model.items.append(FeatureItem(title: .musicText,
                                       image: .liveBundleImage("live_prepare_music_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] in
            guard let self = self else { return }
            self.routerStore.router(action: .present(.musicList))
        }))
        model.items.append(FeatureItem(title: .audioEffectsText,
                                       image: .liveBundleImage("live_prepare_audio_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] in
            guard let self = self else { return }
            self.routerStore.router(action: .present(.audioEffect))
        }))
        model.items.append(FeatureItem(title: .settingText,
                                       image: .liveBundleImage("live_prepare_setting_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] in
            guard let self = self else { return }
            self.routerStore.router(action: .present(.prepareSetting))
        }))
        let featureClickPanel = FeatureClickPanel(model: model)
        return featureClickPanel
    }()
    
    
    let startButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setTitle(.startText, for: .normal)
        button.layer.cornerRadius = 26.scale375()
        button.layer.masksToBounds = true
        button.titleLabel?.font = .customFont(ofSize: 20, weight: .semibold)
        button.setBackgroundImage(UIColor.brandBlueColor.trans2Image(), for: .normal)
        return button
    }()
    
    init(frame: CGRect, store: LiveStore, routerStore: RouterStore) {
        self.store = store
        self.routerStore = routerStore
        super.init(frame: frame)
        registerObserver()
    }
    
    deinit {
        unRegisterObserver()
        debugPrint("deinit \(type(of: self))")
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        backgroundGradientView.gradient(colors: [.g1, .g1.withAlphaComponent(0.5), .g1,], isVertical: true)
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
        addSubview(seatListView)
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
            make.top.equalToSuperview().offset(64.scale375Height())
        }
        editView.snp.makeConstraints { make in
            make.width.equalTo(343.scale375())
            make.height.equalTo(112.scale375())
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(CGFloat(120.0).scale375Height())
        }
        seatListView.snp.makeConstraints { make in
            make.top.equalTo(editView.snp.bottom).offset(40.scale375())
            make.height.equalTo(seatListView.getHeight())
            make.left.equalToSuperview()
            make.right.equalToSuperview()
        }
        startButton.snp.makeConstraints { make in
            make.height.equalTo(52.scale375())
            if WindowUtils.isPortrait {
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
            if WindowUtils.isPortrait {
                make.centerX.equalToSuperview()
                make.bottom.equalTo(startButton.snp.top).offset(-30.scale375Height())
            } else {
                make.trailing.equalTo(startButton.snp.leading).offset(-12)
                make.centerY.equalTo(startButton)
            }
        }
    }
    
    override func bindInteraction() {
        store.dispatch(action: ViewActions.updateLiveStatus(payload: .previewing))
        store.dispatch(action: UserActions.getSelfInfo())
        backButton.addTarget(self, action: #selector(clickBack(sender:)), for: .touchUpInside)
        startButton.addTarget(self, action: #selector(clickStart(sender:)), for: .touchUpInside)
        subscribeRoomBackgroundState()
        subscribeEnterRoomState()
    }
}

// MARK: - subscribe view state.
extension VoiceRoomPrepareView {
    private func subscribeRoomBackgroundState() {
        backgroundUrlPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] url in
                guard let self = self else { return }
                self.backgroundImageView.kf.setImage(with: URL(string: url), placeholder: UIImage.placeholderImage)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeEnterRoomState() {
        liveStatusPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                case .previewing:
                    self.isHidden = false
                case .playing, .pushing, .finished:
                    self.isHidden = true
                default:
                    break
                }
            }.store(in: &cancellableSet)
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
}

extension VoiceRoomPrepareView {
    @objc
    func clickBack(sender: UIButton) {
        routerStore.router(action: .exit)
    }
    
    @objc
    func clickStart(sender: UIButton) {
        delegate?.prepareView(self, didClickStart: sender)
    }
}

private extension String {
    static let startText = localized("live.start.living.title")
    static let categoryText = localized("live.category.xxx")
    static let modeText = localized("live.mode.xxx")
    static let backgroundText: String = localized("live.anchor.setting.background")
    static let musicText: String = localized("live.category.music")
    static let audioEffectsText: String = localized("live.anchor.setting.audio.effects")
    static let settingText: String = localized("live.anchor.setting.title")
}
