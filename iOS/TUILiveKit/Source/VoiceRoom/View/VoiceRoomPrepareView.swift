//
//  VoiceRoomPrepareView.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/7.
//
import UIKit
import RTCRoomEngine
import Combine

protocol VoiceRoomPrepareViewDelegate: AnyObject {
    func prepareView(_ view: VoiceRoomPrepareView, didClickStart button: UIButton)
}

class VoiceRoomPrepareView: UIView {
    
    private var isViewReady: Bool = false
    private var cancellableSet = Set<AnyCancellable>()
    
    @Injected var store: VoiceRoomStoreProvider
    
    private lazy var enterRoomStatePublisher = self.store.select(RoomSelectors.getEnterRoomState)
    private lazy var roomNamePublisher = self.store.select(RoomSelectors.getRoomName)
    private lazy var roomCoverUrlPublisher = self.store.select(RoomSelectors.getRoomCoverUrl)
    private lazy var roomCategoryPublisher = self.store.select(RoomSelectors.getCategory)
    private lazy var roomModePublisher = self.store.select(RoomSelectors.getMode)
    
    weak var delegate: VoiceRoomPrepareViewDelegate?
    
    let effectView: UIVisualEffectView = {
        let blur = UIBlurEffect(style: .light)
        let view = UIVisualEffectView(effect: blur)
        return view
    }()
    
    private let topGradientView: UIView = {
        var view = UIView()
        return view
    }()
    
    let backButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        return button
    }()
    
    let cardView: RoomSettingCardView = {
        let view = RoomSettingCardView(frame: .zero)
        return view
    }()
    
    let startButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setTitle(.startText, for: .normal)
        button.layer.cornerRadius = 10.0
        button.layer.masksToBounds = true
        button.titleLabel?.font = .customFont(ofSize: 16)
        button.setBackgroundImage(UIColor.brandBlueColor.trans2Image(), for: .normal)
        return button
    }()
    
    private lazy var bottomGradientView: UIView = {
        var view = UIView(frame: .zero)
        return view
    }()

    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupViewStyle()
        isViewReady = true
    }
    
    func constructViewHierarchy() {
        addSubview(effectView)
        addSubview(backButton)
        addSubview(cardView)
        addSubview(startButton)
    }
    
    func activateConstraints() {
        effectView.snp.makeConstraints { make in
            make.top.leading.bottom.trailing.equalToSuperview()
        }
        backButton.snp.remakeConstraints { make in
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
            make.leading.equalToSuperview().inset(14)
            make.top.equalToSuperview().offset(64.scale375Height())
        }
        cardView.snp.makeConstraints { make in
            make.width.equalTo(343.scale375())
            make.height.equalTo(112.scale375())
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(CGFloat(120.0).scale375Height())
        }
        startButton.snp.makeConstraints { make in
            make.height.equalTo(52.scale375())
            make.leading.equalToSuperview().offset(15)
            make.trailing.equalToSuperview().offset(-15)
            make.bottom.equalTo(safeAreaLayoutGuide.snp.bottom).offset(-12)
        }
    }
    
    func bindInteraction() {
        backButton.addTarget(self, action: #selector(clickBack(sender:)), for: .touchUpInside)
        startButton.addTarget(self, action: #selector(clickStart(sender:)), for: .touchUpInside)
        cardView.delegate = self
        subscribeCardViewState()
        subscribeEnterRoomState()
    }
    
    func setupViewStyle() {
        bottomGradientView.gradient(colors: [.g1.withAlphaComponent(0), .g1,], isVertical: true)
        topGradientView.gradient(colors: [.g1.withAlphaComponent(0.5),
                                          .g1.withAlphaComponent(0),], isVertical: true)
    }
    
    private func subscribeEnterRoomState() {
        enterRoomStatePublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] state in
                guard let self = self else { return }
                self.isHidden = state == .inRoom
            }
            .store(in: &cancellableSet)
    }
}

// MARK: - subscribe view state.
extension VoiceRoomPrepareView {
    private func subscribeCardViewState() {
        roomNamePublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] value in
                guard let self = self else { return }
                self.cardView.roomNameTextField.text = value
            }
            .store(in: &cancellableSet)
        roomCoverUrlPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] url in
                guard let self = self else { return }
                self.cardView.coverButton.kf.setImage(with: url, for: .normal)
            }
            .store(in: &cancellableSet)
        roomCategoryPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] category in
                guard let self = self else { return }
                let value = String.localizedReplace(.categoryText, replace: category)
                self.cardView.categoryView.text = value
            }
            .store(in: &cancellableSet)
        roomModePublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] mode in
                guard let self = self else { return }
                let value = String.localizedReplace(.modeText, replace: mode)
                self.cardView.livingModeView.text = value
            }
            .store(in: &cancellableSet)
    }
}

extension VoiceRoomPrepareView {
    @objc
    func clickBack(sender: UIButton) {
        store.dispatch(action: NavigatorActions.navigatorTo(payload:.exit))
    }
    
    @objc
    func clickStart(sender: UIButton) {
        self.delegate?.prepareView(self, didClickStart: sender)
    }
}

extension VoiceRoomPrepareView: RoomSettingCardViewDelegate {
    func settingCardView(_ view: RoomSettingCardView, didTapCategory sectionView: SelectionView) {
        let menus = PrepareMenuDataCreator().generateCategorySelectionData()
        store.dispatch(action: NavigatorActions.navigatorTo(payload: .listMenu(menus)))
    }
    
    func settingCardView(_ view: RoomSettingCardView, didTapMode sectionView: SelectionView) {
        let menus = PrepareMenuDataCreator().generateModeSelectionData()
        store.dispatch(action: NavigatorActions.navigatorTo(payload: .listMenu(menus)))
    }
    
    func settingCardView(_ view: RoomSettingCardView, didTapSelectedCover button: UIButton) {
        // TODO: - show cover selected
    }
    
    func settingCardView(_ view: RoomSettingCardView, didChangeRoom name: String) {
        store.dispatch(action: RoomActions.updateRoomName(payload: name))
    }
}

private extension String {
    static let startText = localized("live.start.living.title")
    static let categoryText = localized("live.category.xxx")
    static let modeText = localized("live.mode.xxx")
}
