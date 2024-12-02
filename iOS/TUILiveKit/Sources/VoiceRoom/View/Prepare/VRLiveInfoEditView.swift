//
//  VRLiveInfoEditView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/18.
//

import Kingfisher
import SnapKit
import TUICore
import Combine
import RTCCommon

class VRLiveInfoEditView: UIView {
    private let manager: VoiceRoomManager
    private let routerManager: VRRouterManager
    private var cancellableSet = Set<AnyCancellable>()

    lazy var categorySelectionModel: VRPrepareSelectionModel = {
        let model = VRPrepareSelectionModel()
        model.leftIcon = .liveBundleImage("live_category_icon")
        model.midText = .localizedReplace(.categoryText, replace: manager.roomState.liveExtraInfo.category.getString())
        model.rightIcon = .liveBundleImage("live_selection_arrow_icon")
        return model
    }()

    lazy var modeSelectionModel: VRPrepareSelectionModel = {
        let model = VRPrepareSelectionModel()
        model.leftIcon = .liveBundleImage("live_mode_icon")
        model.midText = .localizedReplace(.modeText, replace: manager.roomState.liveExtraInfo.liveMode.getString())
        model.rightIcon = .liveBundleImage("live_selection_arrow_icon")
        return model
    }()

    private lazy var coverButtonView: UIButton = {
        let view = UIButton()
        view.backgroundColor = .g5
        view.showsTouchWhenHighlighted = false
        view.layer.cornerRadius = 8
        view.layer.masksToBounds = true
        view.addTarget(self, action: #selector(coverButtonClick), for: .touchUpInside)
        view.kf.setImage(with: URL(string: manager.roomState.coverURL), for: .normal, placeholder: UIImage.placeholderImage)
        let label = UILabel(frame: .zero)
        label.backgroundColor = .pureBlackColor.withAlphaComponent(0.5)
        label.font = .customFont(ofSize: 14)
        label.textAlignment = .center
        label.textColor = .g7
        label.text = .editCoverTitle
        view.addSubview(label)
        label.snp.makeConstraints { make in
            make.width.equalTo(view)
            make.height.equalTo(22.scale375())
            make.left.equalToSuperview()
            make.bottom.equalToSuperview()
        }
        return view
    }()

    private lazy var inputTextField: UITextField = {
        let view = UITextField(frame: .zero)
        view.returnKeyType = .done
        view.font = UIFont(name: "PingFangSC-Regular", size: 18)
        view.textColor = .white
        let attributes = [NSAttributedString.Key.font: UIFont.customFont(ofSize: 16),
                          NSAttributedString.Key.foregroundColor: UIColor.tui_color(withHex: "BBBBBB"),]
        view.attributedPlaceholder = NSAttributedString(string: .editPlaceholderText,
                                                        attributes: attributes)
        view.delegate = self
        return view
    }()

    private lazy var inputBackgroundView: UIView = {
        let view = UIView()
        let editIcon = UIButton()
        editIcon.addTarget(self, action: #selector(editIconClick), for: .touchUpInside)
        editIcon.setBackgroundImage(.liveBundleImage("live_edit_icon"), for: .normal)
        view.addSubview(editIcon)
        editIcon.snp.makeConstraints { make in
            make.trailing.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.equalTo(16.scale375())
            make.height.equalTo(16.scale375())
        }

        view.addSubview(inputTextField)
        inputTextField.snp.makeConstraints { make in
            make.leading.equalToSuperview()
            make.trailing.equalTo(editIcon.snp.leading).offset(-8)
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
        }

        let lineView = UIView()
        lineView.backgroundColor = .flowKitWhite.withAlphaComponent(0.3)
        view.addSubview(lineView)
        lineView.snp.makeConstraints { make in
            make.leading.equalToSuperview()
            make.trailing.equalToSuperview()
            make.height.equalTo(1)
            make.top.equalTo(inputTextField.snp.bottom)
        }

        return view
    }()

    private lazy var categoryBackgroundView: VRPrepareSelectionButton = {
        let view = VRPrepareSelectionButton(model: categorySelectionModel)
        view.addTarget(self, action: #selector(categorySelectionClick), for: .touchUpInside)
        return view
    }()

    private lazy var modeBackgroundView: VRPrepareSelectionButton = {
        let view = VRPrepareSelectionButton(model: modeSelectionModel)
        view.addTarget(self, action: #selector(modeSelectionClick), for: .touchUpInside)
        return view
    }()
    
    init(manager: VoiceRoomManager, routerManager: VRRouterManager) {
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeRoomState()
        initialize()
    }
    
    private func initialize() {
        let roomName = TUILogin.getNickName() ?? ""
        inputTextField.text = roomName
        manager.update(roomName: roomName)
    }
}

// MARK: Layout
extension VRLiveInfoEditView {
    func constructViewHierarchy() {
        layer.cornerRadius = 16
        layer.masksToBounds = true
        backgroundColor = .g2.withAlphaComponent(0.4)
        addSubview(coverButtonView)
        addSubview(inputBackgroundView)
        addSubview(categoryBackgroundView)
        addSubview(modeBackgroundView)
    }

    func activateConstraints() {
        coverButtonView.snp.makeConstraints { make in
            make.width.equalTo(72.scale375())
            make.height.equalTo(96.scale375())
            make.leading.equalToSuperview().offset(8)
            make.top.equalToSuperview().offset(8)
        }
        inputBackgroundView.snp.makeConstraints { make in
            make.leading.equalTo(coverButtonView.snp.trailing).offset(12)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalToSuperview().offset(8)
            make.height.equalTo(36.scale375())
        }

        categoryBackgroundView.snp.makeConstraints { make in
            make.leading.equalTo(coverButtonView.snp.trailing).offset(12)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalTo(inputBackgroundView.snp.bottom).offset(14)
            make.height.equalTo(20.scale375())
        }

        modeBackgroundView.snp.makeConstraints { make in
            make.leading.equalTo(categoryBackgroundView.snp.leading)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalTo(categoryBackgroundView.snp.bottom).offset(8)
            make.height.equalTo(20.scale375())
        }
    }
}

// MARK: Action
extension VRLiveInfoEditView {
    @objc func coverButtonClick() {
        inputTextField.resignFirstResponder()
        routerManager.router(action: .present(.systemImageSelection(.cover)))
    }

    @objc func editIconClick() {
        inputTextField.becomeFirstResponder()
    }

    @objc func categorySelectionClick() {
        inputTextField.resignFirstResponder()
        showCategorySelection()
    }

    @objc func modeSelectionClick() {
        inputTextField.resignFirstResponder()
        showModeSelection()
    }
    
    private func showCategorySelection() {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig()
        var config: ActionItemDesignConfig
        for category in LiveStreamCategory.allCases {
            if category == .music {
                config = ActionItemDesignConfig(lineWidth: 7)
            } else {
                config = designConfig
            }
            let item = ActionItem(title: category.getString(), designConfig: config, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                self.manager.update(roomCategory: category)
                self.routerManager.router(action: .dismiss())
            })
            items.append(item)
        }
        var panelData = ActionPanelData(items: items)
        panelData.containCancel = false
        routerManager.router(action: .present(.listMenu(panelData)))
    }

    private func showModeSelection() {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig()
        var config: ActionItemDesignConfig
        for mode in LiveStreamPrivacyStatus.allCases {
            if mode == .privacy {
                config = ActionItemDesignConfig(lineWidth: 7)
            } else {
                config = designConfig
            }
            let item = ActionItem(title: mode.getString(), designConfig: config, actionClosure: { [weak self] value in
                guard let self = self else { return }
                guard let privacy = LiveStreamPrivacyStatus(rawValue: value) else { return }
                self.manager.update(roomPrivacy: privacy)
                self.routerManager.router(action: .dismiss())
            })
            items.append(item)
        }
        var panelData = ActionPanelData(items: items)
        panelData.containCancel = false
        routerManager.router(action: .present(.listMenu(panelData)))
    }
}

// MARK: UITextFieldDelegate
extension VRLiveInfoEditView: UITextFieldDelegate {
    func textField(_ textField: UITextField,
                   shouldChangeCharactersIn range: NSRange,
                   replacementString string: String) -> Bool {
        let maxCount = 30
        guard let textFieldText = textField.text,
              let rangeOfTextToReplace = Range(range, in: textFieldText) else {
            return false
        }
        let substringToReplace = textFieldText[rangeOfTextToReplace]
        if substringToReplace.count > 0 && string.count == 0 {
            return true
        }
        let count = textFieldText.count - substringToReplace.count + string.count

        let res = count <= maxCount
        return res
    }

    func textFieldShouldReturn(_ textField: UITextField) -> Bool {
        textField.resignFirstResponder()
        return true
    }

    func textFieldDidChangeSelection(_ textField: UITextField) {
        manager.update(roomName: textField.text ?? "")
    }
}

// MARK: - subscribeRoomState
extension VRLiveInfoEditView {
    private func subscribeRoomState() {
        manager.subscribeRoomState(StateSelector(keyPath: \VRRoomState.coverURL))
            .receive(on: RunLoop.main)
            .sink { [weak self] url in
                guard let self = self else { return }
                self.coverButtonView.kf.setImage(with: URL(string: manager.roomState.coverURL),
                                                 for: .normal,
                                                 placeholder: UIImage.placeholderImage)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeRoomState(StateSelector(keyPath: \VRRoomState.liveExtraInfo.category))
            .receive(on: RunLoop.main)
            .sink { [weak self] category in
                guard let self = self else { return }
                let value = String.localizedReplace(.categoryText, replace: self.manager.roomState.liveExtraInfo.category.getString())
                self.categorySelectionModel.midText = value
            }
            .store(in: &cancellableSet)
        
        manager.subscribeRoomState(StateSelector(keyPath: \VRRoomState.liveExtraInfo.liveMode))
            .receive(on: RunLoop.main)
            .sink { [weak self] mode in
                guard let self = self else { return }
                let value = String.localizedReplace(.modeText, replace: self.manager.roomState.liveExtraInfo.liveMode.getString())
                self.modeSelectionModel.midText = value
            }
            .store(in: &cancellableSet)
    }
}

private extension String {
    static let editCoverTitle = localized("live.edit.cover.title")
    static let editPlaceholderText = localized("live.edit.placeholder.text")
    static let categoryText = localized("live.category.xxx")
    static let modeText = localized("live.mode.xxx")
}
