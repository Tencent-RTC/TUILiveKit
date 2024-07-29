//
//  LiveInfoEditView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/16.
//

import Foundation
import Kingfisher
import SnapKit
import TUICore
import Combine

class LiveInfoEditView: UIView {
    private var cancellableSet = Set<AnyCancellable>()
    private let store: LiveStore
    private let routerStore: RouterStore
    
    private lazy var roomNamePublisher = self.store.select(RoomSelectors.getRoomName)
    private lazy var roomCoverUrlPublisher = self.store.select(RoomSelectors.getRoomCoverUrl)
    private lazy var roomCategoryPublisher = self.store.select(RoomSelectors.getCategory)
    private lazy var roomModePublisher = self.store.select(RoomSelectors.getLiveMode)

    lazy var categorySelectionModel: PrepareSelectionModel = {
        let model = PrepareSelectionModel()
        model.leftIcon = .liveBundleImage("live_category_icon")
        model.midText.value = .localizedReplace(.categoryText, replace: store.roomState.liveExtraInfo.category.getString())
        model.rightIcon = .liveBundleImage("live_selection_arrow_icon")
        return model
    }()

    lazy var modeSelectionModel: PrepareSelectionModel = {
        let model = PrepareSelectionModel()
        model.leftIcon = .liveBundleImage("live_mode_icon")
        model.midText.value = .localizedReplace(.modeText, replace: store.roomState.liveExtraInfo.liveMode.getString())
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
        view.kf.setImage(with: URL(string: store.selectCurrent(RoomSelectors.getRoomCoverUrl)), for: .normal, placeholder: UIImage.placeholderImage)
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
        view.text = store.roomState.roomName
        view.delegate = self
        return view
    }()

    private lazy var inputBgView: UIView = {
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

    private lazy var categoryBgView: PrepareSelectionButton = {
        let view = PrepareSelectionButton(model: categorySelectionModel)
        view.addTarget(self, action: #selector(categorySelectionClick), for: .touchUpInside)
        return view
    }()

    private lazy var modeBgView: PrepareSelectionButton = {
        let view = PrepareSelectionButton(model: modeSelectionModel)
        view.addTarget(self, action: #selector(modeSelectionClick), for: .touchUpInside)
        return view
    }()
    
    init(store: LiveStore, routerStore: RouterStore) {
        self.store = store
        self.routerStore = routerStore
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
    }
}

// MARK: Layout
extension LiveInfoEditView {
    func constructViewHierarchy() {
        layer.cornerRadius = 16
        layer.masksToBounds = true
        backgroundColor = .g2.withAlphaComponent(0.4)
        addSubview(coverButtonView)
        addSubview(inputBgView)
        addSubview(categoryBgView)
        addSubview(modeBgView)
    }

    func activateConstraints() {
        coverButtonView.snp.makeConstraints { make in
            make.width.equalTo(72.scale375())
            make.height.equalTo(96.scale375())
            make.leading.equalToSuperview().offset(8)
            make.top.equalToSuperview().offset(8)
        }
        inputBgView.snp.makeConstraints { make in
            make.leading.equalTo(coverButtonView.snp.trailing).offset(12)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalToSuperview().offset(8)
            make.height.equalTo(36.scale375())
        }

        categoryBgView.snp.makeConstraints { make in
            make.leading.equalTo(coverButtonView.snp.trailing).offset(12)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalTo(inputBgView.snp.bottom).offset(14)
            make.height.equalTo(20.scale375())
        }

        modeBgView.snp.makeConstraints { make in
            make.leading.equalTo(categoryBgView.snp.leading)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalTo(categoryBgView.snp.bottom).offset(8)
            make.height.equalTo(20.scale375())
        }
    }
}

// MARK: Action
extension LiveInfoEditView {
    @objc func coverButtonClick() {
        inputTextField.resignFirstResponder()
        routerStore.router(action: RouterAction.present(.systemImageSelection(.cover)))
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
                self.store.dispatch(action: RoomActions.updateRoomCategory(payload: category))
            })
            items.append(item)
        }
        routerStore.router(action: RouterAction.present(.listMenu(items)))
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
            let item = ActionItem(title: mode.getString(), designConfig: config, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                self.store.dispatch(action: RoomActions.updateRoomMode(payload: mode))
            })
            items.append(item)
        }
        routerStore.router(action: RouterAction.present(.listMenu(items)))
    }
}

// MARK: UITextFieldDelegate
extension LiveInfoEditView: UITextFieldDelegate {
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
        self.store.dispatch(action: RoomActions.updateRoomName(payload: textField.text ?? ""))
    }
}

extension LiveInfoEditView {
    private func subscribeRoomState() {
        roomNamePublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] value in
                guard let self = self else { return }
                self.inputTextField.text = self.store.roomState.roomName
            }
            .store(in: &cancellableSet)
        roomCoverUrlPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] url in
                guard let self = self else { return }
                self.coverButtonView.kf.setImage(with: URL(string: store.selectCurrent(RoomSelectors.getRoomCoverUrl)),
                                                 for: .normal,
                                                 placeholder: UIImage.placeholderImage)
            }
            .store(in: &cancellableSet)
        roomCategoryPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] category in
                guard let self = self else { return }
                let value = String.localizedReplace(.categoryText, replace: self.store.roomState.liveExtraInfo.category.getString())
                self.categorySelectionModel.midText.value = value
            }
            .store(in: &cancellableSet)
        roomModePublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] mode in
                guard let self = self else { return }
                let value = String.localizedReplace(.modeText, replace: self.store.roomState.liveExtraInfo.liveMode.getString())
                self.modeSelectionModel.midText.value = value
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
