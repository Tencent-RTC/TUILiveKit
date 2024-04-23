//
//  LiveStreamSettingsCard.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/16.
//

import Foundation
import Kingfisher
import SnapKit
import TUICore

class StreamSettingsCardModel {
    private var liveRoomInfo: LiveRoomInfo
    init(liveRoomInfo: LiveRoomInfo) {
        self.liveRoomInfo = liveRoomInfo
    }

    var defaultImage: UIImage? = .liveBundleImage("live_edit_info_default_cover_image")
    var name: Observable<String> {
        return liveRoomInfo.name
    }

    var coverUrl: Observable<String> {
        return liveRoomInfo.coverUrl
    }

    var roomId: Observable<String> {
        return liveRoomInfo.roomId
    }

    var category: Observable<LiveStreamCategory> {
        return liveRoomInfo.category
    }

    var liveMode: Observable<LiveMode> {
        return liveRoomInfo.liveMode
    }
}

class LiveStreamSettingsCard: UIView {
    private var liveRoomInfo:LiveRoomInfo {
          engineService.liveRoomInfo
    }
    private var settingsCardModel: StreamSettingsCardModel
    private var engineService: RoomEngineService
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        self.settingsCardModel = StreamSettingsCardModel(liveRoomInfo: engineService.liveRoomInfo)
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
    }

    lazy var categorySelectionModel: PrepareSelectionModel = {
        let model = PrepareSelectionModel()
        model.leftIcon = .liveBundleImage("live_category_icon")
        model.midText.value = .localizedReplace(.categoryText, replace: settingsCardModel.category.value.getString())
        model.rightIcon = .liveBundleImage("live_selection_arrow_icon")
        settingsCardModel.category.addObserver(self) { [weak self] _, _ in
            guard let self = self else { return }
            let value = String.localizedReplace(.categoryText, replace: self.settingsCardModel.category.value.getString())
            self.categorySelectionModel.midText.value = value
        }
        return model
    }()

    lazy var modeSelectionModel: PrepareSelectionModel = {
        let model = PrepareSelectionModel()
        model.leftIcon = .liveBundleImage("live_mode_icon")
        model.midText.value = .localizedReplace(.modeText, replace: settingsCardModel.liveMode.value.getString())
        model.rightIcon = .liveBundleImage("live_selection_arrow_icon")
        settingsCardModel.liveMode.addObserver(self) { [weak self] _, _ in
            guard let self = self else { return }
            let value = String.localizedReplace(.modeText, replace: self.settingsCardModel.liveMode.value.getString())
            self.modeSelectionModel.midText.value = value
        }
        return model
    }()

    private lazy var coverButtonView: UIButton = {
        let view = UIButton()
        view.backgroundColor = .g5
        view.showsTouchWhenHighlighted = false
        view.layer.cornerRadius = 8
        view.layer.masksToBounds = true
        view.addTarget(self, action: #selector(coverButtonClick), for: .touchUpInside)
        let updateCover = { [weak self] (view: UIButton?)->Void in
            guard let self = self else{ return}
            if let url = URL(string: self.settingsCardModel.coverUrl.value) {
                view?.kf.setImage(with: url, for: .normal, placeholder: UIImage.placeholderImage)
            } else {
                view?.setBackgroundImage(self.settingsCardModel.defaultImage, for: .normal)
            }
        }
        updateCover(view)
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

        settingsCardModel.coverUrl.addObserver(self) { [weak self] _, _ in
            updateCover(self?.coverButtonView)
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
        view.text = settingsCardModel.name.value
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
}

// MARK: Layout

extension LiveStreamSettingsCard {
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
extension LiveStreamSettingsCard {
    @objc func coverButtonClick() {
        inputTextField.resignFirstResponder()
        PopupPanelController.alertView(SystemImageSelectionPanel(engineService: engineService,
                                                                 configs: SystemImageModel.configs()))
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
        let model = ActionModel()
        let designConfig = ActionItemDesignConfig()
        var config: ActionItemDesignConfig
        for category in LiveStreamCategory.allCases {
            if category == .music {
                config = ActionItemDesignConfig(lineWidth: 7)
            } else {
                config = designConfig
            }
            let item = ActionItem(text: category.getString(), designConfig: config, action: PrepareViewActionEvent.categoryValue(category))
            model.items.append(item)
        }
        let actionPanel = ActionPanel(model: model)
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let actionType = action as? PrepareViewActionEvent , case let .categoryValue(value) = actionType {
                self?.liveRoomInfo.category.value = value
            }
        }
    }

    private func showModeSelection() {
        let model = ActionModel()
        let designConfig = ActionItemDesignConfig()
        var config: ActionItemDesignConfig
        for category in LiveMode.allCases {
            if category == .privacy {
                config = ActionItemDesignConfig(lineWidth: 7)
            } else {
                config = designConfig
            }
            let item = ActionItem(text: category.getString(), designConfig: config, action: PrepareViewActionEvent.modeValue(category))
            model.items.append(item)
        }
        
        let actionPanel = ActionPanel(model: model)
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let actionType = action as? PrepareViewActionEvent, case let .modeValue(value) = actionType {
                self?.liveRoomInfo.liveMode.value = value
            }
        }
        
    }
}

// MARK: UITextFieldDelegate

extension LiveStreamSettingsCard: UITextFieldDelegate {
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
        settingsCardModel.name.value = textField.text ?? ""
    }
}

private extension String {
    static var editCoverTitle: String {
        localized("live.edit.cover.title")
    }

    static var editPlaceholderText: String {
        localized("live.edit.placeholder.text")
    }

    static var categoryText: String {
        localized("live.category.xxx")
    }

    static var modeText: String {
        localized("live.mode.xxx")
    }
}
