//
//  RoomSettingCardView.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/7.
//

import UIKit

class SelectionView: UIControl {
    
    var textLeftOffset: CGFloat = 8.0
    var text: String = "" {
        didSet {
            self.titleLabel.text = text
            self.titleLabel.sizeToFit()
            self.layoutSubviews()
        }
    }
    
    var leftIcon: UIImage? = nil {
        didSet {
            leftIconImageView.image = leftIcon
        }
    }
    
    var rightIcon: UIImage? = nil {
        didSet {
            rightIconImageView.image = rightIcon
        }
    }
    
    private let leftIconImageView: UIImageView = {
        let view = UIImageView()
        return view
    }()
    
    private let titleLabel: UILabel = {
        let view = UILabel(frame: .zero)
        view.font = .customFont(ofSize: 14)
        view.textColor = .g7
        view.sizeToFit()
        return view
    }()
    
    private var rightIconImageView: UIImageView = {
        let view = UIImageView()
        return view
    }()
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    func constructViewHierarchy() {
        addSubview(leftIconImageView)
        addSubview(titleLabel)
        addSubview(rightIconImageView)
    }
    
    func activateConstraints() {
        leftIconImageView.snp.remakeConstraints { make in
            make.leading.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.equalTo(16.scale375())
            make.height.equalTo(16.scale375())
        }
        titleLabel.snp.remakeConstraints { make in
            make.leading.equalTo(leftIconImageView.snp.trailing).offset(self.textLeftOffset)
            make.centerY.equalToSuperview()
        }
        rightIconImageView.snp.remakeConstraints { make in
            make.leading.equalTo(titleLabel.snp.trailing)
            make.centerY.equalToSuperview()
            make.width.equalTo(20.scale375())
            make.height.equalTo(20.scale375())
        }
    }
}

protocol RoomSettingCardViewDelegate: AnyObject {
    func settingCardView(_ view: RoomSettingCardView, didTapSelectedCover button: UIButton)
    func settingCardView(_ view: RoomSettingCardView, didChangeRoom name: String)
    func settingCardView(_ view: RoomSettingCardView, didTapCategory sectionView: SelectionView)
    func settingCardView(_ view: RoomSettingCardView, didTapMode sectionView: SelectionView)
}

class RoomSettingCardView: UIView {
    
    private var isViewReady: Bool = false
    weak var delegate: RoomSettingCardViewDelegate?
    
    let coverContainer: UIView = {
        let view = UIView(frame: .zero)
        view.layer.cornerRadius = 10.0
        view.layer.masksToBounds = true
        return view
    }()
    
    let coverButton: UIButton = {
        let button = UIButton(type: .custom)
        button.layer.cornerRadius = 10.0
        button.layer.masksToBounds = true
        button.titleLabel?.font = .customFont(ofSize: 14)
        return button
    }()
    
    let coverLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 14)
        label.textColor = .white
        label.text = .editCoverText
        return label
    }()
    
    let inputContainerView: UIView = {
        let view = UIView(frame: .zero)
        return view
    }()
    
    let roomNameTextField: UITextField = {
        let textField = UITextField(frame: .zero)
        textField.returnKeyType = .done
        textField.font = .customFont(ofSize: 18)
        textField.textColor = .white
        let attributes = [
            NSAttributedString.Key.font: UIFont.customFont(ofSize: 16),
            NSAttributedString.Key.foregroundColor: UIColor(0xBBBBBB),
        ]
        textField.attributedPlaceholder = NSAttributedString(string: .editPlaceholderText)
        let image = UIImage.liveBundleImage("live_edit_icon")
        let imageView = UIImageView(image: image)
        textField.rightView = imageView
        textField.rightViewMode = .always
        return textField
    }()
    
    let divider: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .flowKitWhite.withAlphaComponent(0.3)
        return view
    }()
    
    let categoryView: SelectionView = {
        let view = SelectionView(frame: .zero)
        view.rightIcon = .liveBundleImage("live_selection_arrow_icon")
        view.leftIcon = .liveBundleImage("live_category_icon")
        return view
    }()
    
    let livingModeView: SelectionView = {
        let view = SelectionView(frame: .zero)
        view.rightIcon = .liveBundleImage("live_selection_arrow_icon")
        view.leftIcon = .liveBundleImage("live_mode_icon")
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
        addSubview(coverContainer)
        coverContainer.addSubview(coverButton)
        coverContainer.addSubview(coverLabel)
        addSubview(roomNameTextField)
        addSubview(divider)
        addSubview(categoryView)
        addSubview(livingModeView)
    }
    
    func activateConstraints() {
        activateConstraintsCoverView()
        activateConstraintsRightViews()
    }
    
    private func activateConstraintsCoverView() {
        coverContainer.snp.makeConstraints { make in
            make.width.equalTo(72.scale375())
            make.height.equalTo(96.scale375())
            make.leading.equalToSuperview().offset(8)
            make.top.equalToSuperview().offset(8)
        }
        coverButton.snp.makeConstraints { make in
            make.leading.bottom.trailing.top.equalToSuperview()
        }
        coverLabel.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.bottom.equalToSuperview()
            make.height.equalTo(22.scale375())
        }
    }
    
    private func activateConstraintsRightViews() {
        roomNameTextField.snp.makeConstraints { make in
            make.leading.equalTo(coverContainer.snp.trailing).offset(12)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalToSuperview().offset(8)
            make.height.equalTo(36.scale375())
        }
        divider.snp.makeConstraints { make in
            make.leading.trailing.equalTo(roomNameTextField)
            make.top.equalTo(roomNameTextField.snp.bottom)
            make.height.equalTo(1)
        }
        categoryView.snp.makeConstraints { make in
            make.leading.equalTo(coverContainer.snp.trailing).offset(12)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalTo(divider.snp.bottom).offset(14)
            make.height.equalTo(20.scale375())
        }
        livingModeView.snp.makeConstraints { make in
            make.leading.equalTo(categoryView.snp.leading)
            make.trailing.equalToSuperview().inset(12)
            make.top.equalTo(categoryView.snp.bottom).offset(8)
            make.height.equalTo(20.scale375())
        }
    }
    
    func bindInteraction() {
        roomNameTextField.delegate = self
        coverButton.addTarget(self, action: #selector(coverButtonClick(sender:)), for: .touchUpInside)
        categoryView.addTarget(self, action: #selector(categoryViewClick(sender:)), for: .touchUpInside)
        livingModeView.addTarget(self, action: #selector(livingModeViewClick(sender:)), for: .touchUpInside)
    }
    
    func setupViewStyle() {
        layer.cornerRadius = 16.0
        backgroundColor = .g2.withAlphaComponent(0.4)
    }
}

extension RoomSettingCardView {
    @objc
    func coverButtonClick(sender: UIButton) {
        self.delegate?.settingCardView(self, didTapSelectedCover: sender)
    }
    
    @objc
    func categoryViewClick(sender: SelectionView) {
        self.delegate?.settingCardView(self, didTapCategory: sender)
    }
    
    @objc
    func livingModeViewClick(sender: SelectionView) {
        self.delegate?.settingCardView(self, didTapMode: sender)
    }
}

// MARK: - UITextFieldDelgate
extension RoomSettingCardView: UITextFieldDelegate {
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
        guard let name = textField.text else { return }
        self.delegate?.settingCardView(self, didChangeRoom: name)
    }
}

private extension String {
    static let editCoverText: String = localized("live.edit.cover.title")
    static let editPlaceholderText: String = localized("live.edit.placeholder.text")
}
