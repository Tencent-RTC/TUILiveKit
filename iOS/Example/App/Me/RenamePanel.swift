//
//  RenamePanel.swift
//  TUILiveKitApp
//
//  Created by krabyu on 2024/6/19.
//

import UIKit

class RenamePanel: UIViewController {
    private let loading = UIActivityIndicatorView(style: .large)
    private let imManager = IMManager()
    private lazy var backgroundView: UIVisualEffectView = {
        let effect = UIBlurEffect(style: .systemThinMaterialDark)
        let view = UIVisualEffectView(effect: effect)
        let tap = UITapGestureRecognizer(target: self, action: #selector(blankAreaTapped))
        view.addGestureRecognizer(tap)
        view.isUserInteractionEnabled = true
        return view
    }()
    
    private lazy var contentView: UIView = {
        let view = UIView()
        view.backgroundColor = .white
        view.layer.cornerRadius = 12.scale375()
        let tap = UITapGestureRecognizer(target: self, action: #selector(contentViewTapped))
        view.addGestureRecognizer(tap)
        return view
    }()
    
    private lazy var backButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage(named: "live_back"), for: .normal)
        button.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return button
    }()
    
    private let titleLabel: UILabel = {
        let label = UILabel()
        label.text = .nameText
        label.textColor = UIColor(hex: "0F1014")
        label.font = UIFont(name: "PingFangSC-Semibold", size: 16)
        label.textAlignment = .center
        return label
    }()
    
    private lazy var textField: UITextField = {
        let textField = UITextField()
        textField.layer.borderColor = UIColor(hex: "1C66E5")?.cgColor
        textField.layer.borderWidth = 1
        textField.clearButtonMode = .whileEditing
        textField.layer.cornerRadius = 8
        
        let paddingView = UIView(frame: CGRect(x: 0, y: 0, width: 16.scale375(), height: 48.scale375Height()))
        textField.leftView = paddingView
        textField.leftViewMode = .always
        return textField
    }()
    
    private lazy var saveButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = UIColor(hex: "1C66E5")
        button.setTitle(.saveText, for: .normal)
        button.setTitleColor(.white, for: .normal)
        button.titleLabel?.font = UIFont(name: "PingFangSC-Semibold", size: 16)
        button.addTarget(self, action: #selector(saveButtonClick), for: .touchUpInside)
        button.layer.cornerRadius = 24.scale375Height()
        return button
    }()
    
    override func viewDidLoad() {
        super.viewDidLoad()
        view.backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
    }
}

// MARK: Layout

extension RenamePanel {
    private func constructViewHierarchy() {
        view.addSubview(backgroundView)
        view.addSubview(contentView)
        contentView.addSubview(backButton)
        contentView.addSubview(titleLabel)
        contentView.addSubview(textField)
        contentView.addSubview(saveButton)
    }

    private func activateConstraints() {
        backgroundView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        contentView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(94.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(718.scale375Height())
        }
        backButton.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.leading.equalToSuperview().offset(20.scale375())
            make.height.width.equalTo(24.scale375())
        }
        titleLabel.snp.makeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.width.equalTo(200.scale375())
            make.height.equalTo(24.scale375Height())
        }
        textField.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalTo(335.scale375())
            make.height.equalTo(48.scale375Height())
        }
        saveButton.snp.makeConstraints { make in
            make.top.equalTo(textField.snp.bottom).offset(213.scale375Height())
            make.leading.trailing.equalToSuperview().inset(20.scale375())
            make.height.equalTo(48.scale375Height())
        }
    }
}

// MARK: Action

extension RenamePanel {
    @objc private func blankAreaTapped() {
        dismiss(animated: true)
    }
    
    @objc private func backButtonClick() {
        dismiss(animated: true)
    }

    @objc private func saveButtonClick() {
        guard let text = textField.text, !text.isEmpty else { return }
        loading.startAnimating()
        imManager.changeUserName(newName: text, onSuccess: {
            self.loading.stopAnimating()
            self.view.makeToast(.operateSuccessText)
            NotificationCenter.default.post(name: NSNotification.Name(rawValue: "nameChanged"),
                                            object: nil, userInfo: nil)
        }, onError: { code, message in
            self.loading.stopAnimating()
            debugPrint("Rename failed, code:\(code), message:\(message)")
        })
    }

    @objc private func contentViewTapped() {
        textField.resignFirstResponder()
    }
}

// MARK: Show

extension RenamePanel {
    static func show(in vc: UIViewController) {
        let renamePanel = RenamePanel()
        renamePanel.modalPresentationStyle = .overFullScreen
        renamePanel.textField.becomeFirstResponder()
        vc.present(renamePanel, animated: true)
    }
}

// MARK: Localized String

private extension String {
    static let nameText = TUILiveKitAppLocalize("Name")
    static let saveText = TUILiveKitAppLocalize("Save")
    static let operateSuccessText = TUILiveKitAppLocalize("Operate Success")
}