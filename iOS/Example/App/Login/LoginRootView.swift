//
//  LoginRootView.swift
//  TXLiteAVDemo
//
//  Created by gg on 2021/4/7.
//  Copyright © 2021 Tencent. All rights reserved.
//

import UIKit
import SnapKit

class LoginRootView: UIView {
    
    lazy var logoContentView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = UIColor.white
        return view
    }()
    lazy var tencentCloudImage: UIImageView = {
        let imageView = UIImageView(image: UIImage(named: "tencent_cloud"))
        return imageView
    }()
    lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = UIFont.systemFont(ofSize: 32)
        label.textColor = UIColor(hex: "333333") ?? .black
        label.text = .titleText
        label.numberOfLines = 0
        return label
    }()
    
    lazy var userIdContentView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = UIColor.white
        view.layer.cornerRadius = 10
        view.layer.masksToBounds = true
        view.layer.borderWidth = 1
        view.layer.borderColor = UIColor.gray.cgColor
        return view
    }()
    lazy var userIdTextLable: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = UIFont.systemFont(ofSize: 20)
        label.textColor = UIColor.black
        label.text = "UserId"
        return label
    }()
    lazy var userIdTextField: UITextField = {
        let textField = UITextField(frame: .zero)
        textField.backgroundColor = UIColor.white
        textField.font = UIFont(name: "PingFangSC-Regular", size: 20)
        textField.textColor = UIColor(hex: "333333")
        textField.attributedPlaceholder = NSAttributedString(string: "userId")
        textField.delegate = self
        textField.keyboardType = .phonePad
        return textField
    }()
    weak var currentTextField: UITextField?
    lazy var loginBtn: UIButton = {
        let btn = UIButton(type: .system)
        btn.setTitleColor(.white, for: .normal)
        btn.setTitle(.loginText, for: .normal)
        btn.adjustsImageWhenHighlighted = false
        btn.setBackgroundImage(UIColor(hex: "006EFF")?.trans2Image(), for: .normal)
        btn.titleLabel?.font = UIFont(name: "PingFangSC-Medium", size: 20)
        btn.layer.shadowColor = UIColor(hex: "006EFF")?.cgColor ?? UIColor.blue.cgColor
        btn.layer.shadowOffset = CGSize(width: 0, height: 6)
        btn.layer.masksToBounds = true
        btn.layer.cornerRadius = 10
        return btn
    }()
        
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        
        if let current = currentTextField {
            current.resignFirstResponder()
            currentTextField = nil
        }
        UIView.animate(withDuration: 0.3) {
            self.transform = .identity
        }
    }
    
    weak var rootVC: LoginViewController?
    
    override init(frame: CGRect) {
        super.init(frame: frame)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy() // 视图层级布局
        activateConstraints() // 生成约束（此时有可能拿不到父视图正确的frame）
        bindInteraction()
    }
    
    func constructViewHierarchy() {
        addSubview(logoContentView)
        logoContentView.addSubview(tencentCloudImage)
        logoContentView.addSubview(titleLabel)
        addSubview(userIdContentView)
        userIdContentView.addSubview(userIdTextLable)
        userIdContentView.addSubview(userIdTextField)
        addSubview(loginBtn)
    }
    func activateConstraints() {
        logoContentView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(100)
            make.leading.equalToSuperview().offset(20)
            make.trailing.equalToSuperview().offset(-20)
            make.height.equalTo(100)
        }
        tencentCloudImage.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(20)
            make.height.equalTo(80)
            make.width.equalTo(80)
        }
        titleLabel.snp.makeConstraints { (make) in
            make.centerY.equalTo(tencentCloudImage.snp.centerY)
            make.leading.equalTo(tencentCloudImage.snp.trailing).offset(10)
            make.trailing.equalToSuperview().offset(-20)
        }
        
        userIdContentView.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(20)
            make.trailing.equalToSuperview().offset(-20)
            make.height.equalTo(60)
        }
        userIdTextLable.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(20)
        }
        userIdTextField.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.leading.equalTo(userIdTextLable.snp.trailing).offset(20)
            make.trailing.equalToSuperview().offset(-20)
        }
        
        loginBtn.snp.makeConstraints { (make) in
            make.top.equalTo(userIdContentView.snp.bottom).offset(40)
            make.leading.equalToSuperview().offset(20)
            make.trailing.equalToSuperview().offset(-20)
            make.height.equalTo(52)
        }
    }
    
    func bindInteraction() {
        loginBtn.addTarget(self, action: #selector(loginBtnClick), for: .touchUpInside)
    }
    
    @objc func loginBtnClick() {
        if let current = currentTextField {
            current.resignFirstResponder()
        }
        guard let userId = userIdTextField.text else {
            return
        }
        rootVC?.login(userId: userId)
    }
}

extension LoginRootView: UITextFieldDelegate {
    public func textFieldDidBeginEditing(_ textField: UITextField) {
        if let last = currentTextField {
            last.resignFirstResponder()
        }
        currentTextField = textField
        textField.becomeFirstResponder()
    }
    public func textFieldDidEndEditing(_ textField: UITextField) {
        textField.resignFirstResponder()
        currentTextField = nil
    }
    public func textFieldShouldReturn(_ textField: UITextField) -> Bool {
        textField.resignFirstResponder()
        return true
    }
    public func textField(_ textField: UITextField, 
                          shouldChangeCharactersIn range: NSRange,
                          replacementString string: String) -> Bool {
        return true
    }
}

/// MARK: - internationalization string
fileprivate extension String {
    static let titleText = TUILiveKitAppLocalize("TUILiveKitApp.Login.tencentcloud")
    static let phoneNumPlaceholderText = TUILiveKitAppLocalize("TUILiveKitApp.Login.enterphonenumber")
    static let verifyCodePlaceholderText = TUILiveKitAppLocalize("TUILiveKitApp.Login.enterverificationcode")
    static let getVerifyCodeText = TUILiveKitAppLocalize("TUILiveKitApp.Login.getverificationcode")
    static let loginText = TUILiveKitAppLocalize("TUILiveKitApp.login")
    static let tuicallkitIntroduceText = TUILiveKitAppLocalize("TUILiveKitApp.Login.Introduce")
    static let tuicallkitPlatformText = TUILiveKitAppLocalize("TUILiveKitApp.Login.Platform")
    static let buyText = TUILiveKitAppLocalize("TUILiveKitApp.Login.Purchase")
    static let accessText = TUILiveKitAppLocalize("TUILiveKitApp.Login.Access")
    static let apiText = TUILiveKitAppLocalize("TUILiveKitApp.Login.APIDocumentation")
    static let problemText = TUILiveKitAppLocalize("TUILiveKitApp.Login.Problem")
}
