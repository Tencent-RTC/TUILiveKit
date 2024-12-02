//
//  RegisterViewController.swift
//  TXLiteAVDemo
//
//  Created by gg on 2021/4/8.
//  Copyright © 2021 Tencent. All rights reserved.
//

import Foundation
import SnapKit
import UIKit
import ImSDK_Plus
import TUICore

class RegisterViewController: UIViewController {
    
    let loading = UIActivityIndicatorView(style: .large)
    
    override func viewDidLoad() {
        super.viewDidLoad()
        view.backgroundColor = .white
        TUICSToastManager.setDefaultPosition(TUICSToastPositionBottom)
        title = .titleText
        view.addSubview(loading)
        loading.snp.makeConstraints { (make) in
            make.width.height.equalTo(40)
            make.centerX.centerY.equalTo(view)
        }
    }

    func register(_ userName: String) {
        loading.startAnimating()
        let userFullInfo = V2TIMUserFullInfo()
        userFullInfo.nickName = userName
        userFullInfo.faceURL = SettingsConfig.share.avatar
        V2TIMManager.sharedInstance().setSelfInfo(userFullInfo) { [weak self] in
            guard let self = self else { return }
            SettingsConfig.share.name = userName
            self.registerSuccess()
        } fail: { code, message in
            TUITool.makeToast("login failed, code:\(code), message: \(String(describing: message))")
            self.loading.stopAnimating()
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                self.navigationController?.popViewController(animated: true)
            }
        }
    }
    
    func registerSuccess() {
        self.loading.stopAnimating()
        self.view.makeToast(.registerSuccessText)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            let appDelegate = UIApplication.shared.delegate as? AppDelegate
            appDelegate?.showMainViewController()
        }
    }
    
    override func loadView() {
        super.loadView()
        let rootView = RegisterRootView()
        rootView.rootVC = self
        view = rootView
    }
}

/// MARK: - internationalization string
fileprivate extension String {
    static let titleText = TUILiveKitAppLocalize("TUILiveKitApp.Login.register")
    static let registerSuccessText = TUILiveKitAppLocalize("TUILiveKitApp.Login.registersuccess")
}

