//
//  LoginViewController.swift
//  TXLiteAVDemo
//
//  Created by gg on 2021/4/7.
//  Copyright © 2021 Tencent. All rights reserved.
//

import Foundation
import WebKit
import TUICore

class LoginViewController: UIViewController {
    let UserIdKey = "UserIdKey"
    let loading = UIActivityIndicatorView()
    
    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        view.bringSubviewToFront(loading)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        view.backgroundColor = .white
        navigationController?.navigationBar.barTintColor = .white
        TUICSToastManager.setDefaultPosition(TUICSToastPositionCenter)
        view.addSubview(loading)
        loading.snp.makeConstraints { (make) in
            make.width.height.equalTo(40)
            make.centerX.centerY.equalTo(view)
        }
    }
    
    func login(userId: String) {
        loading.startAnimating()
        TUILogin.login(Int32(SDKAPPID), 
                       userID: userId,
                       userSig: GenerateTestUserSig.genTestUserSig(identifier: userId)) { [weak self] in
            guard let `self` = self else { return }
            self.loading.stopAnimating()
            
            SettingsConfig.share.userId = userId
            UserDefaults.standard.set(userId, forKey: self.UserIdKey)
            UserDefaults.standard.synchronize()
            V2TIMManager.sharedInstance()?.getUsersInfo([userId], succ: { [weak self] (infos) in
                guard let `self` = self else { return }
                if let info = infos?.first {
                    SettingsConfig.share.avatar = info.faceURL ?? TUI_LIVE_DEFAULT_AVATAR
                    SettingsConfig.share.name = info.nickName ?? ""
                }
                self.loginSucc()
            }, fail: { (code, err) in
                
            })
        } fail: { [weak self] code, errorDes in
            guard let `self` = self else { return }
            self.loading.stopAnimating()
            TUITool.makeToast("login failed, code:\(code), error: \(errorDes ?? "nil")")
        }
    }
    
    func loginSucc() {
        let appDelegate = UIApplication.shared.delegate as? AppDelegate
        
        if SettingsConfig.share.name.count == 0 {
            let vc = RegisterViewController()
            navigationController?.pushViewController(vc, animated: true)
        } else {
            self.view.makeToast(TUILiveKitAppLocalize("Logged In"))
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                appDelegate?.showMainViewController()
            }
        }
    }
    
    func showRegisterVC() {
        let vc = RegisterViewController()
        navigationController?.pushViewController(vc, animated: true)
    }
    
    override func loadView() {
        super.loadView()
        let rootView = LoginRootView()
        rootView.rootVC = self
        view = rootView
        
        let userId = UserDefaults.standard.string(forKey: UserIdKey)
        rootView.userIdTextField.text = userId
    }
}
