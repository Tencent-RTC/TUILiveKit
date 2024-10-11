//
//  MeViewController.swift
//  TUILiveKitApp
//
//  Created by krabyu on 2024/6/19.
//

import UIKit
import TUICore

class MeViewController: UIViewController {
    private var rootView: MeRootView = MeRootView()
    private let imManager: IMManager = IMManager()
    init() {
        super.init(nibName: nil, bundle: nil)
        addObserver()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        navigationController?.navigationBar.barTintColor = .white
        initView()
        updateData()
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        updateData()
        initView()
    }
    
    override func loadView() {
        super.loadView()
        rootView.backgroundColor = .white
        rootView.rootVC = self
        view = rootView
    }
    
    deinit {
        removeObserver()
    }
    
    func initView() {
        rootView.imageView.kf.setImage(with: URL(string: SettingsConfig.share.avatar))
        rootView.nameLabel.text = SettingsConfig.share.name
        
        let logoutItem = UIBarButtonItem(image: UIImage(named: "leave_icon"),
                                         style: .done,
                                         target: self,
                                         action: #selector(logout))
        logoutItem.tintColor = .black
        navigationItem.rightBarButtonItem = logoutItem
    }
    
    func updateData() {
        imManager.getUserFollowInfo(userId: TUILogin.getUserID() ?? "") {
            DispatchQueue.main.async { [weak self] in
                guard let self = self else { return }
                self.rootView.fansCountLabel.text = "\(SettingsConfig.share.fansCount)"
                self.rootView.likesCountLabel.text = "\(SettingsConfig.share.followCount)"
            }
        }
    }
    
    func nameLabelClick() {
        RenamePanel.show(in: self)
    }
}

// MARK: - Actions
extension MeViewController {
    
    @objc private func logout() {
        let alertVC = UIAlertController(title:
         TUILiveKitAppLocalize("TUILiveKitApp.Main.areyousureloginout"), message: nil,
         preferredStyle: .alert)
        let cancelAction = UIAlertAction(title: TUILiveKitAppLocalize("TUILiveKitApp.Main.cancel"),
                                         style: .cancel, handler: nil)
        let sureAction = UIAlertAction(title: TUILiveKitAppLocalize("TUILiveKitApp.Main.determine"),
                                       style: .default) { (action) in
            let appDelegate = UIApplication.shared.delegate as? AppDelegate
            appDelegate?.showLoginViewController()
            TUILogin.logout {
                debugPrint("logout success")
            } fail: { code, msg in
                debugPrint("logout error")
            }
        }
        alertVC.addAction(cancelAction)
        alertVC.addAction(sureAction)
        present(alertVC, animated: true, completion: nil)
        
    }
    
}

// MARK: Notification

extension MeViewController {
    private func addObserver() {
        NotificationCenter.default.addObserver(self,
                                               selector: #selector(nameChanged),
                                               name: NSNotification.Name("nameChanged"),
                                               object: nil)

    }
    
    private func removeObserver() {
        NotificationCenter.default.removeObserver(self)
    }
    
    @objc private func nameChanged() {
        rootView.nameLabel.text = SettingsConfig.share.name
    }
}
