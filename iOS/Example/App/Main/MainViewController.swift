//
//  MainViewController.swift
//  TUILiveKitApp
//
//  Created by adams on 2021/6/4.
//

import UIKit
import TUICore
import TUILiveKit

class AppNavigationController: UINavigationController {
    override init(rootViewController: UIViewController) {
        super.init(rootViewController: rootViewController)
        interactivePopGestureRecognizer?.isEnabled = false
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        guard let supportedInterfaceOrientations = 
                topViewController?.supportedInterfaceOrientations as? UIInterfaceOrientationMask
        else { return .portrait }
        return supportedInterfaceOrientations
    }
    override var shouldAutorotate: Bool {
        guard let shouldAutorotate = topViewController?.shouldAutorotate else { return false }
        return shouldAutorotate
    }
}

class MainViewController: UIViewController {
    enum ContentType {
        case liveList
        case selfInfo
    }
    private var currentContentType: ContentType = .liveList
    private let liveListVC = TUILiveListViewController()
    private let meVC = MeViewController()
    private var rootView: MainRootView = MainRootView()
    override func viewDidLoad() {
        super.viewDidLoad()
        navigationController?.navigationBar.barTintColor = .white
        self.addChild(liveListVC)
        self.addChild(meVC)
        updateNavigationView(contentType: currentContentType)
        setupContentView(contentType: currentContentType)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }
    
    override func loadView() {
        super.loadView()
        rootView.backgroundColor = UIColor(hex:"F2F5FC")
        rootView.rootVC = self
        view = rootView
    }
}

// MARK: NavigationView

extension MainViewController {
    private func updateNavigationView(contentType: ContentType) {
        let titleView = UILabel()
        titleView.text = contentType == .liveList ? .liveText : .selfInfoText
        titleView.textColor = .black
        titleView.textAlignment = .center
        titleView.font = UIFont.boldSystemFont(ofSize: 17)
        titleView.adjustsFontSizeToFitWidth = true
        let width = titleView.sizeThatFits(CGSize(width: CGFloat.greatestFiniteMagnitude,
                                                  height: CGFloat.greatestFiniteMagnitude)).width
        titleView.frame = CGRect(origin:CGPoint.zero, size:CGSize(width: width, height: 500))
        self.navigationItem.titleView = titleView
        
        let debugButton = UIButton(type: .custom)
        debugButton.setImage(UIImage(named: "debug"), for: .normal)
        debugButton.addTarget(self, action: #selector(debugButtonClick), for: .touchUpInside)
        debugButton.sizeToFit()
        let debugButtonItem = UIBarButtonItem(customView: debugButton)
        debugButtonItem.tintColor = .black
        
        
        let rightButton = UIButton()
        let image = UIImage(named: contentType == .liveList ? "help_small" : "leave_icon")
        rightButton.setImage(image, for: .normal)
        rightButton.addTarget(self, action: #selector(rightButtonClick), for: .touchUpInside)
        rightButton.sizeToFit()
        let rightButtonItem = UIBarButtonItem(customView: rightButton)
        rightButtonItem.tintColor = .black
        navigationItem.rightBarButtonItems = [rightButtonItem, debugButtonItem]
    }
}

// MARK: Action

extension MainViewController {
    func liveListButtonClick() {
        currentContentType = .liveList
        updateNavigationView(contentType: currentContentType)
        setupContentView(contentType: currentContentType)
    }
    
    func startButtonClick() {
        let liveRoomId = LiveIdentityGenerator.shared.generateId(TUILogin.getUserID() ?? "", .live)
        let voiceRoomId = LiveIdentityGenerator.shared.generateId(TUILogin.getUserID() ?? "", .voice)
        let viewController = TUILivePreviewViewController(liveRoomId: liveRoomId, voiceRoomId: voiceRoomId)
        self.navigationController?.pushViewController(viewController, animated: true)
    }
    
    func meButtonClick() {
        currentContentType = .selfInfo
        updateNavigationView(contentType: currentContentType)
        setupContentView(contentType: currentContentType)
    }
    
    private func setupContentView(contentType: ContentType) {
        rootView.contentView.subviews.forEach { $0.removeFromSuperview() }
       
        let vc = contentType == .liveList ? liveListVC : meVC
        rootView.contentView.addSubview(vc.view)
        vc.view.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
        vc.view.backgroundColor = .clear
        rootView.liveListButton.isSelected = contentType == .liveList
        rootView.meButton.isSelected = contentType == .selfInfo
    }
    
    @objc func debugButtonClick() {
        let debugVC = SandBoxFileBrowserViewController(bathPath: NSHomeDirectory())
        navigationController?.pushViewController(debugVC, animated: true)
    }
    
    @objc func rightButtonClick() {
        if currentContentType == .liveList {
            jumpToDocument()
        } else if currentContentType == .selfInfo {
            goBackToLogin()
        }
    }
    
    private func jumpToDocument() {
        if let url = URL(string: "https://cloud.tencent.com/document/product/647/105441") {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }
    
    private func goBackToLogin() {
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

// MARK: Localized String

private extension String {
    static let liveText = TUILiveKitAppLocalize("TUILiveKitApp.Main.Live")
    static let selfInfoText = TUILiveKitAppLocalize("TUILiveKitApp.Main.SelfInfo")
}
