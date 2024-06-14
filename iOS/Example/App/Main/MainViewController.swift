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
    
    override func viewDidLoad() {
        super.viewDidLoad()
        title = .naviTitleText
        navigationController?.navigationBar.barTintColor = .white
        initNavigationItemTitleView()
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }
    
    override func loadView() {
        super.loadView()
        let rootView = MainRootView()
        rootView.backgroundColor = .white
        rootView.rootVC = self
        view = rootView
    }
}

extension MainViewController {
    private func initNavigationItemTitleView() {
        let backBtn = UIButton(type: .custom)
        backBtn.setImage(UIImage(named: "live_back"), for: .normal)
        backBtn.addTarget(self, action: #selector(backBtnClick), for: .touchUpInside)
        backBtn.sizeToFit()
        let backItem = UIBarButtonItem(customView: backBtn)
        backItem.tintColor = .black
        navigationItem.leftBarButtonItem = backItem
        
        let titleView = UILabel()
        titleView.text = .videoInteractionText
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
        
        
        let helpBtn = UIButton(type: .custom)
        helpBtn.setImage(UIImage(named: "help_small"), for: .normal)
        helpBtn.addTarget(self, action: #selector(helpBtnClick), for: .touchUpInside)
        helpBtn.sizeToFit()
        let rightItem = UIBarButtonItem(customView: helpBtn)
        rightItem.tintColor = .black
        navigationItem.rightBarButtonItems = [rightItem,debugButtonItem]
    }
}

extension MainViewController {
    
    @objc func debugButtonClick() {
        let debugVC = SandBoxFileBrowserViewController(bathPath: NSHomeDirectory())
        navigationController?.pushViewController(debugVC, animated: true)
    }
    
    @objc func backBtnClick() {
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
    
    @objc func helpBtnClick() {
        if let url = URL(string: "https://cloud.tencent.com/document/product/647/105441") {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }
    
    func joinBtnClick(sender: UIButton, view: MainRootView, roomId: String) {
        let viewController:UIViewController
        guard let roomType = LiveIdentityGenerator.shared.getIDType(roomId) else { return }
        switch roomType {
            case .live:
                viewController = TUILiveRoomAudienceViewController(roomId: roomId)
            case .voice:
                viewController = TUIVoiceRoomViewController(roomId: roomId, behavior: .join)
        }
        self.navigationController?.pushViewController(viewController, animated: true)
    }
    
    func startBtnClick(sender: UIButton, view: MainRootView) {
        let liveRoomId = LiveIdentityGenerator.shared.generateId(TUILogin.getUserID() ?? "", .live)
        let voiceRoomId = LiveIdentityGenerator.shared.generateId(TUILogin.getUserID() ?? "", .voice)
        let viewController = TUILivePreviewViewController(liveRoomId: liveRoomId, voiceRoomId: voiceRoomId)
        self.navigationController?.pushViewController(viewController, animated: true)
    }
    
    func liveRoomListBtnClick(sender: UIButton, view: MainRootView) {
        let roomListViewController = TUIRoomListViewController()
        self.navigationController?.pushViewController(roomListViewController, animated: true)
    }
}

extension String {
    static let naviTitleText = TUILiveKitAppLocalize("TUILiveKitApp.Main.videointeraction")
    static let videoInteractionText = TUILiveKitAppLocalize("TUILiveKitApp.Main.videointeraction")
    static let streamdoesnotexistText = TUILiveKitAppLocalize("TUILiveKitApp.Main.streamdoesnotexist")
}
