//
//  LiveRoomMainViewController.swift
//  TUILiveRoomApp
//
//  Created by adams on 2021/6/4.
//

import UIKit
import TUILiveRoom
import TUICore

class LiveRoomMainViewController: UIViewController {
    
    let rootView = LiveRoomMainRootView.init(frame: .zero)
    
    override func viewDidLoad() {
        super.viewDidLoad()
        title = .naviTitleText
        navigationController?.navigationBar.barTintColor = .white
        setupViewHierarchy()
        initNavigationItemTitleView()
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }

}

extension LiveRoomMainViewController {
    private func setupViewHierarchy() {
        rootView.frame = view.bounds
        rootView.backgroundColor = .white
        rootView.delegate = self
        view = rootView
    }
    
    private func initNavigationItemTitleView() {
        let titleView = UILabel()
        titleView.text = .videoInteractionText
        titleView.textColor = .black
        titleView.textAlignment = .center
        titleView.font = UIFont.boldSystemFont(ofSize: 17)
        titleView.adjustsFontSizeToFitWidth = true
        let width = titleView.sizeThatFits(CGSize(width: CGFloat.greatestFiniteMagnitude, height: CGFloat.greatestFiniteMagnitude)).width
        titleView.frame = CGRect(origin:CGPoint.zero, size:CGSize(width: width, height: 500))
        self.navigationItem.titleView = titleView
        
        let isCdnMode = ((UserDefaults.standard.object(forKey: "liveRoomConfig_useCDNFirst") as? Bool) ?? false)
        let rightCDN = UIBarButtonItem()
        if isCdnMode {
            rightCDN.title = "CDN模式"
        } else {
            rightCDN.title = ""
        }
        
        let helpBtn = UIButton(type: .custom)
        helpBtn.setImage(UIImage.init(named: "help_small"), for: .normal)
        helpBtn.addTarget(self, action: #selector(connectWeb), for: .touchUpInside)
        helpBtn.sizeToFit()
        let rightItem = UIBarButtonItem(customView: helpBtn)
        rightItem.tintColor = .black
        navigationItem.rightBarButtonItems = [rightItem, rightCDN]
        
        let backBtn = UIButton(type: .custom)
        backBtn.setImage(UIImage.init(named: "liveroom_back"), for: .normal)
        backBtn.addTarget(self, action: #selector(backBtnClick), for: .touchUpInside)
        backBtn.sizeToFit()
        let backItem = UIBarButtonItem(customView: backBtn)
        backItem.tintColor = .black
        navigationItem.leftBarButtonItem = backItem
    }
    
}

extension LiveRoomMainViewController {
    @objc func backBtnClick() {
        let alertVC = UIAlertController.init(title:
         TRTCLiveRoomLocalize("App.PortalViewController.areyousureloginout"), message: nil,
         preferredStyle: .alert)
        let cancelAction = UIAlertAction.init(title: TRTCLiveRoomLocalize("App.PortalViewController.cancel"), style: .cancel, handler: nil)
        let sureAction = UIAlertAction.init(title: TRTCLiveRoomLocalize("App.PortalViewController.determine"), style: .default) { (action) in
            ProfileManager.shared.removeLoginCache()
            AppUtils.shared.appDelegate.showLoginViewController()
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
    
    @objc func connectWeb() {
        if let url = URL(string: "https://cloud.tencent.com/document/product/647/35429") {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }
}

extension LiveRoomMainViewController: LiveRoomMainRootViewDelegate {
    
    func enterRoom(roomId: String) {
        guard let roomID = Int(roomId) else { return }
        TUILiveRoom.sharedInstance.enterRoom(roomId: roomID)
    }
    
    @objc func createRoom() {
        guard let roomId = TUILogin.getUserID() else {
            return
        }
        guard let roomID = Int(roomId) else { return }
        /// Create Room
        TUILiveRoom.sharedInstance.createRoom(roomId: roomID, roomName: "test room")
    }
    
}

extension String {
    static let naviTitleText = TRTCLiveRoomLocalize("Demo.TRTC.LiveRoom.videointeraction")
    static let videoInteractionText = TRTCLiveRoomLocalize("Demo.TRTC.LiveRoom.videointeraction")
    static let roomdoesnotexistText = TRTCLiveRoomLocalize("Demo.TRTC.LiveRoom.roomdoesnotexist")
}
