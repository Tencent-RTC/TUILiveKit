//
//  AppDelegate.swift
//  TRTCLiveRoom
//
//  Created by abyyxwang on 2021/5/6.
//

import UIKit
import TUILiveRoom

@main
class AppDelegate: UIResponder, UIApplicationDelegate {

    let LICENCEURL = ""
    let LICENCEKEY = ""

    func setLicence() {
        TXLiveBase.setLicenceURL(LICENCEURL, key: LICENCEKEY)
    }

    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        // Override point for customization after application launch.
        setLicence()
        return true
    }

    // MARK: UISceneSession Lifecycle

    func application(_ application: UIApplication, configurationForConnecting connectingSceneSession: UISceneSession, options: UIScene.ConnectionOptions) -> UISceneConfiguration {
        // Called when a new scene session is being created.
        // Use this method to select a configuration to create the new scene with.
        return UISceneConfiguration(name: "Default Configuration", sessionRole: connectingSceneSession.role)
    }

    func application(_ application: UIApplication, didDiscardSceneSessions sceneSessions: Set<UISceneSession>) {
        // Called when the user discards a scene session.
        // If any sessions were discarded while the application was not running, this will be called shortly after application:didFinishLaunchingWithOptions.
        // Use this method to release any resources that were specific to the discarded scenes, as they will not return.
    }

    func showMainViewController() {
        let liveRoom = TRTCLiveRoom.shareInstance()
        guard let userId = ProfileManager.shared.curUserModel?.userId,
              let avatar = ProfileManager.shared.curUserModel?.avatar,
              let name = ProfileManager.shared.curUserModel?.name else {
            debugPrint("not login")
            return
        }
        let liveRoomConfig = TRTCLiveRoomConfig.init()
        if let useCDNFirst = UserDefaults.standard.object(forKey: "liveRoomConfig_useCDNFirst") as? Bool {
            liveRoomConfig.useCDNFirst = useCDNFirst
        }
        if let cdnPlayDomain = UserDefaults.standard.object(forKey: "liveRoomConfig_cndPlayDomain") as? String, liveRoomConfig.useCDNFirst == true {
            liveRoomConfig.cdnPlayDomain = cdnPlayDomain
        }
        
        liveRoom.login(sdkAppID: Int32(SDKAPPID), userID: userId, userSig: ProfileManager.shared.curUserSig(), config: liveRoomConfig) { (code, error) in
            print("chatSalon login code = \(code)  message = \(String(describing: error))")
        }
        liveRoom.setSelfProfile(name: name, avatarURL: avatar) { (code, message) in
            print("chatSalon setSelfProfile code = \(code)  message = \(String(describing: message))")
        }
        TUILiveRoomProfileManager.sharedManager().setProfileInfo(SDKAPPID: Int32(SDKAPPID), avatar: avatar, userId: userId, name: name)
        TUILiveRoomProfileManager.sharedManager().delegate = self
        let liveRoomVC = LiveRoomMainViewController.init(liveRoom: liveRoom)
        liveRoomVC.hidesBottomBarWhenPushed = true
        let rootVC = UINavigationController.init(rootViewController: liveRoomVC)
        if let keyWindow = SceneDelegate.getCurrentWindow() {
            keyWindow.rootViewController = rootVC
            keyWindow.makeKeyAndVisible()
        }
    }
    
    func showLoginViewController() {
        let loginVC = TRTCLoginViewController.init()
        let nav = UINavigationController(rootViewController: loginVC)
        if let keyWindow = SceneDelegate.getCurrentWindow() {
            keyWindow.rootViewController = nav
            keyWindow.makeKeyAndVisible()
        }
        else {
            debugPrint("window error")
        }
    }

    
    private func alert(roomId: String, handle: @escaping () -> Void) {
        let alertVC = UIAlertController.init(title: .promptText, message: .roomNumberisText + roomId, preferredStyle: .alert)
        let alertAction = UIAlertAction.init(title: .okText, style: .default) { _ in
            handle()
        }
        alertVC.addAction(alertAction)
        if let keyWindow = SceneDelegate.getCurrentWindow() {
            keyWindow.rootViewController?.present(alertVC, animated: true, completion: nil)
        }
    }
}

extension AppDelegate: TUILiveRoomProfileManagerDelegate {
    func liveRoomGetRoomList(success: @escaping ([String]) -> Void, failed: @escaping (Int32, String) -> Void) {
        
    }
    
    func liveRoomDestroyRoom(roomId: String, success: @escaping () -> Void, failed: @escaping (Int32, String) -> Void) {
        success()
    }
    
    func liveRoomCreateRoom(roomId: String, success: @escaping () -> Void, failed: @escaping (Int32, String) -> Void) {
        success()
//        alert(roomId: roomId) {
//            success()
//        }
    }
}

extension String {
    static let promptText = TRTCLiveRoomLocalize("Demo.TRTC.LiveRoom.prompt")
    static let okText = TRTCLiveRoomLocalize("Demo.TRTC.LiveRoom.ok")
    static let roomNumberisText = TRTCLiveRoomLocalize("Demo.TRTC.LiveRoom.roomNumberis:")
}

