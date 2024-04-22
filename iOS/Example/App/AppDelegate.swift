//
//  AppDelegate.swift
//  TUILiveKitApp
//
//  Created by WesleyLei on 2023/10/11.
//

import UIKit
import TUILiveKit
import TUICore
import RTCRoomEngine

@main
class AppDelegate: UIResponder, UIApplicationDelegate {
    func application(_ application: UIApplication, 
                     didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        return true
    }

    // MARK: UISceneSession Lifecycle

    func application(_ application: UIApplication,
                     configurationForConnecting connectingSceneSession: UISceneSession,
                     options: UIScene.ConnectionOptions) -> UISceneConfiguration {
        // Called when a new scene session is being created.
        // Use this method to select a configuration to create the new scene with.
        return UISceneConfiguration(name: "Default Configuration", sessionRole: connectingSceneSession.role)
    }

    func application(_ application: UIApplication, didDiscardSceneSessions sceneSessions: Set<UISceneSession>) {
    }
    
    func showMainViewController() {
        let mainViewController = MainViewController()
        let rootVC = AppNavigationController(rootViewController: mainViewController)

        if let keyWindow = SceneDelegate.getCurrentWindow() {
            keyWindow.rootViewController = rootVC
            keyWindow.makeKeyAndVisible()
        } else {
            debugPrint("window show MainViewController error")
        }
    }
    
    func showLoginViewController() {
        let loginVC = LoginViewController()
        let nav = AppNavigationController(rootViewController: loginVC)
        if let keyWindow = SceneDelegate.getCurrentWindow() {
            keyWindow.rootViewController = nav
            keyWindow.makeKeyAndVisible()
        }
        else {
            debugPrint("window error")
        }
    }

    
    
}

