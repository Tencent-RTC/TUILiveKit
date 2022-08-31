//
//  HUDHelper.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/21.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
import Toast_Swift
import SwiftUI

class HUDHelper: NSObject {
    private var syncHUD: NSObject?
    class func alert(msg: String?) {
        UIApplication.shared.windows.first?.rootViewController?.view .makeToast(msg)
        
    }
    
    class func alert(msg: String?, cancel: String?) {
        UIApplication.shared.windows.first?.rootViewController?.view .makeToast(msg)
    }
    
    class func alertTitle(title: String, message msg: String, cancel: String) {
        guard let window = UIApplication.shared.windows.first else { return }
        guard let rootViewController = window.rootViewController else { return }
        UIApplication.shared.windows.first?.rootViewController?.view .makeToast(msg, point: (rootViewController.view.center) , title: title, image: nil, completion: { didTap in
            
        })
    }
    
    
}

