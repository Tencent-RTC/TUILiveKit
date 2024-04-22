//
//  CGFloat+scale375.swift
//  TUIKitCommon
//
//  Created by krabyu on 2023/10/16.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import UIKit

public let screenWidth = min(UIScreen.main.bounds.width, UIScreen.main.bounds.height)
public let screenHeight = max(UIScreen.main.bounds.width,UIScreen.main.bounds.height)

public let deviceIsiPhoneX : Bool = {
    if UIDevice.current.userInterfaceIdiom == .pad {
        return false
    }
    let size = UIScreen.main.bounds.size
    let notchValue = Int(size.width/size.height*100)
    if notchValue == 216 || notchValue == 46 {
        return true
    }
    return false
}()

public let deviceSafeBottomHeight : CGFloat = {
    if deviceIsiPhoneX {
        return 34
    }
    else {
        return 0
    }
}()

extension CGFloat {
    
    /// 375设计图中的尺寸
    ///
    /// - Returns: 最终结果缩放结果
    public func scale375Width(exceptPad: Bool = true) -> CGFloat {
        if UIDevice.current.userInterfaceIdiom == .pad {
            return exceptPad ? self * 1.5 : self * (screenWidth / 375.00)
        }
        return self * (screenWidth / 375.00)
    }
    
    public func scale375Height(exceptPad: Bool = true) -> CGFloat {
        if UIDevice.current.userInterfaceIdiom == .pad {
            return exceptPad ? self * 1.5 : self * (screenHeight / 812.00)
        }
        return self * (screenHeight / 812.00)
    }
}

extension Int {
    /// 375设计图中的尺寸
    ///
    /// - Returns: 最终结果缩放结果
    public func scale375Width(exceptPad: Bool = true) -> CGFloat {
        return CGFloat(self).scale375Width()
    }
    
    public func scale375Height(exceptPad: Bool = true) -> CGFloat {
        return CGFloat(self).scale375Height()
    }
}
