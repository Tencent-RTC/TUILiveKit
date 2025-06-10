//
//  UIButton+Extension.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/8/2.
//

import Foundation
import UIKit

public extension UIButton {
    func setLevel(level: Int = 0) {
        backgroundColor = getLevelBackground(level: level)
        setImage(getLevelImage(level: level), for: .normal)
        setTitle("\(level)", for: .normal)
    }
    
    private func getLevelImage(level: Int) -> UIImage? {
        if level <= 30 {
            return UIImage(named: "barrage_level1", in: internalBundle, compatibleWith: nil)
        } else if level <= 60 {
            return UIImage(named: "barrage_level2", in: internalBundle, compatibleWith: nil)
        } else if level <= 90 {
            return UIImage(named: "barrage_level3", in: internalBundle, compatibleWith: nil)
        } else {
            return UIImage(named: "barrage_level4", in: internalBundle, compatibleWith: nil)
        }
    }
    
    private func getLevelBackground(level: Int) -> UIColor {
        if level <= 30 {
            return UIColor.horizontalGradientColor(colors: [UIColor(hex: "#6CFFE5"), UIColor(hex: "#82FFE1")],
                                                   frame: CGRect(x: 0, y: 0, width: 35.scale375(), height: 14.scale375Height()))
        } else if level <= 60 {
            return UIColor.horizontalGradientColor(colors: [UIColor(hex: "#6CA7FF"), UIColor(hex: "#82B4FF")],
                                                   frame: CGRect(x: 0, y: 0, width: 35.scale375(), height: 14.scale375Height()))
        } else if level <= 90 {
            return UIColor.horizontalGradientColor(colors: [UIColor(hex: "#9B6CFF"), UIColor(hex: "#AA82FF")],
                                                   frame: CGRect(x: 0, y: 0, width: 35.scale375(), height: 14.scale375Height()))
        } else {
            return UIColor.horizontalGradientColor(colors: [UIColor(hex: "#FF6C87"), UIColor(hex: "#FF82CD")],
                                                   frame: CGRect(x: 0, y: 0, width: 35.scale375(), height: 14.scale375Height()))
        }
    }
}
