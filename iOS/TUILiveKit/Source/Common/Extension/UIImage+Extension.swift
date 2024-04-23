//
//  UIImage+Extension.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation

extension UIImage {
    static func liveBundleImage(_ named: String) -> UIImage? {
        return UIImage(named: named, in: Bundle.liveBundle, compatibleWith: nil) ?? UIImage(named: named)
    }
    static var placeholderImage: UIImage {
        UIColor.lightPurpleColor.trans2Image()
    }
}


