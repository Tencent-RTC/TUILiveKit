//
//  UIImage+Extension.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation
import UIKit

extension UIImage {
    static func barrageBundleImage(_ named: String) -> UIImage? {
        return UIImage(named: named, in: Bundle.barrageBundle, compatibleWith: nil) ?? UIImage(named: named)
    }
    static var placeholderImage: UIImage {
        UIColor.lightPurpleColor.trans2Image()
    }
    static var avatarPlaceholderImage: UIImage? {
        UIImage(named: "live_seat_placeholder_avatar", in: .barrageBundle, compatibleWith: nil)
    }
}


