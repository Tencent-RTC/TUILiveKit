//
//  UIImage+Extension.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation
import UIKit

public extension UIImage {
    static func liveBundleImage(_ named: String) -> UIImage? {
        return UIImage(named: named, in: Bundle.liveBundle, compatibleWith: nil) ?? UIImage(named: named)
    }
    static var placeholderImage: UIImage {
        UIColor.lightPurpleColor.trans2Image()
    }
    static var avatarPlaceholderImage: UIImage? {
        UIImage(named: "live_seat_placeholder_avatar", in: .liveBundle, compatibleWith: nil)
    }
}
