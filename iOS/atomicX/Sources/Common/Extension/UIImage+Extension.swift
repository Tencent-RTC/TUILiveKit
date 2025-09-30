//
//  UIImage+Extension.swift
//  Pods
//
//  Created by ssc on 2025/8/29.
//

public extension UIImage {
    static func atomicXBundleImage(named: String) -> UIImage? {
        return UIImage(named: named, in: atomicXBundle, compatibleWith: nil) ?? UIImage(named: named)
    }
    static var placeholderImage: UIImage {
        UIColor.lightPurpleColor.trans2Image()
    }
    static var avatarPlaceholderImage: UIImage? {
        UIImage(named: "live_seat_placeholder_avatar", in: atomicXBundle, compatibleWith: nil)
    }
}
