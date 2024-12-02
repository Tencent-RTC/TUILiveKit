//
//  UIImage+Extension.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/18.
//

import Foundation

extension UIImage: SeatGridViewExtensionWrapper {}

extension SeatGridViewExtension where Base == UIImage {
    static func seatGridViewImage(_ named: String) -> UIImage? {
        return UIImage(named: named, in: .sg_seatGridViewBundle, compatibleWith: nil) ?? UIImage(named: named)
    }
    static var placeholderImage: UIImage {
        UIColor.lightPurpleColor.trans2Image()
    }
    static var avatarPlaceholderImage: UIImage? {
        UIImage(named: "seat_placeholder_avatar", in: .sg_seatGridViewBundle, compatibleWith: nil)
    }
}
