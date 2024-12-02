//
//  UIFont+Extension.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/28.
//

extension UIFont: SeatGridViewExtensionWrapper {}

extension SeatGridViewExtension where Base == UIFont {
    static func customFont(ofSize fontSize: CGFloat, weight: UIFont.Weight = .regular) -> UIFont {
        switch weight {
        case .regular:
            return UIFont(name: "PingFangSC-Regular", size: fontSize) ?? .systemFont(ofSize: fontSize, weight: weight)
        case .medium:
            return UIFont(name: "PingFangSC-Medium", size: fontSize) ?? .systemFont(ofSize: fontSize, weight: weight)
        case .semibold:
            return UIFont(name: "PingFangSC-Semibold", size: fontSize) ?? .systemFont(ofSize: fontSize, weight: weight)
        case .light:
            return UIFont(name: "PingFangSC-Light", size: fontSize) ?? .systemFont(ofSize: fontSize, weight: weight)
        case .ultraLight:
            return UIFont(name: "PingFangSC-Ultralight", size: fontSize) ?? .systemFont(ofSize: fontSize, weight: weight)
        default:
            return UIFont(name: "PingFangSC-Regular", size: fontSize) ?? .systemFont(ofSize: fontSize, weight: weight)
        }
    }
}


