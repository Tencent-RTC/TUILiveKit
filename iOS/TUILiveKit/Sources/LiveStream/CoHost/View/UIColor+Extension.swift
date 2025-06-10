//
//  UIColor+Extension.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/6/5.
//

import UIKit

public extension UIColor {
    static func verticalGradientColor(colors: [UIColor?],
                             frame: CGRect) -> UIColor {
            let gradient = CAGradientLayer()
            gradient.frame = frame
            gradient.colors = colors.map({ color in
                return color?.cgColor ?? UIColor.black.cgColor
            })
            gradient.startPoint = CGPoint(x: 0.5, y: 0)
            gradient.endPoint = CGPoint(x: 0.5, y: 1)
           
            let render = UIGraphicsImageRenderer(size: gradient.bounds.size)
            let gradientImage = render.image { context in
                gradient.render(in: context.cgContext)
            }
            
            return UIColor(patternImage: gradientImage)
        }
        
        static func horizontalGradientColor(colors: [UIColor?],
                             frame: CGRect) -> UIColor {
            let gradient = CAGradientLayer()
            gradient.frame = frame
            gradient.colors = colors.map({ color in
                return color?.cgColor ?? UIColor.black.cgColor
            })
            gradient.startPoint = CGPoint(x: 0, y: 0.5)
            gradient.endPoint = CGPoint(x: 1, y: 0.5)
           
            let render = UIGraphicsImageRenderer(size: gradient.bounds.size)
            let gradientImage = render.image { context in
                gradient.render(in: context.cgContext)
            }
            
            return UIColor(patternImage: gradientImage)
        }
}
