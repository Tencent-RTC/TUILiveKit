//
//  UIView+roundrect.swift
//  TRTCScene
//
//  Created by adams on 2021/4/8.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation

public extension UIView {
    func roundedRect(rect: CGRect, byRoundingCorners: UIRectCorner, cornerRadii: CGSize) {
        let maskPath = UIBezierPath(roundedRect: rect, byRoundingCorners: byRoundingCorners, cornerRadii: cornerRadii)
        let maskLayer = CAShapeLayer()
        maskLayer.frame = bounds
        maskLayer.path = maskPath.cgPath
        layer.mask = maskLayer
    }

    func roundedCircle(rect: CGRect) {
        roundedRect(rect: rect, byRoundingCorners: .allCorners, cornerRadii: CGSize(width: bounds.size.width / 2, height: bounds.size.height / 2))
    }
    
   func clearRoundedCorners() {
       layer.mask = nil
    }
}

public extension UIView {
    private struct AssociatedKeys {
        static var gradientLayerKey = "gradientLayerKey"
    }

    var gradientLayer: CAGradientLayer? {
        get {
            return objc_getAssociatedObject(self, &AssociatedKeys.gradientLayerKey) as? CAGradientLayer
        }
        set {
            objc_setAssociatedObject(self, &AssociatedKeys.gradientLayerKey, newValue, objc_AssociationPolicy.OBJC_ASSOCIATION_RETAIN_NONATOMIC)
        }
    }

    func removeGradientLayer() {
        guard let glayer = gradientLayer else {
            return
        }
        glayer.removeFromSuperlayer()
        gradientLayer = nil
    }

    @discardableResult
    func gradient(colors: [UIColor], bounds: CGRect = .zero, isVertical: Bool = false) -> CAGradientLayer {
        let gradientLayer = self.gradientLayer ?? CAGradientLayer()
        if isVertical {
            gradientLayer.startPoint = CGPoint(x: 0.5, y: 0.0)
            gradientLayer.endPoint = CGPoint(x: 0.5, y: 1.0)
        } else {
            gradientLayer.startPoint = CGPoint(x: 0.0, y: 0.5)
            gradientLayer.endPoint = CGPoint(x: 1.0, y: 0.5)
        }
        self.gradientLayer = gradientLayer
        gradientLayer.colors = colors.map { $0.cgColor }
        gradientLayer.frame = bounds == .zero ? self.bounds : bounds
        layer.insertSublayer(gradientLayer, at: 0)
        return gradientLayer
    }
}
