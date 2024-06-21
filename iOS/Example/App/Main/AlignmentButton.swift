//
//  AlignmentButton.swift
//  TUILiveKitApp
//
//  Created by krabyu on 2024/6/19.
//

import UIKit

class AlignmentButton: UIButton {
    enum ImageAlignment: NSInteger {
        case left = 0
        case top
        case bottom
        case right
    }
    var imageAlignment: ImageAlignment = .left
    var spaceBetweenTitleAndImage: CGFloat = 0
    
    override func layoutSubviews() {
        super.layoutSubviews()
        
        let space: CGFloat = self.spaceBetweenTitleAndImage
        
        let titleWidth: CGFloat = self.titleLabel?.bounds.width ?? 0
        let titleHeight: CGFloat = self.titleLabel?.bounds.height ?? 0
        
        let imageWidth: CGFloat = self.imageView?.bounds.width ?? 0
        let imageHeight: CGFloat = self.imageView?.bounds.height ?? 0
        
        let buttonCenterX: CGFloat = self.bounds.width / 2
        let imageCenterX: CGFloat = buttonCenterX - titleWidth / 2
        let titleCenterX = buttonCenterX + imageWidth / 2
        
        switch self.imageAlignment {
        case .top:
            self.titleEdgeInsets = UIEdgeInsets(top: imageHeight / 2 + space / 2,
                                                left: -(titleCenterX - buttonCenterX),
                                                bottom: -(imageHeight/2 + space/2),
                                                right: titleCenterX-buttonCenterX)
            self.imageEdgeInsets = UIEdgeInsets(top: -(titleHeight / 2 + space / 2),
                                                left: buttonCenterX - imageCenterX,
                                                bottom: titleHeight / 2 + space / 2,
                                                right: -(buttonCenterX - imageCenterX))
        case .left:
            self.titleEdgeInsets = UIEdgeInsets(top: 0, left: space / 2, bottom: 0, right: -space / 2)
            self.imageEdgeInsets = UIEdgeInsets(top: 0, left: -space / 2, bottom: 0, right: space)
        case .bottom:
            self.titleEdgeInsets = UIEdgeInsets(top: -(imageHeight / 2 + space / 2), 
                                                left: -(titleCenterX - buttonCenterX),
                                                bottom: imageHeight / 2 + space / 2,
                                                right: titleCenterX - buttonCenterX)
            self.imageEdgeInsets = UIEdgeInsets(top: titleHeight / 2 + space / 2, 
                                                left: buttonCenterX - imageCenterX,
                                                bottom: -(titleHeight / 2 + space / 2),
                                                right: -(buttonCenterX - imageCenterX))
        case .right:
            self.titleEdgeInsets = UIEdgeInsets(top: 0, left: -(imageWidth + space / 2),
                                                bottom: 0, right: imageWidth + space / 2)
            self.imageEdgeInsets = UIEdgeInsets(top: 0, left: titleWidth + space / 2, 
                                                bottom: 0, right: -(titleWidth + space / 2))
        }
    }
 
}

