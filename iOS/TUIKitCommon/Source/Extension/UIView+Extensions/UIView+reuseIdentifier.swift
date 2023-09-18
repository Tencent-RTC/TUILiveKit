//  
//  UIView+reuseIdentifier.swift
//  XiaoZhiBoApp
//
//  Created by jack on 2021/12/13.
//  Copyright © 2022 Tencent. All rights reserved.

import UIKit

// MARK: - UICollectionViewCell 重用标识符
extension UITableViewCell {
    
    /// 获取Cell的重用标识符
    public class var cellReuseIdentifier: String {
        return "reuseId_\(self.description())"
    }
    
}

// MARK: - UICollectionViewCell 重用标识符
extension UICollectionViewCell {
    
    /// 获取Cell的重用标识符
    public class var cellReuseIdentifier: String {
        return "reuseId_\(self.description())"
    }
    
}
