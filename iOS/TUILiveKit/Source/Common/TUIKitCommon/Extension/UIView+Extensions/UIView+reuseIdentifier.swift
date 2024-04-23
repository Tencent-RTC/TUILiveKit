//  
//  UIView+reuseIdentifier.swift
//  XiaoZhiBoApp
//
//  Created by jack on 2021/12/13.
//  Copyright © 2022 Tencent. All rights reserved.

import UIKit

extension UITableViewCell {
    
    public class var cellReuseIdentifier: String {
        return "reuseId_\(self.description())"
    }
    
}

extension UICollectionViewCell {
    
    public class var cellReuseIdentifier: String {
        return "reuseId_\(self.description())"
    }
    
}
