//
//  NSTextAttachment+Emotion.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import UIKit

private var displayTextKey: Void?
extension NSTextAttachment {
    var displayText: String {
        set {
            objc_setAssociatedObject(self, &displayTextKey, newValue, objc_AssociationPolicy.OBJC_ASSOCIATION_COPY_NONATOMIC)
        }
        get {
            return objc_getAssociatedObject(self, &displayTextKey) as? String ?? ""
        }
    }
}
