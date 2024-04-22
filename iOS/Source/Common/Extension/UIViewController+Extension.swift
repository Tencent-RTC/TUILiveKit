//
//  UIViewController+Extension.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/11.
//

import UIKit

// TODO: - deprecated this extension, use 'router' replace.
extension UIViewController {
    
    func backToPreviousPage() {
        if let count = navigationController?.viewControllers.count, count > 1 {
            navigationController?.popViewController(animated: true)
        } else {
            dismiss(animated: true)
        }
    }
}
