//
//  UIAlerController+Extension.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/11.
//

import RTCCommon

extension UIAlertController {
    static func showAlertController(title: String? = nil,
                                    message: String? = nil,
                                    cancel: String? = nil,
                                    sure: String? = nil,
                                    callback: ((_ sure: Bool) -> Void)? = nil) {
        let alertVC = UIAlertController(title: title, message: message,
                                        preferredStyle: .alert)
        if let cancel = cancel {
            let cancelAction = UIAlertAction(title: cancel, style: .cancel) { _ in
                callback?(false)
            }
            alertVC.addAction(cancelAction)
        }

        if let sure = sure {
            let sureAction = UIAlertAction(title: sure, style: .default) { _ in
                callback?(true)
            }
            alertVC.addAction(sureAction)
        }
        WindowUtils.getCurrentWindowViewController()?.present(alertVC, animated: true, completion: nil)
    }
}
