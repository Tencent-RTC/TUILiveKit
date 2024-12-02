//
//  UIViewController+Extension.swift
//  TUILiveKitApp
//
//  Created by krabyu on 2023/10/20.
//

import UIKit

extension UIViewController {
    open override var canBecomeFirstResponder: Bool {
        return true
    }

    open override func motionEnded(_ motion: UIEvent.EventSubtype, with event: UIEvent?) {
#if DEBUG
        if motion == .motionShake {
            let sheet = UIAlertController(title: "Debug", message: nil, preferredStyle: .actionSheet)
            let _2DAction = UIAlertAction(title: "2D View", style: .default) { _ in
               let success = NotificationCenter.default.post(name: NSNotification.Name("Lookin_2D"), object: nil)
               print(success)
            }
            sheet.addAction(_2DAction)
            let _3DAction = UIAlertAction(title: "3D View", style: .default) { _ in
               let success = NotificationCenter.default.post(name: NSNotification.Name("Lookin_3D"), object: nil)
               print(success)
            }
            sheet.addAction(_3DAction)
            let _ExportAction = UIAlertAction(title: "Export View", style: .default) { _ in
               let success = NotificationCenter.default.post(name: NSNotification.Name("Lookin_Export"), object: nil)
               print(success)
            }
            sheet.addAction(_ExportAction)

            sheet.addAction(UIAlertAction(title: "Cancel", style: .cancel))
            present(sheet, animated: true)
           }
#endif
    }
}
