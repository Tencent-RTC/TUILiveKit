//
//  UIViewController+Alert.swift
//  TUILiveRoom
//
//  Created by adams on 2021/5/27.
//

extension UIViewController {
    @objc public func alertUserTips(_ vc: UIViewController) {
        // 提醒用户不要用Demo App来做违法的事情
        // 每天提醒一次
        let nowDay = Calendar.current.component(.day, from: Date())
        if let day = UserDefaults.standard.object(forKey: "UserTipsKey") as? Int {
            if day == nowDay {
                return
            }
        }
        UserDefaults.standard.set(nowDay, forKey: "UserTipsKey")
        UserDefaults.standard.synchronize()
        let alertVC = UIAlertController(title:LiveRoomLocalize("LoginNetwork.AppUtils.warmprompt"), message: LiveRoomLocalize("LoginNetwork.AppUtils.tomeettheregulatory"), preferredStyle: UIAlertController.Style.alert)
        let okView = UIAlertAction(title: LiveRoomLocalize("LoginNetwork.AppUtils.determine"), style: UIAlertAction.Style.default, handler: nil)
        alertVC.addAction(okView)
        vc.present(alertVC, animated: true, completion: nil)
    }
}

