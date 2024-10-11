//
//  IMManager.swift
//  TUILiveKitApp
//
//  Created by krabyu on 2024/6/19.
//

import Foundation
import ImSDK_Plus

typealias onSuccess = () -> Void
typealias onError = (Int,String) -> Void
class IMManager {
    func getUserFollowInfo(userId: String, onSuccess: onSuccess? = nil, onError: onError? = nil) {
        V2TIMManager.sharedInstance().getUserFollowInfo([userId], succ: { followInfo in
            guard let followInfo = followInfo?.first else { return}
            SettingsConfig.share.fansCount = Int(followInfo.followersCount)
            SettingsConfig.share.followCount = Int(followInfo.followingCount)
            onSuccess?()
        }, fail: { code, message in
            debugPrint("getUserFollowInfo onError, code:\(code), message:\(String(describing: message))")
            onError?(Int(code), message ?? "")
        })
    }
    
    func changeUserName(newName name: String, onSuccess: onSuccess? = nil, onError: onError? = nil) {
        let userFullInfo = V2TIMUserFullInfo()
        userFullInfo.nickName = name
        V2TIMManager.sharedInstance().setSelfInfo(userFullInfo, succ: { [weak self] in
            guard let self = self else { return }
            SettingsConfig.share.name = name
            onSuccess?()
        }, fail: { code, message in
            debugPrint("changeUserName onError, code:\(code), message:\(String(describing: message))")
            onError?(Int(code), message ?? "")
        })
    }
 }
