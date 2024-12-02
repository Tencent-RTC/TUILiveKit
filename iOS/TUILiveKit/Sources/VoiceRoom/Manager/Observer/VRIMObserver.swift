//
//  VRIMObserver.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/22.
//

import Foundation
import ImSDK_Plus

protocol VRIMObserverInterface {
    func onMyFollowingListChanged(userInfoList: [V2TIMUserFullInfo], isAdd: Bool)
}

class VRIMObserver: NSObject {
    private(set) weak var context: VoiceRoomManager.Context?
    init(context: VoiceRoomManager.Context) {
        self.context = context
        super.init()
    }
}

extension VRIMObserver {
    private var userManager: VRUserManager? {
        context?.userManager
    }
}

extension VRIMObserver: V2TIMFriendshipListener {
    func onMyFollowingListChanged(userInfoList: [V2TIMUserFullInfo], isAdd: Bool) {
        userManager?.onMyFollowingListChanged(userInfoList: userInfoList, isAdd: isAdd)
    }
}
