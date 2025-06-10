//
//  RoomInfoService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/10/31.
//

import Foundation
import RTCRoomEngine
import ImSDK_Plus
import TUICore

class RoomInfoService: NSObject {
    let state = RoomInfoState()
    
    func initRoomInfo(roomInfo: TUIRoomInfo) {
        state.roomId = roomInfo.roomId
        state.selfUserId = TUILogin.getUserID() ?? ""
        state.ownerId = roomInfo.ownerId
        state.ownerName = roomInfo.ownerName
        state.ownerAvatarUrl = roomInfo.ownerAvatarUrl
        V2TIMManager.sharedInstance().addFriendListener(listener: self)
        TUIRoomEngine.sharedInstance().addObserver(self)
    }
    
    func getFansNumber() {
        V2TIMManager.sharedInstance().getUserFollowInfo(userIDList: [state.ownerId]) { [weak self] followInfoList in
            guard let self = self, let followInfo = followInfoList?.first else { return }
            self.state.fansNumber = Int(followInfo.followersCount)
        } fail: { code, message in
            debugPrint("[RoomInfo] getFansNumber failed, error:\(code), message:\(String(describing: message))")
        }
    }
    
    func isFollow(userId: String) {
        let userInfo = TUIUserInfo()
        userInfo.userId = userId
        V2TIMManager.sharedInstance().checkFollowType(userIDList: [state.ownerId]) { [weak self] checkResultList in
            guard let self = self, let result = checkResultList?.first else { return }
            if result.followType == .FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST || result.followType == .FOLLOW_TYPE_IN_MY_FOLLOWING_LIST {
                if !self.state.followingList.contains(where: { $0.userId == userId }) {
                    self.state.followingList.insert(userInfo)
                }
            } else {
                self.state.followingList = state.followingList.filter { $0.userId != userId}
            }
        } fail: { code, message in
            debugPrint("[RoomInfo] isFollow failed, error:\(code), message:\(String(describing: message))")
        }
    }
    
    func followUser(userId: String) {
        let userInfo = TUIUserInfo()
        userInfo.userId = userId
        V2TIMManager.sharedInstance().followUser(userIDList: [userId]) { [weak self] followResultList in
            guard let self = self, let result = followResultList?.first else { return }
            if result.resultCode == 0, !self.state.followingList.contains(where: { $0.userId == userId }) {
                self.state.followingList.insert(userInfo)
                self.getFansNumber()
            }
        } fail: { code, message in
            debugPrint("[RoomInfo] followUser failed, error:\(code), message:\(String(describing: message))")
        }
    }
    
    func unfollowUser(userId: String) {
        V2TIMManager.sharedInstance().unfollowUser(userIDList: [userId]) { [weak self] followResultList in
            guard let self = self, let result = followResultList?.first else { return }
            if result.resultCode == 0 {
                self.state.followingList = state.followingList.filter { $0.userId != userId}
            }
        } fail: { code, message in
            debugPrint("[RoomInfo] unfollowUser failed, error:\(code), message:\(String(describing: message))")
        }
    }
}

extension RoomInfoService: V2TIMFriendshipListener {
    // audience listen to this callback
    func onMyFollowingListChanged(userInfoList: [V2TIMUserFullInfo], isAdd: Bool) {
        isFollow(userId: state.ownerId)
    }
   
    // owner listen to this callback
    func onMyFollowersListChanged(userInfoList: [V2TIMUserFullInfo], isAdd: Bool) {
        isFollow(userId: state.ownerId)
    }
}

extension RoomInfoService: TUIRoomObserver {
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        state.roomDismissedSubject.send(roomId)
    }
}
