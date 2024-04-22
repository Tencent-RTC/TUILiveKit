//
//  UserManager.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/15.
//

import Foundation
import ImSDK_Plus
typealias GetUserInfoSuccessCallback = (_ userInfo: UserInfo?) -> Void
typealias GetUserListSuccessCallback = (_ userList: [UserInfo]) -> Void
public class UserManager {

    static func getUserInfo(userId: String, success: @escaping GetUserInfoSuccessCallback, fail: V2TIMFail? = nil) {
        getUserList(userList: [userId], success: { userList in
            success(userList.first)
        }, fail: fail)
    }

    static func getUserList(userList: [String], success: @escaping GetUserListSuccessCallback, fail: V2TIMFail? = nil) {
        LiveKitLog.info("\(#file)","\(#line)","getUserList:[userList:\(userList)]")
        V2TIMManager.sharedInstance().getUsersInfo(userList) { userFullInfoList in
            var list: [UserInfo] = []
            guard let userFullInfoList = userFullInfoList else {
                LiveKitLog.warn("\(#file)","\(#line)","getUserList:[userList:\(userList),userFullInfoList:0]")
                success(list)
                return
            }
            LiveKitLog.info("\(#file)","\(#line)","getUserList:[userList:\(userList),userFullInfoList:\(userFullInfoList.count)]")
            for userFullInfo in userFullInfoList {
                let user = UserInfo()
                user.userId = userFullInfo.userID
                user.avatarUrl.value = userFullInfo.faceURL
                user.name.value = userFullInfo.nickName
                list.append(user)
            }
            success(list)
        } fail: { code, message in
            LiveKitLog.error("\(#file)","\(#line)","getUserList:[userList:\(userList),code:\(code),message:\(String(describing: message))]")
            fail?(code, message)
        }
    }
}
