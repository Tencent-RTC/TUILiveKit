//
//  TCMsgModel.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/21.
//

import Foundation

struct IMUserAble {
    var cmdType: TCMsgModelType?
    // 两个用户是否相同，可通过比较imUserId来判断
    // 用户IMSDK的identigier
    var imUserId: String?
    // 用户昵称
    var imUserName: String?
    // 用户头像地址
    var imUserIconUrl: String?
}

struct TCMsgModel {
    var msgType: TCMsgModelType? //消息类型
    var userId: String? //用户Id
    var userName: String? //用户名字
    var userMsg: String? //用户发的消息
    var userHeadImageUrl: String? //用户头像url
    var msgHeight:CGFloat = 0 //消息高度
    var msgAttribText: NSAttributedString?
}
