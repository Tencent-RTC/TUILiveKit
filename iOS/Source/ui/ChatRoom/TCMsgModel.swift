//
//  TCMsgModel.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/21.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation

struct IMUserAble {
    var cmdType: TCMsgModelType?
    var imUserId: String?
    var imUserName: String?
    var imUserIconUrl: String?
}

struct TCMsgModel {
    var msgType: TCMsgModelType?
    var userId: String?
    var userName: String?
    var userMsg: String?
    var userHeadImageUrl: String?
    var msgHeight:CGFloat = 0
    var msgAttribText: NSAttributedString?
}
