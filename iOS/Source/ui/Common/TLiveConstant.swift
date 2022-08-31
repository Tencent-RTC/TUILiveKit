//
//  TLiveConstant.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/21.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation

let SCREEN_WIDTH = UIScreen.main.bounds.size.width
let SCREEN_HEIGHT = UIScreen.main.bounds.size.height
let StatusBarHeight = UIApplication.shared.statusBarFrame.height

let MSG_TABLEVIEW_WIDTH = 200
let MSG_TABLEVIEW_HEIGHT = 240
let MSG_TABLEVIEW_BOTTOM_SPACE = 40
let MSG_TABLEVIEW_LABEL_FONT = 14
let MSG_BULLETVIEW_HEIGHT = 34
let MSG_UI_SPACE = 10

let MSG_TEXT_SEND_VIEW_HEIGHT = 45
let MSG_TEXT_SEND_FEILD_HEIGHT = 25
let MSG_TEXT_SEND_BTN_WIDTH = 40
let MSG_TEXT_SEND_BULLET_BTN_WIDTH = 55

let BOTTOM_BTN_ICON_WIDTH = 44

let IMAGE_SIZE = 35
let IMAGE_SPACE = 5

let MSG_ANIMATE_VIEW_SPACE = 30.0
let MSG_IMAGEVIEW_WIDTH = 27.0
let MSG_LABEL_FONT = 18
let FULL_SCREEN_PLAY_VIDEO_VIEW = 10000
let MSG_ANIMATE_DURANTION  = 8.0
let VIDEO_VIEW_WIDTH = 100
let VIDEO_VIEW_HEIGHT = 150
let VIDEO_VIEW_MARGIN_BOTTOM = 116
let VIDEO_VIEW_MARGIN_RIGHT = 8
let VIDEO_VIEW_MARGIN_SPACE = 5

let MAX_LINKMIC_MEMBER_SUPPORT = 3

let trtcLiveSendMsgTimeOut: Int32 = 15
let trtcLiveHandleMsgTimeOut: Int32 = 10
let trtcLiveCheckStatusTimeOut: Int32 = 3


enum TCMsgModelType : Int {
    case normal = 1
    case memberEnterRoom = 2
    case memberQuitRoom = 3
    case praise = 4
    case danmaMsg = 5
    case unknownMsg = 0
    
}

let SafeAreaTopHeight = IPHONE_X ? 44 : 20

var IPHONE_X: Bool
{
    var isPhoneX = false
    if #available(iOS 11.0, *) {
        isPhoneX = (UIApplication.shared.keyWindow?.safeAreaInsets.bottom ?? 0.0) > 0.0
    }
    return isPhoneX
}

func IntConversionTCMsgModelType(command: String) -> TCMsgModelType {
    switch Int(command) {
    case 1:
        return TCMsgModelType.normal
    case 2:
        return TCMsgModelType.memberEnterRoom
    case 3:
        return TCMsgModelType.memberQuitRoom
    case 4:
        return TCMsgModelType.praise
    case 5:
        return TCMsgModelType.danmaMsg
    default:
        return TCMsgModelType.unknownMsg
    }
}
