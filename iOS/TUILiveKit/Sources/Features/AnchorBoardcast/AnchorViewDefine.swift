//
//  AnchorViewDefine.swift
//  TUILiveKit
//
//  Created by gg on 2025/6/20.
//

import Foundation

public class AnchorState {
    public var duration: Int = 0
    public var viewCount: Int = 0
    public var messageCount: Int = 0
    public var giftTotalCoins: Int = 0
    public var giftTotalUniqueSender: Int = 0
    public var likeTotalUniqueSender: Int = 0
}

public protocol AnchorViewDelegate: AnyObject {
    func onClickFloatWindow()
    func onEndLiving(state: AnchorState)
}

public enum RoomBehavior {
    case createRoom
    case enterRoom
}
