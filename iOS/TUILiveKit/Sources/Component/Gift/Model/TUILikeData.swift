//
//  TUILikeData.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/2.
//

import Foundation
import RTCRoomEngine

public class TUILikeData {
    public var sender: TUIUserInfo = TUIUserInfo()

    public init() {}
    
    public init(sender: TUIUserInfo) {
        self.sender = sender
    }
}

