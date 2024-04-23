//
//  TUILiveDefine.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/11.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation

public class LiveStreamParams {
    public init() {}
    public var category: LiveStreamCategory = .chat
    public var liveMode: LiveMode = .public
}

public enum RoleType {
    case none
    case anchor
    case audience
}

public enum UserInteractionStatus  {
    case none
    case applying
    case linking
    case pking
}

public enum InteractionType  {
    case broadcast
    case link
    case pk
}

public enum LiveStreamCategory: CaseIterable  {
    case chat
    case living
    case beauty
    case teach
    case shopping
    case music
}


public enum LiveMode: CaseIterable{
    case `public`
    case privacy
}

public enum UserLiveStatus {
    case none
    case previewing
    case pushing
    case playing
}
