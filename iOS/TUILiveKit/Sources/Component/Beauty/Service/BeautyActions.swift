//
//  BeautyActions.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/5/28.
//

import Foundation

enum BeautyActions {
    static let key = "Beauty.action"
    static let setSmoothLevel = ActionTemplate(id: key.appending(".SetSmoothLevel"), payloadType: Int.self)
    static let setWhitenessLevel = ActionTemplate(id: key.appending(".setWhitenessLevel"), payloadType: Int.self)
    static let setRuddyLevel = ActionTemplate(id: key.appending(".setRuddyLevel"), payloadType: Int.self)
    static let updateSmoothLevel = ActionTemplate(id: key.appending(".updateSmoothLevel"), payloadType: Int.self)
    static let updateWhitenessLevel = ActionTemplate(id: key.appending(".updateWhitenessLevel"), payloadType: Int.self)
    static let updateRuddyLevel = ActionTemplate(id: key.appending(".updateRuddyLevel"), payloadType: Int.self)
    static let setLocalVideoView = ActionTemplate(id: key.appending(".setLocalVideoView"), payloadType: UIView.self)
    static let openLocalCamera = ActionTemplate(id: key.appending(".openLocalCamera"))
}
