//
//  AnchorPrepareViewDefine.swift
//  TUILiveKit
//
//  Created by gg on 2025/5/15.
//

import Foundation

public class EditInfo {
    public var roomName: String
    @Published public var coverUrl: String
    @Published public var privacyMode: LiveStreamPrivacyStatus
    init(roomName: String, coverUrl: String, privacyMode: LiveStreamPrivacyStatus) {
        self.roomName = roomName
        self.coverUrl = coverUrl
        self.privacyMode = privacyMode
    }
}

public enum Feature {
    case beauty
    case audioEffect
    case flipCamera
}

public protocol AnchorPrepareViewDelegate: AnyObject {
    func prepareView(_ view: AnchorPrepareView, didClickBack button: UIButton)
    func prepareView(_ view: AnchorPrepareView, didClickStart button: UIButton, editInfo: EditInfo)
}
