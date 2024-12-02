//
//  LiveListObserver.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/15.
//

import Foundation
import RTCRoomEngine

protocol VRLiveListObserverInterface {
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag)
}

class VRLiveListObserver: NSObject, VRLiveListObserverInterface {
    private(set) weak var context: VoiceRoomManager.Context?
    init(context: VoiceRoomManager.Context) {
        self.context = context
        super.init()
    }
}

extension VRLiveListObserver {
    private var roomManager: VRRoomManager? {
        context?.roomManager
    }
}

// MARK: - TUILiveListManagerObserver
extension VRLiveListObserver: TUILiveListManagerObserver {
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        roomManager?.onLiveInfoChanged(liveInfo: liveInfo, modifyFlag: modifyFlag)
    }
}
