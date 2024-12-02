//
//  LSLiveListObserver.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCRoomEngine

class LSLiveListObserver: NSObject {
    private(set) weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        super.init()
    }
}

extension LSLiveListObserver: TUILiveListManagerObserver {
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        context?.roomManager.onLiveInfoChanged(liveInfo: liveInfo, modifyFlag: modifyFlag)
    }
}
