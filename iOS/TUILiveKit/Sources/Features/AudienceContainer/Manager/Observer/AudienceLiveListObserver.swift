//
//  AudienceLiveListObserver.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCRoomEngine

class AudienceLiveListObserver: NSObject {
    private(set) weak var context: AudienceManager.Context?
    
    init(context: AudienceManager.Context) {
        self.context = context
        super.init()
    }
}

extension AudienceLiveListObserver: TUILiveListManagerObserver {
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        context?.roomManager.onLiveInfoChanged(liveInfo: liveInfo, modifyFlag: modifyFlag)
    }
}
