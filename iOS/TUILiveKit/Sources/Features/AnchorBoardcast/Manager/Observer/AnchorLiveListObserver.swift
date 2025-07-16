//
//  AnchorLiveListObserver.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCRoomEngine

class AnchorLiveListObserver: NSObject {
    private(set) weak var context: AnchorManager.Context?
    
    init(context: AnchorManager.Context) {
        self.context = context
        super.init()
    }
}

extension AnchorLiveListObserver: TUILiveListManagerObserver {
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        context?.roomManager.onLiveInfoChanged(liveInfo: liveInfo, modifyFlag: modifyFlag)
    }
}
