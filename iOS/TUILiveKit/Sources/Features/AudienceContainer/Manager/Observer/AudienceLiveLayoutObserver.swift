//
//  AudienceLiveLayoutObserver.swift
//  Pods
//
//  Created by vincepzhang on 2025/6/16.
//

import RTCRoomEngine

class AudienceLiveLayoutObserver: NSObject, TUILiveLayoutObserver {
    private(set) weak var context: AudienceManager.Context?
    
    init(context: AudienceManager.Context) {
        self.context = context
        super.init()
    }
    
    func onSeatLayoutChanged(roomId: String, layout: TUISeatLayout) {
        guard let localRoomId = context?.roomManager.roomState.roomId else { return }
        if (localRoomId != roomId) { return }
        context?.roomManager.updateVideoStreamIsLandscape(isLandscape: layout.canvasWidth >= layout.canvasHeight)
    }
}
