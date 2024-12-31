//
//  LiveLayoutObserver.swift
//  LiveStreamCore
//
//  Created by adamsfliu on 2024/12/2.
//

import RTCRoomEngine

class LiveLayoutObserver: NSObject, TUILiveLayoutObserver {
    private(set) weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        super.init()
    }
    
    func onLiveVideoLayoutChanged(roomId: String, layoutInfo: String) {
        guard let localRoomId = context?.roomManager.roomState.roomId else { return }
        LiveStreamLog.info("\(#file)","\(#line)", "onLiveVideoLayoutChanged:[localRoomId: \(localRoomId), remoteRoomId: \(roomId) layoutInfo: \(layoutInfo)]")
        if (localRoomId != roomId) { return }
        guard var videoLayout = convertLayoutListFromJsonString(jsonString: layoutInfo), videoLayout.layoutList.count > 0 else {
            LiveStreamLog.info("\(#file)","\(#line)", "convertViewInfoMapFromJsonString fail")
            return
        }
        let layoutList = videoLayout.layoutList.map({ layout in
            var viewInfo = ViewInfo(x: layout.x,
                                    y: layout.y,
                                    width: layout.width,
                                    height: layout.height,
                                    zOrder: layout.zOrder,
                                    backgroundColor: "",
                                    userId: layout.userId)
            return viewInfo
        })
        videoLayout.layoutList = layoutList
        context?.layoutManager.updateVideoLayout(layout: videoLayout)
    }
    
    private func convertLayoutListFromJsonString(jsonString: String) -> VideoLayoutInfo? {
        guard let data = jsonString.data(using: .utf8) else {
            debugPrint("convert to json data error")
            return nil
        }
        do {
            return try JSONDecoder().decode(VideoLayoutInfo.self, from: data)
        } catch {
            debugPrint("Error loading or parsing JSON: \(error)")
            return nil
        }
    }
}
