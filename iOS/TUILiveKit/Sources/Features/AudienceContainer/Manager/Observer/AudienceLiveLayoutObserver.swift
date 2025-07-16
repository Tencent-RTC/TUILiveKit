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
    
    func onLiveVideoLayoutChanged(roomId: String, layoutInfo: String) {
        guard let localRoomId = context?.roomManager.roomState.roomId else { return }
        if (localRoomId != roomId) { return }
        if let canvasSize = parseCanvasSize(json:  layoutInfo) {
            context?.roomManager.updateVideoStreamIsLandscape(isLandscape: canvasSize.width >= canvasSize.height)
        }
    }
        
    private func parseCanvasSize(json: String) -> CGSize? {
        guard let data = json.data(using: .utf8) else { return nil }
        guard let dic = try? JSONSerialization.jsonObject(with: data) as? [String: Any], dic.keys.contains("canvas") else { return nil }
        if let canvas = dic["canvas"] as? [String: Int] {
            guard canvas.keys.contains("w"), canvas.keys.contains("h") else { return nil }
            if let w = canvas["w"], let h = canvas["h"] {
                return CGSize(width: w, height: h)
            }
        }
        return nil
    }
}
