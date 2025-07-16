//
//  NetWorkInfoService.swift
//  Pods
//
//  Created by ssc on 2025/5/9.
//

import Foundation
import Network
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

protocol NetWorkInfoServiceDelegate {
    func addTRTCObserver(_ observer: TRTCCloudDelegate)
    func removeTRTCObserver(_ observer: TRTCCloudDelegate)
}

class NetWorkInfoService {

    private let trtcCloud: TRTCCloud
    private let roomEngine: TUIRoomEngine = TUIRoomEngine.sharedInstance()

    init(trtcCloud: TRTCCloud) {
        self.trtcCloud = trtcCloud
    }

    func setAudioCaptureVolume(volume : Int) {
        trtcCloud.setAudioCaptureVolume(volume)
    }

    func getSelfUserId() -> String {
        return TUIRoomEngine.getSelfInfo().userId
    }

    func getVolueme() -> Int {
        return trtcCloud.getAudioCaptureVolume()
    }

    func setVideoResolution(resolution: TRTCVideoResolution) {
        let params = TRTCVideoEncParam()
        params.videoResolution = resolution
        trtcCloud.setVideoEncoderParam(params)
    }

    func updateAudioQuality(quality: TUIAudioQuality) {
        roomEngine.updateAudioQuality(quality)
    }
}

extension NetWorkInfoService: NetWorkInfoServiceDelegate {
    func addTRTCObserver(_ observer: any TRTCCloudDelegate) {
        trtcCloud.addDelegate(observer)
    }

    func addRoomEngineObserver(_ observer: any TUIRoomObserver) {
        roomEngine.addObserver(observer)
    }

    func removeTRTCObserver(_ observer: any TRTCCloudDelegate) {
        trtcCloud.removeDelegate(observer)
    }

    func removeRoomEngineObserver(_ observer: any TUIRoomObserver) {
        roomEngine.removeObserver(observer)
    }
}
