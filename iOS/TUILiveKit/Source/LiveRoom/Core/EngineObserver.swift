//
//  EngineObserver.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/6.
//

import RTCRoomEngine
import UIKit
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class EngineObserver: NSObject {
    private var liveRoomInfo: LiveRoomInfo
    @WeakLazyInjected var engineManager:EngineServiceProvider?
    private var engineService: RoomEngineService? {
        return engineManager?.getRoomEngineService(roomId: liveRoomInfo.roomId.value)
    }
    
    init(liveRoomInfo:LiveRoomInfo) {
        self.liveRoomInfo = liveRoomInfo
    }

    private func handleLeftList(leftList: [TUISeatInfo]) {
        guard let engineService = engineService else{ return}
        for seatInfo: TUISeatInfo in leftList {
            guard let userId = seatInfo.userId else { continue }
            if userId == engineService.liveRoomInfo.selfInfo.userId {
                engineService.liveRoomInfo.selfInfo.role.value = .audience
                engineService.liveRoomInfo.selfInfo.status.value = .none
                return
            }
        }
    }
}

extension EngineObserver: TUIRoomObserver {
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        LiveKitLog.info("\(#file)","\(#line)","onRemoteUserEnterRoom:[roomId:\(roomId),userId:\(userInfo.userId)]")
        let user = liveRoomInfo.userInfoPool.get(userInfo.userId)
        user.update(userInfo)
        guard liveRoomInfo.audienceList.value.filter({ $0.userId == userInfo.userId }).first != nil else {
            liveRoomInfo.audienceList.value.append(user)
            return
        }
    }

    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        guard let engineService = engineService else{ return}
        liveRoomInfo.audienceList.value = liveRoomInfo.audienceList.value.filter({ $0.userId != userInfo.userId })
        liveRoomInfo.linkingAudienceList.value = liveRoomInfo.linkingAudienceList.value.filter({ $0.userId != userInfo.userId })
        engineService.liveRoomInfo.applyLinkAudienceList =
        engineService.liveRoomInfo.applyLinkAudienceList.filter({ $0.userId != userInfo.userId })
        liveRoomInfo.userInfoPool.remove(userInfo.userId)
        LiveKitLog.info("\(#file)","\(#line)","onRemoteUserLeaveRoom:[roomId:\(roomId),userId:\(userInfo.userId)]")
    }

    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        LiveKitLog.info("\(#file)","\(#line)","onUserAudioStateChanged:[userId:\(userId),hasAudio:\(hasAudio)]")
        let user = liveRoomInfo.userInfoPool.get(userId)
        user.audioInfo.muteAudio.value = !hasAudio
    }

    func onUserVideoStateChanged(userId: String, streamType: TUIVideoStreamType, hasVideo: Bool, reason: TUIChangeReason) {
        LiveKitLog.info("\(#file)","\(#line)","onUserVideoStateChanged:[userId:\(userId),hasVideo:\(hasVideo)]")
        let user = liveRoomInfo.userInfoPool.get(userId)
        user.videoInfo.isCameraOpened.value = hasVideo
    }

    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        LiveKitLog.info("\(#file)","\(#line)","onSeatListChanged:[seatList.count:\(seatList.count),leftList.count:\(leftList.count)]")
        engineService?.handleSeatList(seatList: seatList,seatedList: seatedList)
        handleLeftList(leftList: leftList)
    }

    func onRoomDismissed(roomId: String) {
        LiveKitLog.info("\(#file)","\(#line)","onRoomDismissed:[roomId:\(roomId)]")
        engineService?.changeUserLiveStatus(userLiveStatus: .none)
    }

    // MARK: - Signaling request-related callback

    func onRequestReceived(request: TUIRequest) {
        LiveKitLog.info("\(#file)","\(#line)","onRequestReceived:[requestId:\(request.requestId),requestAction:\(request.requestAction)]")
        guard let userInfo = liveRoomInfo.audienceList.value.first(where: { $0.userId == request.userId }) else { return }
        guard let engineService = engineService else{ return }
        userInfo.requestId = request.requestId
        if request.requestAction == .takeSeat && liveRoomInfo.interactionType.value == .link {
            var array = engineService.liveRoomInfo.applyLinkAudienceList.filter({ $0.userId != userInfo.userId })
            array.append(userInfo)
            engineService.liveRoomInfo.applyLinkAudienceList = array
        }
    }

    func onRequestCancelled(requestId: String, userId: String) {
        LiveKitLog.info("\(#file)","\(#line)","onRequestCancelled:[requestId:\(requestId),userId:\(userId)]")
        guard let engineService = engineService else{ return }
        engineService.liveRoomInfo.applyLinkAudienceList =
        engineService.liveRoomInfo.applyLinkAudienceList.filter({ $0.requestId != requestId })
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        guard let engineService = engineService else{ return }
        let count = max(userCount - 1, 0)
        engineService.liveRoomInfo.audienceCount.value = max(count, engineService.liveRoomInfo.audienceCount.value)
    }
}
