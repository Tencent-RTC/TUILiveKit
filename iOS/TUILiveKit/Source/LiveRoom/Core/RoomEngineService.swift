//
//  RoomEngineService.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/7.
//

import Foundation
import RTCRoomEngine
import TUICore

#if TXLiteAVSDK_TRTC
    import TXLiteAVSDK_TRTC
#elseif TXLiteAVSDK_Professional
    import TXLiteAVSDK_Professional
#endif

class RoomEngineService {
    private let timeOutNumber: Double = 0
    let liveRoomInfo: LiveRoomInfo
    let liveKitStore: LiveKitStore

    init(liveRoomInfo: LiveRoomInfo, liveKitStore: LiveKitStore) {
        self.liveRoomInfo = liveRoomInfo
        self.liveKitStore = liveKitStore
    }

    private(set) lazy var roomEngine: TUIRoomEngine = {
        let roomEngine = TUIRoomEngine.sharedInstance()
        roomEngine.addObserver(self.observer)
        return roomEngine
    }()

    private lazy var observer: EngineObserver = {
        let observer = EngineObserver(liveRoomInfo: self.liveRoomInfo)
        return observer
    }()

    func getUserList(onSuccess: TUISuccessBlock? = nil, onError: TUIErrorBlock? = nil) {
        LiveKitLog.info("\(#file)", "\(#line)", "getUserList")
        roomEngine.getUserList(nextSequence: 0) { [weak self] list, _ in
            LiveKitLog.info("\(#file)", "\(#line)", "getUserList:[onSuccess][list.count:\(list.count)]")
            self?.initAudienceList(list: list)
            onSuccess?()
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "getUserList:[code:\(code),message:\(message)]")
            onError?(code, message)
        }
    }

    private func initAudienceList(list: [TUIUserInfo]) {
        var audienceList: [UserInfo] = []
        list.forEach { userInfo in
            if userInfo.userName.isEmpty {
                userInfo.userName = userInfo.userId
            }
            let audienceInfo = self.liveRoomInfo.userInfoPool.get(userInfo.userId)
            audienceInfo.update(userInfo)
            audienceList.append(audienceInfo)
        }
        liveRoomInfo.audienceList.value = audienceList
    }

    deinit {
    }
}

// MARK: Room

extension RoomEngineService {
    func createRoom(roomInfo: TUIRoomInfo, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LiveKitLog.info("\(#file)", "\(#line)", "createRoom:[roomId:\(roomInfo.roomId),name:\(roomInfo.name)]")
        roomEngine.createRoom(roomInfo) { [weak self] in
            LiveKitLog.info("\(#file)", "\(#line)", "createRoom:onSuccess[roomId:\(roomInfo.roomId),name:\(roomInfo.name)]")
            self?.liveRoomInfo.createTime = Date().timeIntervalSince1970
            onSuccess()
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "createRoom:[roomId:\(roomInfo.roomId),name:\(roomInfo.name),code:\(code) message:\(message)]")
            onError(code, message)
        }
    }

    func enterRoom(roomId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LiveKitLog.info("\(#file)", "\(#line)", "enterRoom:[roomId:\(roomId)]")
        roomEngine.enterRoom(roomId, roomType: .live) { [weak self] roomInfo in
            LiveKitLog.info("\(#file)", "\(#line)", "enterRoom:onSuccess [roomId:\(roomId)]")
            self?.enterRoomSuccess()
            self?.liveRoomInfo.maxSeatCount = roomInfo?.maxSeatCount ?? 1
            onSuccess()
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "enterRoom:[roomId:\(roomId),code:\(code) message:\(message)]")
            onError(code, message)
        }
    }

    func destroyRoom(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        let roomId = liveRoomInfo.roomId.value
        LiveKitLog.info("\(#file)", "\(#line)", "destroyRoom:[roomId:\(roomId)]")
        roomEngine.destroyRoom { [weak self] in
            LiveKitLog.info("\(#file)", "\(#line)", "destroyRoom:[onSuccess][roomId:\(roomId)]")
            self?.liveRoomInfo.userLiveStatus.value = .none
            onSuccess?()
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "destroyRoom:[roomId:\(roomId),code:\(code) message:\(message)]")
            onError?(code, message)
        }
    }

    func exitRoom(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        let roomId = liveRoomInfo.roomId.value
        LiveKitLog.info("\(#file)", "\(#line)", "exitRoom:[roomId:\(roomId)]")
        roomEngine.exitRoom(syncWaiting: false) { [weak self] in
            LiveKitLog.info("\(#file)", "\(#line)", "exitRoom:[onSuccess][roomId:\(roomId)]")
            self?.liveRoomInfo.userLiveStatus.value = .none
            onSuccess?()
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "exitRoom:[roomId:\(roomId),code:\(code) message:\(message)]")
            onError?(code, message)
        }
    }

    func fetchRoomInfo(onSuccess: TUISuccessBlock? = nil, onError: TUIErrorBlock? = nil) {
        roomEngine.fetchRoomInfo { [weak self] roomInfo in
            self?.updateLiveRoomInfo(roomInfo: roomInfo)
            onSuccess?()
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "fetchRoomInfo:[code:\(code) message:\(message)]")
            onError?(code, message)
        }
    }

    private func enterRoomSuccess() {
        liveRoomInfo.interactionType.value = .broadcast
        fetchRoomInfo()
        getSeatList()
    }

    private func updateLiveRoomInfo(roomInfo: TUIRoomInfo?) {
        guard let roomInfo = roomInfo else { return }
        liveRoomInfo.roomId.value = roomInfo.roomId
        liveRoomInfo.name.value = roomInfo.name
        liveRoomInfo.coverUrl.value = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png"
        let ownerInfo = liveRoomInfo.userInfoPool.get(roomInfo.ownerId)
        ownerInfo.role.value = .anchor
        liveRoomInfo.anchorInfo.value = ownerInfo
        UserManager.getUserInfo(userId: roomInfo.ownerId) { [weak self] userInfo in
            guard let userInfo = userInfo else { return }
            ownerInfo.name.value = userInfo.name.value
            ownerInfo.avatarUrl.value = userInfo.avatarUrl.value
            self?.liveRoomInfo.anchorInfo.value = ownerInfo
        }
    }
}

// MARK: Seat

extension RoomEngineService {
    func handleSeatList(seatList: [TUISeatInfo], seatedList: [TUISeatInfo]) {
        var resultList = seatList.filter({
            guard let userId = $0.userId else { return false }
            if userId.isEmpty {
                return false
            }
            for seatInfo in seatedList where seatInfo.userId == userId {
                return false
            }
            return true
        })
        resultList = resultList + seatedList
        var list: [UserInfo] = []
        for seatInfo: TUISeatInfo in resultList {
            guard let userId = seatInfo.userId,
                  let userName = seatInfo.userName,
                  let avatarUrl = seatInfo.avatarUrl else { continue }
            let userInfo = liveRoomInfo.userInfoPool.get(userId)
            userInfo.name.value = userName
            userInfo.avatarUrl.value = avatarUrl
            list.append(userInfo)
        }

        if liveRoomInfo.interactionType.value == .link || (liveRoomInfo.anchorInfo.value.userId != liveKitStore.selfInfo.userId) {
            liveRoomInfo.linkingAudienceList.value = list
        } else {
            liveRoomInfo.linkingAudienceList.value = []
        }
    }

    func enableLinkMicRequest(enable: Bool) {
        LiveKitLog.info("\(#file)", "\(#line)", "enableLinkMicRequest:[enable:\(enable)]")
        if enable {
            liveRoomInfo.interactionType.value = .link
        } else {
            for userInfo in liveRoomInfo.linkingAudienceList.value {
                kickUserOffSeatByAdmin(userId: userInfo.userId)
            }
            liveRoomInfo.linkingAudienceList.value = []

            for userInfo in liveKitStore.applyLinkAudienceList {
                responseRemoteRequestUser(userInfo, agree: false)
            }
            liveKitStore.applyLinkAudienceList = []
            liveRoomInfo.interactionType.value = .broadcast
        }
    }

    func getSeatList(onSuccess: TUISeatListResponseBlock? = nil, onError: TUIErrorBlock? = nil) {
        roomEngine.getSeatList { [weak self] seatList in
            onSuccess?(seatList)
            self?.handleSeatList(seatList: seatList, seatedList: [])
        } onError: { code, message in
            onError?(code, message)
        }
    }

    func takeSeat(onAccepted: TUIRequestAcceptedBlock? = nil,
                  onRejected: TUIRequestRejectedBlock? = nil,
                  onCancelled: TUIRequestCancelledBlock? = nil,
                  onTimeout: TUIRequestTimeoutBlock? = nil,
                  onError: TUIRequestErrorBlock? = nil) -> TUIRequest {
        LiveKitLog.info("\(#file)", "\(#line)", "takeSeat")
        let request = roomEngine.takeSeat(-1, timeout: timeOutNumber) { [weak self] requestId, userId in
            LiveKitLog.info("\(#file)", "\(#line)", "takeSeat:[onAccepted][requestId:\(requestId),userId:\(userId)]")
            self?.liveRoomInfo.interactionType.value = .link
            onAccepted?(requestId, userId)
        } onRejected: { requestId, userId, message in
            LiveKitLog.info("\(#file)", "\(#line)", "takeSeat:[onRejected][requestId:\(requestId),userId:\(userId),message:\(message)]")
            onRejected?(requestId, userId, message)
        } onCancelled: { requestId, userId in
            LiveKitLog.warn("\(#file)", "\(#line)", "takeSeat:[onCancelled][requestId:\(requestId),userId:\(userId)]")
            onCancelled?(requestId, userId)
        } onTimeout: { requestId, userId in
            LiveKitLog.warn("\(#file)", "\(#line)", "takeSeat:[onTimeout][requestId:\(requestId),userId:\(userId)]")
            onTimeout?(requestId, userId)
        } onError: { requestId, userId, code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "takeSeat:[onRejected][requestId:\(requestId),userId:\(userId),code:\(code),message:\(message)]")
            onError?(requestId, userId, code, message)
        }
        return request
    }

    func leaveSeat(onSuccess: TUISuccessBlock? = nil, onError: TUIErrorBlock? = nil) {
        LiveKitLog.info("\(#file)", "\(#line)", "leaveSeat")
        roomEngine.leaveSeat {
            LiveKitLog.info("\(#file)", "\(#line)", "leaveSeat:[onSuccess]")
            onSuccess?()
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "leaveSeat:[code:\(code),message:\(message)]")
            onError?(code, message)
        }
    }

    func kickUserOffSeatByAdmin(userId: String, onSuccess: TUISuccessBlock? = nil, onError: TUIErrorBlock? = nil) {
        LiveKitLog.info("\(#file)", "\(#line)", "kickUserOffSeatByAdmin:[userId:\(userId)]")
        roomEngine.kickUserOffSeatByAdmin(0, userId: userId) { [weak self] in
            LiveKitLog.info("\(#file)", "\(#line)", "kickUserOffSeatByAdmin:[onSuccess][userId:\(userId)]")
            guard let self = self else { return }
            onSuccess?()
            self.liveRoomInfo.linkingAudienceList.value = self.liveRoomInfo.linkingAudienceList.value.filter({ $0.userId != userId })
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "kickUserOffSeatByAdmin:[userId:\(userId),code:\(code),message:\(message)]")
            onError?(code, message)
        }
    }
}

// MARK: Store

extension RoomEngineService {
    func changeSelfRole(role: RoleType) {
        LiveKitLog.info("\(#file)", "\(#line)", "changeSelfRole:[role:\(role)]")
        liveKitStore.selfInfo.role.value = role
    }

    func changeSelfStatus(status: UserInteractionStatus) {
        LiveKitLog.info("\(#file)", "\(#line)", "changeSelfStatus:[status:\(status)]")
        liveKitStore.selfInfo.status.value = status
    }

    func changeUserLiveStatus(userLiveStatus: UserLiveStatus) {
        LiveKitLog.info("\(#file)", "\(#line)", "changeUserLiveStatus:[userLiveStatus:\(userLiveStatus)]")
        liveRoomInfo.userLiveStatus.value = userLiveStatus
    }
}

// MARK: Device

extension RoomEngineService {
    func openLocalMicrophone(onSuccess: TUISuccessBlock? = nil, onError: TUIErrorBlock? = nil) {
        let actionBlock = { [weak self] in
            guard let self = self else { return }
            self.roomEngine.openLocalMicrophone(.default) {
                LiveKitLog.info("\(#file)", "\(#line)", "openLocalMicrophone:[onSuccess]")
                self.liveKitStore.selfInfo.audioInfo.muteAudio.value = false
                onSuccess?()
            } onError: { code, message in
                LiveKitLog.error("\(#file)", "\(#line)", "openLocalMicrophone:[code:\(code),message:\(message)]")
                onError?(code, message)
            }
        }
        LiveKitLog.info("\(#file)", "\(#line)", "openLocalMicrophone")
        PermissionManager.microphoneRequestAccess { granted in
            if granted {
                actionBlock()
            } else {
                let logContent = "code:\(TUIError.microphoneNotAuthorized),message:The microphone does not have system authorization"
                LiveKitLog.error("\(#file)", "\(#line)", "openLocalMicrophone:[\(logContent)]")
                onError?(TUIError.microphoneNotAuthorized, "The microphone does not have system authorization")
            }
        }
    }

    func closeLocalMicrophone() {
        LiveKitLog.info("\(#file)", "\(#line)", "closeLocalMicrophone")
        roomEngine.closeLocalMicrophone()
    }

    func openLocalCamera(onSuccess: TUISuccessBlock? = nil, onError: TUIErrorBlock? = nil) {
        let actionBlock = { [weak self] in
            guard let self = self else { return }

            self.roomEngine.openLocalCamera(isFront: liveKitStore.selfInfo.videoInfo.isFrontCamera.value,
                                            quality: liveKitStore.selfInfo.videoInfo.videoQuality.value) {
                LiveKitLog.info("\(#file)", "\(#line)", "openLocalCamera:[onSuccess]")
                self.liveKitStore.selfInfo.videoInfo.isCameraOpened.value = true
                onSuccess?()
            } onError: { code, message in
                LiveKitLog.error("\(#file)", "\(#line)", "openLocalCamera:[code:\(code),message:\(message)]")
                onError?(code, message)
            }
        }
        LiveKitLog.info("\(#file)", "\(#line)", "openLocalCamera")
        PermissionManager.cameraRequestAccess { granted in
            if granted {
                actionBlock()
            } else {
                let log = "openLocalMicrophone:[code:\(TUIError.cameraNotAuthorized),message:The camera does not have system authorization]"
                LiveKitLog.error("\(#file)", "\(#line)", log)
                onError?(TUIError.cameraNotAuthorized, "The camera does not have system authorization")
            }
        }
    }

    func switchCamera() {
        let isFrontCamera = !liveKitStore.selfInfo.videoInfo.isFrontCamera.value
        LiveKitLog.info("\(#file)", "\(#line)", "switchCamera:[isFrontCamera:\(isFrontCamera)]")
        liveKitStore.selfInfo.videoInfo.isFrontCamera.value = isFrontCamera
        roomEngine.switchCamera(frontCamera: isFrontCamera)
    }
}

// MARK: Signal

extension RoomEngineService {
    func responseRemoteRequestUser(_ userInfo: UserInfo, agree: Bool, onSuccess: TUISuccessBlock? = nil, onError: TUIErrorBlock? = nil) {
        let logContent = "userId:\(userInfo.userId),requestId:\(userInfo.requestId),agree:\(agree)"
        LiveKitLog.info("\(#file)", "\(#line)", "responseRemoteRequestUser:[\(logContent)]")
        roomEngine.responseRemoteRequest(userInfo.requestId, agree: agree) {
            LiveKitLog.info("\(#file)", "\(#line)", "responseRemoteRequestUser:[onSuccess][\(logContent)]")
            self.liveKitStore.applyLinkAudienceList = self.liveKitStore.applyLinkAudienceList.filter({ $0.userId != userInfo.userId })
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "responseRemoteRequestUser:[\(logContent),code:\(code),message:\(message)]")
            onError?(code, message)
        }
    }

    func cancelRequest(_ requestId: String, onSuccess: TUISuccessBlock? = nil, onError: TUIErrorBlock? = nil) {
        LiveKitLog.info("\(#file)", "\(#line)", "cancelRequest:[requestId:\(requestId)]")
        roomEngine.cancelRequest(requestId) {
            LiveKitLog.info("\(#file)", "\(#line)", "cancelRequest:[onSuccess]")
            onSuccess?()
        } onError: { code, message in
            LiveKitLog.error("\(#file)", "\(#line)", "cancelRequest:[requestId:\(requestId),code:\(code),message:\(message)]")
            onError?(code, message)
        }
    }
}

// MARK: Video Render

extension RoomEngineService {
    func initLivingConfig() {
        LiveKitLog.info("\(#file)", "\(#line)", "initLivingConfig")
        roomEngine.enableGravitySensor(enable: true)
        roomEngine.setVideoResolutionMode(streamType: .cameraStream, resolutionMode: WindowUtils.isPortrait ? .portrait : .landscape)
        updateAudioQuality(quality: liveKitStore.selfInfo.audioInfo.audioQuality.value)

        let beautyInfo = liveKitStore.selfInfo.beautyInfo
        setBeautyLevel(beautyInfo.buffingLevel.value)
        setWhitenessLevel(beautyInfo.whitenessLevel.value)
        setRuddyLevel(beautyInfo.ruddyLevel.value)
        let enable = liveKitStore.selfInfo.videoInfo.isMirror.value
        let params = TRTCRenderParams()
        params.mirrorType = enable ? .enable : .disable
        trtcCloud.setLocalRenderParams(params)
        trtcCloud.setVideoEncoderMirror(enable)
    }

    func updateVideoQuality(quality: TUIVideoQuality) {
        LiveKitLog.info("\(#file)", "\(#line)", "updateVideoQuality:[quality:\(quality)]")
        roomEngine.updateVideoQuality(quality)
    }

    func updateAudioQuality(quality: TUIAudioQuality) {
        LiveKitLog.info("\(#file)", "\(#line)", "updateAudioQuality:[quality:\(quality)]")
        roomEngine.updateAudioQuality(quality)
    }

    func setLocalVideoView(view: UIView?) {
        LiveKitLog.info("\(#file)", "\(#line)", "setLocalVideoView:[view:\(String(describing: view))]")
        roomEngine.setLocalVideoView(view: view)
    }

    func setRemoteVideoView(userId: String, streamType: TUIVideoStreamType, view: UIView?) {
        LiveKitLog.info("\(#file)", "\(#line)", "setRemoteVideoView:[userId:\(userId),streamType:\(streamType),view:\(String(describing: view))]")
        roomEngine.setRemoteVideoView(userId: userId, streamType: streamType, view: view)
    }

    func startPlayRemoteVideo(userId: String,
                              streamType: TUIVideoStreamType,
                              onSuccess: TUISuccessBlock? = nil,
                              onLoading: TUIPlayOnLoadingBlock? = nil,
                              onError: TUIPlayOnErrorBlock? = nil) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "startPlayRemoteVideo:[userId:\(userId),streamType:\(streamType)]")
        roomEngine.startPlayRemoteVideo(userId: userId, streamType: streamType, onPlaying: { _ in
            LiveKitLog.info("\(#file)",
                            "\(#line)",
                            "startPlayRemoteVideo:[onSuccess][userId:\(userId),streamType:\(streamType)]")
            guard let onSuccess = onSuccess else { return }
            onSuccess()
        }, onLoading: { userId in
            LiveKitLog.info("\(#file)",
                            "\(#line)",
                            "startPlayRemoteVideo:[onLoading][userId:\(userId),streamType:\(streamType)]")
            guard let onLoading = onLoading else { return }
            onLoading(userId)
        }, onError: { userId, code, message in
            LiveKitLog.error("\(#file)",
                             "\(#line)",
                             "startPlayRemoteVideo:[userId:\(userId),streamType:\(streamType),code:\(code),message:\(message)]")
            guard let onError = onError else { return }
            onError(userId, code, message)
        })
    }

    func stopPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType) {
        LiveKitLog.info("\(#file)", "\(#line)", "stopPlayRemoteVideo:[userId:\(userId),streamType:\(streamType)]")
        roomEngine.stopPlayRemoteVideo(userId: userId, streamType: streamType)
    }

    func switchMirror() {
        let enable = !liveKitStore.selfInfo.videoInfo.isMirror.value
        LiveKitLog.info("\(#file)", "\(#line)", "switchMirror:[enable:\(enable)]")
        liveKitStore.selfInfo.videoInfo.isMirror.value = enable
        let params = TRTCRenderParams()
        params.mirrorType = enable ? .enable : .disable
        trtcCloud.setLocalRenderParams(params)
        trtcCloud.setVideoEncoderMirror(enable)
    }

    func stopLocalPreview() {
        LiveKitLog.info("\(#file)", "\(#line)", "stopLocalPreview]")
        trtcCloud.stopLocalPreview()
    }
}

// MARK: Beauty

extension RoomEngineService {
    func setBeautyLevel(_ beautyLevel: Float) {
        LiveKitLog.info("\(#file)", "\(#line)", "setBeautyLevel:[beautyLevel:\(beautyLevel)]")
        trtcCloud.getBeautyManager().setBeautyLevel(beautyLevel)
    }

    func setWhitenessLevel(_ whitenessLevel: Float) {
        LiveKitLog.info("\(#file)", "\(#line)", "setWhitenessLevel:[whitenessLevel:\(whitenessLevel)]")
        trtcCloud.getBeautyManager().setWhitenessLevel(whitenessLevel)
    }

    func setRuddyLevel(_ ruddyLevel: Float) {
        LiveKitLog.info("\(#file)", "\(#line)", "setRuddyLevel:[ruddyLevel:\(ruddyLevel)]")
        trtcCloud.getBeautyManager().setRuddyLevel(ruddyLevel)
    }
}

// MARK: Audio

extension RoomEngineService {
    func muteAllRemoteAudio(isMute: Bool) {
        let roomId = liveRoomInfo.roomId.value
        LiveKitLog.info("\(#file)", "\(#line)", "muteAllRemoteAudio:[isMute:\(isMute),roomId:\(roomId)]")
        trtcCloud.muteLocalAudio(isMute)
    }
}

// MARK: Audio Effect

extension RoomEngineService {
    func startPlayMusic(_ musicInfo: MusicInfo) {
        let param = TXAudioMusicParam()
        param.publish = true
        param.loopCount = Int.max
        param.id = musicInfo.id
        param.path = musicInfo.path
        let log = "startPlayMusic:[id:\(param.id),path:\(param.path),publish:\(param.publish),loopCount:\(param.loopCount)]"
        LiveKitLog.info("\(#file)", "\(#line)", log)
        trtcCloud.getAudioEffectManager().startPlayMusic(param, onStart: nil, onProgress: nil)
        trtcCloud.getAudioEffectManager().setAllMusicVolume(liveKitStore.selfInfo.audioInfo.musicVolume.value)
        trtcCloud.getAudioEffectManager().setMusicPitch(musicInfo.id, pitch: Double(musicInfo.pitch))
    }

    func stopPlayMusic(_ id: Int32) {
        LiveKitLog.info("\(#file)", "\(#line)", "stopPlayMusic:[id:\(id)]")
        trtcCloud.getAudioEffectManager().stopPlayMusic(id)
    }

    func setMusicVolume(_ volume: Int) {
        LiveKitLog.info("\(#file)", "\(#line)", "setMusicVolume:[volume:\(volume)]")
        trtcCloud.getAudioEffectManager().setAllMusicVolume(volume)
    }

    func setVoiceVolume(_ volume: Int) {
        LiveKitLog.info("\(#file)", "\(#line)", "setVoiceVolume:[volume:\(volume)]")
        trtcCloud.getAudioEffectManager().setVoiceEarMonitorVolume(volume)
        trtcCloud.getAudioEffectManager().setVoiceVolume(volume)
    }

    func setMusicPitch(_ id: Int32, pitch: Double) {
        LiveKitLog.info("\(#file)", "\(#line)", "setMusicPitch:[id:\(id),pitch:\(pitch)]")
        trtcCloud.getAudioEffectManager().setMusicPitch(id, pitch: pitch)
    }

    func enableVoiceEarMonitor(enable: Bool) {
        LiveKitLog.info("\(#file)", "\(#line)", "enableVoiceEarMonitor:[enable:\(enable)]")
        trtcCloud.getAudioEffectManager().enableVoiceEarMonitor(enable)
    }

    func setVoiceChangerType(_ changerType: TXVoiceChangeType) {
        LiveKitLog.info("\(#file)", "\(#line)", "setVoiceChangerType:[changerType:\(changerType)]")
        liveKitStore.selfInfo.audioInfo.changerType.value = changerType
        trtcCloud.getAudioEffectManager().setVoiceChangerType(changerType)
    }

    func setVoiceReverbType(_ reverbType: TXVoiceReverbType) {
        LiveKitLog.info("\(#file)", "\(#line)", "setVoiceReverbType:[reverbType:\(reverbType)]")
        liveKitStore.selfInfo.audioInfo.reverbType.value = reverbType
        trtcCloud.getAudioEffectManager().setVoiceReverbType(reverbType)
    }
}

// MARK: trtcCloud

extension RoomEngineService {
    private var trtcCloud: TRTCCloud {
        return roomEngine.getTRTCCloud()
    }
}
