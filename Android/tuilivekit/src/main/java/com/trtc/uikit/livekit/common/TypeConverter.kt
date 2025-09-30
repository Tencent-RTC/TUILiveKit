package com.trtc.uikit.livekit.common

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.cloud.tuikit.engine.extension.TUIRoomDeviceManager
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import io.trtc.tuikit.atomicxcore.api.AudioRoute
import io.trtc.tuikit.atomicxcore.api.BattleConfig
import io.trtc.tuikit.atomicxcore.api.BattleInfo
import io.trtc.tuikit.atomicxcore.api.DeviceError
import io.trtc.tuikit.atomicxcore.api.DeviceStatus
import io.trtc.tuikit.atomicxcore.api.LiveEndedReason
import io.trtc.tuikit.atomicxcore.api.LiveInfo
import io.trtc.tuikit.atomicxcore.api.LiveKickedOutReason
import io.trtc.tuikit.atomicxcore.api.LiveUserInfo
import io.trtc.tuikit.atomicxcore.api.NetworkInfo
import io.trtc.tuikit.atomicxcore.api.NetworkQuality
import io.trtc.tuikit.atomicxcore.api.RegionInfo
import io.trtc.tuikit.atomicxcore.api.Role
import io.trtc.tuikit.atomicxcore.api.SeatInfo
import io.trtc.tuikit.atomicxcore.api.SeatUserInfo
import io.trtc.tuikit.atomicxcore.api.TakeSeatMode
import io.trtc.tuikit.atomicxcore.api.VideoQuality
import java.util.Collections

fun liveInfoFromEngineLiveInfo(liveInfo: TUILiveListManager.LiveInfo): LiveInfo {
    val owner = LiveUserInfo().apply {
        userId = liveInfo.ownerId ?: ""
        userName = liveInfo.ownerName ?: ""
        avatarURL = liveInfo.ownerAvatarUrl ?: ""
    }
    return LiveInfo(
        liveInfo.roomId ?: "",
        liveInfo.name ?: "",
        liveInfo.notice ?: "",
        liveInfo.isMessageDisableForAllUser,
        liveInfo.isPublicVisible,
        liveInfo.isSeatEnabled,
        liveInfo.keepOwnerOnSeat,
        liveInfo.maxSeatCount,
        seatModeFromEngineSeatMode(liveInfo.seatMode),
        liveInfo.seatLayoutTemplateId,
        liveInfo.coverUrl ?: "",
        liveInfo.backgroundUrl ?: "",
        liveInfo.categoryList ?: emptyList(),
        liveInfo.activityStatus,
        owner,
        liveInfo.createTime,
        liveInfo.viewCount,
        true,
        emptyMap()
    )
}

fun liveInfoFromEngineRoomInfo(roomInfo: TUIRoomDefine.RoomInfo): TUILiveListManager.LiveInfo {
    val liveInfo = TUILiveListManager.LiveInfo()
    liveInfo.roomId = roomInfo.roomId
    liveInfo.ownerId = roomInfo.ownerId
    liveInfo.ownerName = roomInfo.ownerName
    liveInfo.ownerAvatarUrl = roomInfo.ownerAvatarUrl
    liveInfo.name = roomInfo.name
    liveInfo.isMessageDisableForAllUser = roomInfo.isMessageDisableForAllUser
    liveInfo.isSeatEnabled = roomInfo.isSeatEnabled
    liveInfo.keepOwnerOnSeat = roomInfo.keepOwnerOnSeat
    liveInfo.maxSeatCount = roomInfo.maxSeatCount
    liveInfo.seatMode = roomInfo.seatMode
    liveInfo.createTime = roomInfo.createTime
    liveInfo.categoryList = Collections.EMPTY_LIST as List<Int?>?
    return liveInfo
}

fun liveInfoToEngineLiveInfo(liveInfo: LiveInfo): TUILiveListManager.LiveInfo {
    return TUILiveListManager.LiveInfo().apply {
        roomId = liveInfo.liveId
        name = liveInfo.liveName
        notice = liveInfo.notice
        isMessageDisableForAllUser = liveInfo.isMessageDisable
        isPublicVisible = liveInfo.isPublicVisible
        isSeatEnabled = liveInfo.isSeatEnabled
        keepOwnerOnSeat = liveInfo.keepOwnerOnSeat
        maxSeatCount = liveInfo.maxSeatCount
        seatMode = seatModeToEngineSeatMode(liveInfo.seatMode)
        seatLayoutTemplateId = liveInfo.seatLayoutTemplateId
        coverUrl = liveInfo.coverURL
        backgroundUrl = liveInfo.backgroundURL
        categoryList = liveInfo.categoryList
        activityStatus = liveInfo.activityStatus
        ownerId = liveInfo.liveOwner.userId
        ownerName = liveInfo.liveOwner.userName
        ownerAvatarUrl = liveInfo.liveOwner.avatarURL
        roomInfo = TUIRoomDefine.RoomInfo().apply {
            roomId = liveInfo.liveId
            ownerId = liveInfo.liveOwner.userId
            ownerName = liveInfo.liveOwner.userName
            ownerAvatarUrl = liveInfo.liveOwner.avatarURL
            name = liveInfo.liveName
            isMessageDisableForAllUser = liveInfo.isMessageDisable
            isSeatEnabled = liveInfo.isSeatEnabled
            keepOwnerOnSeat = liveInfo.keepOwnerOnSeat
            maxSeatCount = liveInfo.maxSeatCount
            seatMode =
                if (liveInfo.seatMode == TakeSeatMode.FREE) TUIRoomDefine.SeatMode.FREE_TO_TAKE else
                    TUIRoomDefine.SeatMode.APPLY_TO_TAKE
            createTime = liveInfo.createTime
        }
    }
}

fun seatModeFromEngineSeatMode(seatMode: TUIRoomDefine.SeatMode): TakeSeatMode {
    return when (seatMode) {
        TUIRoomDefine.SeatMode.FREE_TO_TAKE -> TakeSeatMode.FREE
        TUIRoomDefine.SeatMode.APPLY_TO_TAKE -> TakeSeatMode.APPLY
    }
}

fun seatModeToEngineSeatMode(seatMode: TakeSeatMode): TUIRoomDefine.SeatMode {
    return when (seatMode) {
        TakeSeatMode.FREE -> TUIRoomDefine.SeatMode.FREE_TO_TAKE
        TakeSeatMode.APPLY -> TUIRoomDefine.SeatMode.APPLY_TO_TAKE
    }
}

fun modifyFlagToEngineModifyFlag(modifyFlagList: List<LiveInfo.ModifyFlag>): List<TUILiveListManager.LiveModifyFlag> {
    val engineModifyFlag = mutableListOf<TUILiveListManager.LiveModifyFlag>()
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.LIVE_NAME)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.NAME)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.NOTICE)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.NOTICE)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.IS_MESSAGE_DISABLE)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.DISABLE_MESSAGE)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.IS_PUBLIC_VISIBLE)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.PUBLISH)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.SEAT_MODE)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.TAKE_SEAT_MODE)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.COVER_URL)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.COVER_URL)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.BACKGROUND_URL)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.BACKGROUND_URL)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.CATEGORY_LIST)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.CATEGORY)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.ACTIVITY_STATUS)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.ACTIVITY_STATUS)
    }
    if (modifyFlagList.contains(LiveInfo.ModifyFlag.SEAT_LAYOUT_TEMPLATE_ID)) {
        engineModifyFlag.add(TUILiveListManager.LiveModifyFlag.SEAT_LAYOUT_TEMPLATE_ID)
    }
    return engineModifyFlag
}

fun convertToBattleConfig(battleConfig: TUILiveBattleManager.BattleConfig): BattleConfig {
    return BattleConfig(
        battleConfig.duration,
        battleConfig.needResponse,
        battleConfig.extensionInfo
    )
}

fun convertToBattleConfig(battleConfig: BattleConfig?): TUILiveBattleManager.BattleConfig {
    if (battleConfig == null) {
        return TUILiveBattleManager.BattleConfig()
    }
    return TUILiveBattleManager.BattleConfig().apply {
        duration = battleConfig.duration
        needResponse = battleConfig.needResponse
        extensionInfo = battleConfig.extensionInfo
    }
}

fun convertToBattleInfo(battleInfo: TUILiveBattleManager.BattleInfo): BattleInfo {
    return BattleInfo(
        battleInfo.battleId,
        convertToBattleConfig(battleInfo.config),
        battleInfo.startTime,
        battleInfo.endTime
    )
}

fun convertToSeatUserInfo(battleInfo: TUILiveBattleManager.BattleUser?): SeatUserInfo {
    if (battleInfo == null) {
        return SeatUserInfo()
    }
    return SeatUserInfo(
        battleInfo.userId,
        battleInfo.userName,
        battleInfo.avatarUrl,
        Role.GENERAL_USER,
        battleInfo.roomId,
        DeviceStatus.OFF,
        true,
        DeviceStatus.OFF,
        true
    )
}

fun convertToSeatFullInfo(data: SeatInfo): TUIRoomDefine.SeatFullInfo {
    return TUIRoomDefine.SeatFullInfo().apply {
        roomId = data.userInfo.liveId
        seatIndex = data.index
        isSeatLocked = data.isLocked
        userId = data.userInfo.userId
        userName = data.userInfo.name
        userAvatar = data.userInfo.avatarURL
        userMicrophoneStatus =
            if (data.userInfo.microphoneStatus == DeviceStatus.ON) TUIRoomDefine.DeviceStatus.OPENED else TUIRoomDefine.DeviceStatus.CLOSED_BY_SELF
        userCameraStatus =
            if (data.userInfo.cameraStatus == DeviceStatus.ON) TUIRoomDefine.DeviceStatus.OPENED else
                TUIRoomDefine.DeviceStatus.CLOSED_BY_SELF
        userSuspendStatus = TUIRoomDefine.SuspendStatus.NONE
        x = data.region.x
        y = data.region.y
        width = data.region.w
        height = data.region.h
        zorder = data.region.zorder
    }
}

fun convertToSeatInfo(seatFullInfo: TUIRoomDefine.SeatFullInfo): SeatInfo {
    val seatUserInfo = SeatUserInfo(
        userId = seatFullInfo.userId,
        name = seatFullInfo.userName,
        avatarURL = seatFullInfo.userAvatar,
        liveId = seatFullInfo.roomId,
        microphoneStatus = convertToDeviceStatus(seatFullInfo.userMicrophoneStatus),
        cameraStatus = convertToDeviceStatus(seatFullInfo.userCameraStatus)
    )
    val regionInfo = RegionInfo(
        x = seatFullInfo.x,
        y = seatFullInfo.y,
        w = seatFullInfo.width,
        h = seatFullInfo.height,
        zorder = seatFullInfo.zorder
    )
    return SeatInfo().apply {
        index = seatFullInfo.seatIndex
        isLocked = seatFullInfo.isSeatLocked
        userInfo = seatUserInfo
        region = regionInfo
    }
}

fun convertToSeatUserInfo(seatFullInfo: TUIRoomDefine.SeatFullInfo): SeatUserInfo {
    return SeatUserInfo(
        userId = seatFullInfo.userId,
        name = seatFullInfo.userName,
        avatarURL = seatFullInfo.userAvatar,
        liveId = seatFullInfo.roomId,
        microphoneStatus = convertToDeviceStatus(seatFullInfo.userMicrophoneStatus),
        cameraStatus = convertToDeviceStatus(seatFullInfo.userCameraStatus)
    )
}

fun convertToDeviceStatus(status: TUIRoomDefine.DeviceStatus): DeviceStatus {
    return when (status) {
        TUIRoomDefine.DeviceStatus.OPENED -> DeviceStatus.ON
        else -> DeviceStatus.OFF
    }
}

fun convertToUserInfo(seatInfo: TUIRoomDefine.SeatInfo?): TUIRoomDefine.UserInfo {
    val userInfo = TUIRoomDefine.UserInfo()
    if (seatInfo != null) {
        userInfo.userId = seatInfo.userId
        userInfo.userName = seatInfo.userName
        userInfo.avatarUrl = seatInfo.avatarUrl
    }
    return userInfo
}

fun convertToUserInfo(userInfo: TUIRoomDefine.UserInfo): LiveUserInfo {
    return LiveUserInfo(
        userId = userInfo.userId,
        userName = userInfo.userName,
        avatarURL = userInfo.avatarUrl
    )
}

fun convertToUserInfo(request: TUIRoomDefine.Request): TUIRoomDefine.UserInfo {
    val userInfo = TUIRoomDefine.UserInfo()
    userInfo.userId = request.userId
    userInfo.userName = request.userName
    userInfo.avatarUrl = request.avatarUrl
    return userInfo
}

fun convertToUserInfo(liveUserInfo: LiveUserInfo): TUIRoomDefine.UserInfo {
    val userInfo = TUIRoomDefine.UserInfo()
    userInfo.userId = liveUserInfo.userId
    userInfo.userName = liveUserInfo.userName
    userInfo.avatarUrl = liveUserInfo.avatarURL
    return userInfo
}

fun convertToUserInfo(audienceInfo: SeatUserInfo): TUIRoomDefine.UserInfo {
    val userInfo = TUIRoomDefine.UserInfo()
    userInfo.userId = audienceInfo.userId
    userInfo.userName = audienceInfo.name
    userInfo.avatarUrl = audienceInfo.avatarURL
    return userInfo
}

fun convertToUserInfo(login: TUIRoomDefine.LoginUserInfo): TUIRoomDefine.UserInfo {
    return TUIRoomDefine.UserInfo().apply {
        userId = login.userId
        userName = login.userName
        avatarUrl = login.avatarUrl
    }
}

fun convertToBattleUserInfo(battleUserInfo: SeatUserInfo): TUILiveBattleManager.BattleUser {
    return TUILiveBattleManager.BattleUser().apply {
        roomId = battleUserInfo.liveId
        userId = battleUserInfo.userId
        userName = battleUserInfo.name
        avatarUrl = battleUserInfo.avatarURL
    }
}

fun convertToSeatInfo(audienceInfo: SeatInfo): TUIRoomDefine.SeatInfo {
    return TUIRoomDefine.SeatInfo().apply {
        index = audienceInfo.index
        userId = audienceInfo.userInfo.userId
        userName = audienceInfo.userInfo.name
        nameCard = audienceInfo.userInfo.name
        avatarUrl = audienceInfo.userInfo.avatarURL
        isLocked = audienceInfo.isLocked
        isVideoLocked = audienceInfo.userInfo.allowOpenCamera
        isAudioLocked = audienceInfo.userInfo.allowOpenMicrophone
    }
}

fun convertToSeatInfo(seatInfo: TUIRoomDefine.SeatInfo, hasAudio: Boolean): SeatInfo {
    return SeatInfo().apply {
        index = seatInfo.index
        isLocked = seatInfo.isLocked
        userInfo = convertToSeatUserInfo(seatInfo, hasAudio)
    }
}

fun endedReasonFromEngineRoomDismissedReason(reason: TUIRoomDefine.RoomDismissedReason): LiveEndedReason {
    return when (reason) {
        TUIRoomDefine.RoomDismissedReason.BY_OWNER -> LiveEndedReason.ENDED_BY_HOST
        TUIRoomDefine.RoomDismissedReason.BY_SERVER -> LiveEndedReason.ENDED_BY_SERVER
    }
}

fun kickedOutReasonFromEngineKickedOutOfRoomReason(reason: TUIRoomDefine.KickedOutOfRoomReason): LiveKickedOutReason {
    return when (reason) {
        TUIRoomDefine.KickedOutOfRoomReason.BY_ADMIN -> LiveKickedOutReason.BY_ADMIN
        TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE -> LiveKickedOutReason.BY_LOGGED_ON_OTHER_DEVICE
        TUIRoomDefine.KickedOutOfRoomReason.BY_SERVER -> LiveKickedOutReason.BY_SERVER
        TUIRoomDefine.KickedOutOfRoomReason.NETWORK_DISCONNECTED -> LiveKickedOutReason.FOR_NETWORK_DISCONNECTED
        TUIRoomDefine.KickedOutOfRoomReason.JOIN_ROOM_STATUS_INVALID_DURING_OFFLINE -> LiveKickedOutReason.FOR_JOIN_ROOM_STATUS_INVALID_DURING_OFFLINE
        TUIRoomDefine.KickedOutOfRoomReason.COUNT_OF_JOINED_ROOMS_EXCEED_LIMIT -> LiveKickedOutReason.FOR_COUNT_OF_JOINED_ROOMS_EXCEED_LIMIT
    }
}

fun deviceErrorFromEngineError(error: TUICommonDefine.Error): DeviceError {
    return when (error) {
        TUICommonDefine.Error.SUCCESS -> DeviceError.NO_ERROR
        TUICommonDefine.Error.CAMERA_DEVICE_EMPTY -> DeviceError.NO_DEVICE_DETECTED
        TUICommonDefine.Error.MICROPHONE_DEVICE_EMPTY -> DeviceError.NO_DEVICE_DETECTED
        TUICommonDefine.Error.OPEN_CAMERA_NEED_PERMISSION_FROM_ADMIN,
        TUICommonDefine.Error.OPEN_MICROPHONE_NEED_PERMISSION_FROM_ADMIN,
        TUICommonDefine.Error.OPEN_SCREEN_SHARE_NEED_PERMISSION_FROM_ADMIN -> DeviceError.NO_SYSTEM_PERMISSION

        else -> DeviceError.UNKNOWN_ERROR
    }
}

fun audioRouteToEngineAudioRoute(audioRoute: AudioRoute): TUIRoomDeviceManager.AudioRoute {
    return when (audioRoute) {
        AudioRoute.EARPIECE -> TUIRoomDeviceManager.AudioRoute.EARPIECE
        AudioRoute.SPEAKERPHONE -> TUIRoomDeviceManager.AudioRoute.SPEAKERPHONE
    }
}

fun videoQualityToEngineVideoQuality(videoQuality: VideoQuality): TUIRoomDefine.VideoQuality {
    return when (videoQuality) {
        VideoQuality.QUALITY_360P -> TUIRoomDefine.VideoQuality.Q_360P
        VideoQuality.QUALITY_540P -> TUIRoomDefine.VideoQuality.Q_540P
        VideoQuality.QUALITY_720P -> TUIRoomDefine.VideoQuality.Q_720P
        VideoQuality.QUALITY_1080P -> TUIRoomDefine.VideoQuality.Q_1080P
    }
}

fun networkInfoFromEngineNetworkInfo(networkInfo: TUICommonDefine.NetworkInfo): NetworkInfo {
    return NetworkInfo(
        userId = networkInfo.userId ?: "",
        quality = networkQualityFromEngineNetworkQuality(networkInfo.quality),
        upLoss = networkInfo.upLoss,
        downLoss = networkInfo.downLoss,
        delay = networkInfo.delay
    )
}

fun networkQualityFromEngineNetworkQuality(networkQuality: TUICommonDefine.NetworkQuality): NetworkQuality {
    return when (networkQuality) {
        TUICommonDefine.NetworkQuality.UNKNOWN -> NetworkQuality.UNKNOWN
        TUICommonDefine.NetworkQuality.EXCELLENT -> NetworkQuality.EXCELLENT
        TUICommonDefine.NetworkQuality.GOOD -> NetworkQuality.GOOD
        TUICommonDefine.NetworkQuality.POOR -> NetworkQuality.POOR
        TUICommonDefine.NetworkQuality.BAD -> NetworkQuality.BAD
        TUICommonDefine.NetworkQuality.VERY_BAD -> NetworkQuality.VERY_BAD
        TUICommonDefine.NetworkQuality.DOWN -> NetworkQuality.DOWN
    }
}

fun convertToSeatUserInfo(seatInfo: TUIRoomDefine.SeatInfo, hasAudio: Boolean): SeatUserInfo {
    return SeatUserInfo(
        userId = seatInfo.userId,
        name = seatInfo.userName,
        avatarURL = seatInfo.avatarUrl,
        allowOpenMicrophone = !seatInfo.isAudioLocked,
        allowOpenCamera = !seatInfo.isVideoLocked,
        microphoneStatus = if (hasAudio) {
            DeviceStatus.ON
        } else {
            DeviceStatus.OFF
        }
    )
}

fun convertToLiveUserInfo(request: TUIRoomDefine.Request): LiveUserInfo {
    val userInfo = LiveUserInfo().apply {
        userId = request.userId
        userName = request.userName
        avatarURL = request.avatarUrl
    }
    return userInfo
}

fun convertToConnectionUser(request: SeatUserInfo?): ConnectionUser {
    val userInfo = ConnectionUser().apply {
        userId = request?.userId
        userName = request?.name
        avatarUrl = request?.avatarURL
        roomId = request?.liveId
    }
    return userInfo
}

fun convertToBattleUser(userInfo: SeatUserInfo): TUILiveBattleManager.BattleUser {
    return TUILiveBattleManager.BattleUser().apply {
        roomId = userInfo.liveId
        userId = userInfo.userId
        userName = userInfo.name
        avatarUrl = userInfo.avatarURL
        score = 0
    }
}

fun convertToSeatUserInfo(seatInfo: ConnectionUser): SeatUserInfo {
    return SeatUserInfo(
        liveId = seatInfo.roomId,
        userId = seatInfo.userId,
        name = seatInfo.userName,
        avatarURL = seatInfo.avatarUrl,
    )
}

fun convertToKickedOutReason(reason: LiveKickedOutReason): TUIRoomDefine.KickedOutOfRoomReason {
    return when (reason) {
        LiveKickedOutReason.BY_ADMIN -> TUIRoomDefine.KickedOutOfRoomReason.BY_ADMIN
        LiveKickedOutReason.BY_LOGGED_ON_OTHER_DEVICE -> TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE
        LiveKickedOutReason.BY_SERVER -> TUIRoomDefine.KickedOutOfRoomReason.BY_SERVER
        LiveKickedOutReason.FOR_NETWORK_DISCONNECTED -> TUIRoomDefine.KickedOutOfRoomReason.NETWORK_DISCONNECTED
        LiveKickedOutReason.FOR_JOIN_ROOM_STATUS_INVALID_DURING_OFFLINE -> TUIRoomDefine.KickedOutOfRoomReason.JOIN_ROOM_STATUS_INVALID_DURING_OFFLINE
        LiveKickedOutReason.FOR_COUNT_OF_JOINED_ROOMS_EXCEED_LIMIT -> TUIRoomDefine.KickedOutOfRoomReason.COUNT_OF_JOINED_ROOMS_EXCEED_LIMIT
    }
}