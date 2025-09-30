package com.trtc.uikit.livekit.voiceroom.manager.api

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver
import com.tencent.imsdk.v2.V2TIMFollowOperationResult
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult
import com.tencent.imsdk.v2.V2TIMFriendshipListener
import com.tencent.imsdk.v2.V2TIMValueCallback

interface IVoiceRoom {
    fun addRoomEngineObserver(observer: TUIRoomObserver)
    fun removeRoomEngineObserver(observer: TUIRoomObserver)

    fun addLiveListManagerObserver(observer: TUILiveListManager.Observer)
    fun removeLiveListManagerObserver(observer: TUILiveListManager.Observer)

    fun addFriendListener(listener: V2TIMFriendshipListener)
    fun removeFriendListener(listener: V2TIMFriendshipListener)

    /* ****************************************** 房间业务 *******************************************/
    fun getLiveInfo(roomId: String, callback: TUILiveListManager.LiveInfoCallback)

    /* ****************************************** 座位业务 *******************************************/
    fun getSeatList(callback: TUIRoomDefine.GetSeatListCallback)

    /* ****************************************** 用户业务 *******************************************/
    fun getUserList(nextSequence: Long, callback: TUIRoomDefine.GetUserListCallback)
    fun getUserInfo(userId: String, callback: TUIRoomDefine.GetUserInfoCallback)
    fun muteAllRemoteAudio(isMute: Boolean)

    /* ****************************************** IM业务 *******************************************/
    fun followUser(userIDList: List<String>, callback: V2TIMValueCallback<List<V2TIMFollowOperationResult>>)

    fun unfollowUser(userIDList: List<String>, callback: V2TIMValueCallback<List<V2TIMFollowOperationResult>>)

    fun checkFollowType(userIDList: List<String>, callback: V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>)

    /* ****************************************** 插件 - 房间列表 *******************************************/
    fun setLiveInfo(liveInfo: LiveInfo, flagList: List<LiveModifyFlag>, callback: TUIRoomDefine.ActionCallback)
}