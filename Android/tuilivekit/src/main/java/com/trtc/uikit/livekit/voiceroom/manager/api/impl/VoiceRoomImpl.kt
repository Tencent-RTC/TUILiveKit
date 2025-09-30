package com.trtc.uikit.livekit.voiceroom.manager.api.impl

import com.google.gson.Gson
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver
import com.tencent.imsdk.v2.V2TIMFollowOperationResult
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult
import com.tencent.imsdk.v2.V2TIMFriendshipListener
import com.tencent.imsdk.v2.V2TIMManager
import com.tencent.imsdk.v2.V2TIMValueCallback
import com.tencent.trtc.TRTCCloud
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom

class VoiceRoomImpl : IVoiceRoom {
    private val logger = LiveKitLogger.getVoiceRoomLogger("VoiceRoomImpl")

    private val roomEngine: TUIRoomEngine = TUIRoomEngine.sharedInstance()
    private val trtcCloud: TRTCCloud = roomEngine.getTRTCCloud()
    private val tuILiveListManager: TUILiveListManager =
        roomEngine.getExtension(LIVE_LIST_MANAGER) as TUILiveListManager

    override fun addRoomEngineObserver(observer: TUIRoomObserver) {
        logger.info("addRoomEngineObserver:[observer:${observer.hashCode()}]")
        roomEngine.addObserver(observer)
    }

    override fun removeRoomEngineObserver(observer: TUIRoomObserver) {
        logger.info("removeRoomEngineObserver:[observer:${observer.hashCode()}]")
        roomEngine.removeObserver(observer)
    }

    override fun addLiveListManagerObserver(observer: TUILiveListManager.Observer) {
        logger.info("addLiveListManagerObserver:[observer:${observer.hashCode()}]")
        tuILiveListManager.addObserver(observer)
    }

    override fun removeLiveListManagerObserver(observer: TUILiveListManager.Observer) {
        logger.info("removeLiveListManagerObserver:[observer:${observer.hashCode()}]")
        tuILiveListManager.removeObserver(observer)
    }

    override fun addFriendListener(listener: V2TIMFriendshipListener) {
        logger.info("addFriendListener:[listener:${listener.hashCode()}]")
        V2TIMManager.getFriendshipManager().addFriendListener(listener)
    }

    override fun removeFriendListener(listener: V2TIMFriendshipListener) {
        logger.info("removeFriendListener:[listener:${listener.hashCode()}]")
        V2TIMManager.getFriendshipManager().removeFriendListener(listener)
    }

    override fun getLiveInfo(roomId: String, callback: TUILiveListManager.LiveInfoCallback) {
        tuILiveListManager.getLiveInfo(roomId, object : TUILiveListManager.LiveInfoCallback {
            override fun onSuccess(liveInfo: LiveInfo) {
                logger.info("getLiveInfo:[onSuccess:[liveInfo${Gson().toJson(liveInfo)}]]")
                callback.onSuccess(liveInfo)
            }

            override fun onError(error: TUICommonDefine.Error, message: String) {
                logger.error("getLiveInfo:[onError:[error:$error,message:$message]]")
                callback.onError(error, message)
            }
        })
    }

    /* ****************************************** 座位业务 *******************************************/
    override fun getSeatList(callback: TUIRoomDefine.GetSeatListCallback) {
        logger.info("getSeatList:[]")
        roomEngine.getSeatList(object : TUIRoomDefine.GetSeatListCallback {
            override fun onSuccess(list: List<TUIRoomDefine.SeatInfo>) {
                logger.info("getSeatList:[onSuccess]")
                callback?.onSuccess(list)
            }

            override fun onError(error: TUICommonDefine.Error, message: String) {
                logger.error("getSeatList:[onError:[error:$error,message:$message]]")
                callback?.onError(error, message)
            }
        })
    }

    /* ****************************************** 用户业务 *******************************************/
    override fun getUserList(nextSequence: Long, callback: TUIRoomDefine.GetUserListCallback) {
        logger.info("getUserList:[nextSequence:$nextSequence]")
        roomEngine.getUserList(nextSequence, object : TUIRoomDefine.GetUserListCallback {
            override fun onSuccess(userListResult: TUIRoomDefine.UserListResult) {
                logger.info("getUserList:[onSuccess]")
                callback.onSuccess(userListResult)
            }

            override fun onError(error: TUICommonDefine.Error, message: String) {
                logger.error("getUserList:[onError:[error:$error,message:$message]]")
                callback.onError(error, message)
            }
        })
    }

    override fun getUserInfo(userId: String, callback: TUIRoomDefine.GetUserInfoCallback) {
        logger.info("getUserInfo:[userId:$userId]")
        roomEngine.getUserInfo(userId, object : TUIRoomDefine.GetUserInfoCallback {
            override fun onSuccess(userInfo: TUIRoomDefine.UserInfo) {
                logger.info("getUserInfo:[onSuccess]")
                callback.onSuccess(userInfo)
            }

            override fun onError(error: TUICommonDefine.Error, message: String) {
                logger.error("getUserInfo:[onError:[error:$error,message:$message]]")
                callback.onError(error, message)
            }
        })
    }

    override fun muteAllRemoteAudio(isMute: Boolean) {
        logger.info("muteAllRemoteAudio:[isMute:$isMute]")
        trtcCloud.muteAllRemoteAudio(isMute)
    }

    /* ****************************************** IM业务 *******************************************/
    override fun followUser(
        userIDList: List<String>,
        callback: V2TIMValueCallback<List<V2TIMFollowOperationResult>>
    ) {
        logger.info("followUser:[userIDList:$userIDList]")
        V2TIMManager.getFriendshipManager().followUser(
            userIDList,
            object : V2TIMValueCallback<List<V2TIMFollowOperationResult>> {
                override fun onSuccess(results: List<V2TIMFollowOperationResult>) {
                    logger.info("followUser:[onSuccess:[results:${Gson().toJson(results)}]]")
                    callback.onSuccess(results)
                }

                override fun onError(code: Int, message: String) {
                    logger.error("followUser:[onError:[code:$code,message:$message]]")
                    callback.onError(code, message)
                }
            })
    }

    override fun unfollowUser(
        userIDList: List<String>,
        callback: V2TIMValueCallback<List<V2TIMFollowOperationResult>>
    ) {
        logger.info("unfollowUser:[userIDList:$userIDList]")
        V2TIMManager.getFriendshipManager().unfollowUser(
            userIDList,
            object : V2TIMValueCallback<List<V2TIMFollowOperationResult>> {
                override fun onSuccess(results: List<V2TIMFollowOperationResult>) {
                    logger.info("unfollowUser:[onSuccess:[results:${Gson().toJson(results)}]]")
                    callback.onSuccess(results)
                }

                override fun onError(code: Int, message: String) {
                    logger.error("unfollowUser:[onError:[code:$code,message:$message]]")
                    callback.onError(code, message)
                }
            })
    }

    override fun checkFollowType(
        userIDList: List<String>,
        callback: V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>
    ) {
        logger.info("checkFollowType:[userIDList:$userIDList]")
        V2TIMManager.getFriendshipManager().checkFollowType(
            userIDList,
            object : V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>> {
                override fun onSuccess(results: List<V2TIMFollowTypeCheckResult>) {
                    logger.info("checkFollowType:[onSuccess:[results:${Gson().toJson(results)}]]")
                    callback.onSuccess(results)
                }

                override fun onError(code: Int, message: String) {
                    logger.error("checkFollowType:[onSuccess:[code:$code,message:$message]]")
                    callback.onError(code, message)
                }
            })
    }

    override fun setLiveInfo(
        liveInfo: LiveInfo,
        flagList: List<LiveModifyFlag>,
        callback: TUIRoomDefine.ActionCallback
    ) {
        logger.info("setLiveInfo:[liveInfo:${Gson().toJson(liveInfo)},flag:$flagList]")
        tuILiveListManager.setLiveInfo(liveInfo, flagList, object : TUIRoomDefine.ActionCallback {
            override fun onSuccess() {
                logger.info("setLiveInfo:[onSuccess]")
                callback.onSuccess()
            }

            override fun onError(error: TUICommonDefine.Error, message: String) {
                logger.error("setLiveInfo:[onError:[error:$error,message:$message]]")
                callback.onError(error, message)
            }
        })
    }
}