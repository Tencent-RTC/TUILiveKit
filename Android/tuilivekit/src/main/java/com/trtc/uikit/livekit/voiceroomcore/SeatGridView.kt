package com.trtc.uikit.livekit.voiceroomcore

import android.annotation.SuppressLint
import android.content.Context
import android.util.AttributeSet
import android.view.View
import android.widget.FrameLayout
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleOwner
import androidx.lifecycle.LifecycleRegistry
import androidx.lifecycle.lifecycleScope
import com.google.gson.Gson
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_CANCEL_REQUEST
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_JOIN_ROOM
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_KICK_USER_OFF_SEAT
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_ROOM
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_SEAT
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LOCK_SEAT
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MOVE_TO_SEAT
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MUTE_MICROPHONE
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_RESPONSE_REQUEST
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_LAYOUT_MODE
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_SEAT_VIEW_ADAPTER
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_MICROPHONE
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_ROOM
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_MICROPHONE
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_ROOM
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_SEAT
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_USER_ON_SEAT
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UNMUTE_MICROPHONE
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UPDATE_SEAT_MODE
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_PANEL_HIDE_SEAT_GRID_VIEW
import com.trtc.uikit.livekit.common.LIVEKIT_METRICS_PANEL_SHOW_SEAT_GRID_VIEW
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.common.convertToKickedOutReason
import com.trtc.uikit.livekit.common.convertToSeatInfo
import com.trtc.uikit.livekit.common.convertToUserInfo
import com.trtc.uikit.livekit.common.liveInfoFromEngineLiveInfo
import com.trtc.uikit.livekit.common.liveInfoToEngineLiveInfo
import com.trtc.uikit.livekit.common.reportEventData
import com.trtc.uikit.livekit.common.seatModeFromEngineSeatMode
import com.trtc.uikit.livekit.voiceroomcore.impl.SeatGridLayout
import com.trtc.uikit.livekit.voiceroomcore.impl.SeatGridViewObserverManager
import com.trtc.uikit.livekit.voiceroomcore.impl.SeatInfoWrapper
import com.trtc.uikit.livekit.voiceroomcore.impl.SeatLayoutConfigManager
import com.trtc.uikit.livekit.voiceroomcore.view.SeatInfoView
import io.trtc.tuikit.atomicxcore.api.CoGuestStore
import io.trtc.tuikit.atomicxcore.api.CompletionHandler
import io.trtc.tuikit.atomicxcore.api.DeviceStore
import io.trtc.tuikit.atomicxcore.api.GuestListener
import io.trtc.tuikit.atomicxcore.api.HostListener
import io.trtc.tuikit.atomicxcore.api.LiveEndedReason
import io.trtc.tuikit.atomicxcore.api.LiveInfo
import io.trtc.tuikit.atomicxcore.api.LiveInfoCompletionHandler
import io.trtc.tuikit.atomicxcore.api.LiveKickedOutReason
import io.trtc.tuikit.atomicxcore.api.LiveListListener
import io.trtc.tuikit.atomicxcore.api.LiveListStore
import io.trtc.tuikit.atomicxcore.api.LiveSeatStore
import io.trtc.tuikit.atomicxcore.api.LiveUserInfo
import io.trtc.tuikit.atomicxcore.api.MoveSeatPolicy
import io.trtc.tuikit.atomicxcore.api.NoResponseReason
import io.trtc.tuikit.atomicxcore.api.StopLiveCompletionHandler
import io.trtc.tuikit.atomicxcore.api.TakeSeatMode
import kotlinx.coroutines.Job
import kotlinx.coroutines.launch


@SuppressLint("ViewConstructor")
class SeatGridView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0,
) : FrameLayout(context, attrs, defStyleAttr), LifecycleOwner {
    companion object {
        private val LOGGER = LiveKitLogger.Companion.getComponentLogger("SeatGridView")
    }

    private val seatGridLayout = SeatGridLayout(context)
    private var seatViewAdapter: VoiceRoomDefine.SeatViewAdapter? = null
    private var observerManager = SeatGridViewObserverManager()
    private val seatLayoutConfigManager = SeatLayoutConfigManager()
    private var applicationCallback: VoiceRoomDefine.RequestCallback? = null
    private val inviteCallbackMap = HashMap<String, VoiceRoomDefine.RequestCallback?>()

    private val deviceStore: DeviceStore
    private val liveListStore: LiveListStore = LiveListStore.shared()
    private var coGuestStore: CoGuestStore? = null
    private var seatStore: LiveSeatStore? = null
    private var liveInfo: LiveInfo? = null

    private val lifecycleRegistry = LifecycleRegistry(this)
    private val collectJobs = mutableListOf<Job>()

    init {
        lifecycleRegistry.currentState = Lifecycle.State.INITIALIZED
        deviceStore = DeviceStore.shared()
    }

    override val lifecycle: Lifecycle get() = lifecycleRegistry

    @Deprecated("")
    fun startMicrophone(callback: TUIRoomDefine.ActionCallback?) {
        LOGGER.info("API startMicrophone")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_MICROPHONE)
        deviceStore.openLocalMicrophone(ActionCallback(callback))
    }

    @Deprecated("")
    fun stopMicrophone() {
        LOGGER.info("API stopMicrophone")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_MICROPHONE)
        deviceStore.closeLocalMicrophone()
    }

    @Deprecated("")
    fun muteMicrophone() {
        LOGGER.info("API muteMicrophone")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MUTE_MICROPHONE)
        if (seatStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            return
        }
        seatStore?.muteMicrophone()
    }

    @Deprecated("")
    fun unmuteMicrophone(callback: TUIRoomDefine.ActionCallback?) {
        LOGGER.info("API unmuteMicrophone")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UNMUTE_MICROPHONE)
        if (seatStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        seatStore?.unmuteMicrophone(ActionCallback(callback))
    }

    @Deprecated("")
    fun startVoiceRoom(
        liveInfo: TUILiveListManager.LiveInfo,
        callback: TUILiveListManager.LiveInfoCallback?,
    ) {
        LOGGER.info("API startVoiceRoom liveInfo:${Gson().toJson(liveInfo)}")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_ROOM)
        init(liveInfo.roomId)
        liveInfo.keepOwnerOnSeat = true
        liveListStore.createLive(
            liveInfoFromEngineLiveInfo(liveInfo),
            object : LiveInfoCompletionHandler {
                override fun onSuccess(liveInfo: LiveInfo) {
                    this@SeatGridView.liveInfo = liveInfo
                    seatLayoutConfigManager.initSeatList(liveInfo.maxSeatCount)
                    initSeatGridLayout(liveInfo.maxSeatCount)
                    callback?.onSuccess(liveInfoToEngineLiveInfo(liveInfo))
                }

                override fun onFailure(code: Int, desc: String) {
                    callback?.onError(TUICommonDefine.Error.fromInt(code), desc)
                }
            })
    }

    @Deprecated("")
    fun stopVoiceRoom(callback: TUILiveListManager.StopLiveCallback?) {
        LOGGER.info("API stopVoiceRoom")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_ROOM)
        unInit()
        liveListStore.endLive(object : StopLiveCompletionHandler {
            override fun onSuccess(statisticsData: TUILiveListManager.LiveStatisticsData) {
                callback?.onSuccess(statisticsData)
            }

            override fun onFailure(code: Int, desc: String) {
                callback?.onError(TUICommonDefine.Error.fromInt(code), desc)
            }
        })
        liveInfo = null
    }

    @Deprecated("")
    fun joinVoiceRoom(roomId: String, callback: TUILiveListManager.LiveInfoCallback?) {
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_JOIN_ROOM)
        init(roomId)
        liveListStore.joinLive(roomId, object : LiveInfoCompletionHandler {
            override fun onSuccess(liveInfo: LiveInfo) {
                this@SeatGridView.liveInfo = liveInfo
                seatLayoutConfigManager.initSeatList(liveInfo.maxSeatCount)
                initSeatGridLayout(liveInfo.maxSeatCount)
                callback?.onSuccess(liveInfoToEngineLiveInfo(liveInfo))
            }

            override fun onFailure(code: Int, desc: String) {
                callback?.onError(TUICommonDefine.Error.fromInt(code), desc)
            }
        })
    }

    @Deprecated("")
    fun leaveVoiceRoom(callback: TUIRoomDefine.ActionCallback?) {
        LOGGER.info("API leaveVoiceRoom")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_ROOM)
        unInit()
        liveListStore.leaveLive(ActionCallback(callback))
        liveInfo = null
    }

    @Deprecated("")
    fun updateRoomSeatMode(
        seatMode: TUIRoomDefine.SeatMode,
        callback: TUIRoomDefine.ActionCallback?,
    ) {
        LOGGER.info("API updateRoomSeatMode seatMode:$seatMode")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UPDATE_SEAT_MODE)
        val liveId = liveInfo?.liveId
        if (liveId.isNullOrEmpty()) {
            callback?.onError(TUICommonDefine.Error.FAILED, "please enter room first")
            LOGGER.error("not enter room")
            return
        }

        val info = LiveInfo().apply {
            this.seatMode = seatModeFromEngineSeatMode(seatMode)
            this.liveId = liveId
        }
        val flagList = ArrayList<LiveInfo.ModifyFlag>()
        flagList.add(LiveInfo.ModifyFlag.SEAT_MODE)
        liveListStore.updateLiveInfo(info, flagList, ActionCallback(callback))
    }

    @Deprecated("")
    fun responseRemoteRequest(
        userId: String,
        agree: Boolean,
        callback: TUIRoomDefine.ActionCallback?,
    ) {
        LOGGER.info("API responseRemoteRequest userId:$userId agree:$agree")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_RESPONSE_REQUEST)
        if (agree) {
            acceptRequest(userId, callback)
        } else {
            rejectRequest(userId, callback)
        }
    }

    @Deprecated("")
    fun cancelRequest(userId: String, callback: TUIRoomDefine.ActionCallback?) {
        if (coGuestStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        LOGGER.info("API cancelRequest userId:$userId")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_CANCEL_REQUEST)
        if (isOwner()) {
            coGuestStore?.cancelInvitation(
                userId, ActionCallback(
                    callback = callback,
                    success = {
                        val userInfo = TUIRoomDefine.UserInfo().apply {
                            this.userId = userId
                        }
                        inviteCallbackMap.get(userId)?.onCancelled(userInfo)
                        inviteCallbackMap.remove(userId)
                    }
                ))
        } else {
            coGuestStore?.cancelApplication(
                ActionCallback(
                    callback = callback,
                    success = {
                        applicationCallback?.onCancelled(convertToUserInfo(TUIRoomEngine.getSelfInfo()))
                        applicationCallback = null
                    }
                ))
        }
    }

    @Deprecated("")
    fun takeSeat(index: Int, timeout: Int, callback: VoiceRoomDefine.RequestCallback?) {
        LOGGER.info("API takeSeat index:$index timeout:$timeout")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_SEAT)
        val userInfo = convertToUserInfo(TUIRoomEngine.getSelfInfo())
        if (seatStore == null || coGuestStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                userInfo,
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        val liveInfo = liveListStore.liveState.currentLive.value
        val isOwner = TUIRoomEngine.getSelfInfo().userId == liveInfo.liveOwner.userId
        if (liveInfo.seatMode == TakeSeatMode.FREE || isOwner) {
            seatStore?.takeSeat(index, object : CompletionHandler {
                override fun onSuccess() {
                    callback?.onAccepted(userInfo)
                }

                override fun onFailure(code: Int, desc: String) {
                    callback?.onError(userInfo, TUICommonDefine.Error.fromInt(code), desc)
                }
            })
            return
        }

        coGuestStore?.applyForSeat(index, timeout, null, object : CompletionHandler {
            override fun onSuccess() {
            }

            override fun onFailure(code: Int, desc: String) {
                callback?.onError(
                    convertToUserInfo(liveInfo.liveOwner),
                    TUICommonDefine.Error.fromInt(code),
                    desc
                )
            }
        })
        applicationCallback = callback
    }

    @Deprecated("")
    fun moveToSeat(index: Int, callback: TUIRoomDefine.ActionCallback?) {
        LOGGER.info("API moveToSeat index:$index")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MOVE_TO_SEAT)
        if (seatStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        seatStore?.moveUserToSeat(
            TUIRoomEngine.getSelfInfo().userId,
            index,
            MoveSeatPolicy.ABORT_WHEN_OCCUPIED,
            ActionCallback(callback)
        )
    }

    @Deprecated("")
    fun leaveSeat(callback: TUIRoomDefine.ActionCallback?) {
        LOGGER.info("API leaveSeat");
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_SEAT)
        val liveInfo = liveListStore.liveState.currentLive.value
        if (seatStore == null || coGuestStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        if (liveInfo.seatMode == TakeSeatMode.FREE || isOwner()) {
            seatStore?.leaveSeat(ActionCallback(callback))
        } else {
            coGuestStore?.disconnect(ActionCallback(callback))
        }
    }

    @Deprecated("")
    fun takeUserOnSeatByAdmin(
        index: Int,
        userId: String,
        timeout: Int,
        callback: VoiceRoomDefine.RequestCallback?,
    ) {
        LOGGER.info("API takeUserOnSeatByAdmin index:$index userId:$userId timeout:$timeout");
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_USER_ON_SEAT)
        val userInfo = TUIRoomDefine.UserInfo().apply {
            this.userId = userId
        }
        if (coGuestStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                userInfo,
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        coGuestStore?.inviteToSeat(userId, index, timeout, null, object : CompletionHandler {
            override fun onSuccess() {
            }

            override fun onFailure(code: Int, desc: String) {
                callback?.onError(
                    userInfo,
                    TUICommonDefine.Error.fromInt(code),
                    desc
                )
            }
        })
        inviteCallbackMap.put(userId, callback)
    }

    @Deprecated("")
    fun kickUserOffSeatByAdmin(userId: String, callback: TUIRoomDefine.ActionCallback?) {
        LOGGER.info("API kickUserOffSeatByAdmin userId:$userId");
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_KICK_USER_OFF_SEAT)
        if (seatStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        seatStore?.kickUserOutOfSeat(userId, ActionCallback(callback))
    }

    fun lockSeat(
        seatIndex: Int,
        params: TUIRoomDefine.SeatLockParams,
        callback: TUIRoomDefine.ActionCallback?,
    ) {
        LOGGER.info("API lockSeat seatIndex:$seatIndex  params: + ${Gson().toJson(params)}")
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LOCK_SEAT)
        TUIRoomEngine.sharedInstance()
            .lockSeatByAdmin(seatIndex, params, object : TUIRoomDefine.ActionCallback {
                override fun onSuccess() {
                    callback?.onSuccess()
                }

                override fun onError(error: TUICommonDefine.Error, message: String) {
                    callback?.onError(error, message)
                }
            })
    }

    @Deprecated("")
    fun setLayoutMode(
        layoutMode: VoiceRoomDefine.LayoutMode,
        layoutConfig: VoiceRoomDefine.SeatViewLayoutConfig?,
    ) {
        LOGGER.info(
            "API setLayoutMode layoutMode: layoutMode $layoutMode: + ${
                Gson().toJson(
                    layoutConfig
                )
            }"
        )
        reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_LAYOUT_MODE)
        seatLayoutConfigManager.setLayoutMode(layoutMode, layoutConfig)
        val seatSize = seatLayoutConfigManager.seatList.size
        if (seatSize > 0) {
            initSeatGridLayout(seatSize)
        }
    }

    @Deprecated("")
    fun setSeatViewAdapter(adapter: VoiceRoomDefine.SeatViewAdapter?) {
        LOGGER.info("API setSeatViewAdapter adapter:$adapter")
        reportEventData(
            LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_SEAT_VIEW_ADAPTER
        )
        seatViewAdapter = adapter
        initSeatGridLayout(seatLayoutConfigManager.seatList.size)
    }

    @Deprecated("")
    fun addObserver(observer: SeatGridViewObserver) {
        LOGGER.info("API addObserver adapter:$observer");
        observerManager.addObserver(observer)
    }

    @Deprecated("")
    fun removeObserver(observer: SeatGridViewObserver) {
        LOGGER.info("API removeObserver adapter:$observer");
        observerManager.removeObserver(observer)
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        reportEventData(LIVEKIT_METRICS_PANEL_SHOW_SEAT_GRID_VIEW)
        addView(seatGridLayout)
        lifecycleRegistry.currentState = Lifecycle.State.STARTED
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        reportEventData(LIVEKIT_METRICS_PANEL_HIDE_SEAT_GRID_VIEW)
        lifecycleRegistry.currentState = Lifecycle.State.DESTROYED
    }

    private fun init(liveId: String) {
        coGuestStore = CoGuestStore.create(liveId)
        seatStore = LiveSeatStore.create(liveId)
        seatLayoutConfigManager.setOnItemUpdateListener(onItemUpdateListener)
        coGuestStore?.addGuestListener(guestListener)
        coGuestStore?.addHostListener(hostListener)
        liveListStore.addLiveListListener(liveListEvent)
        observeSeatList()
        observeSpeakingUsers()
    }

    private fun unInit() {
        seatLayoutConfigManager.setOnItemUpdateListener(null)
        collectJobs.forEach { it.cancel() }
        collectJobs.clear()
        coGuestStore?.removeGuestListener(guestListener)
        coGuestStore?.removeHostListener(hostListener)
        liveListStore.removeLiveListListener(liveListEvent)
        coGuestStore = null
        seatStore = null
    }

    private fun observeSeatList() {
        val job = lifecycleScope.launch {
            seatStore?.liveSeatState?.seatList?.collect { seatList ->
                seatLayoutConfigManager.updateSeatList(seatList)
            }
        }
        collectJobs.add(job)
    }

    private fun observeSpeakingUsers() {
        val job = lifecycleScope.launch {
            seatStore?.liveSeatState?.speakingUsers?.collect { speakingUsers ->
                speakingUsers.forEach { (userId, volume) ->
                    onUserVolumeChanged(userId, volume)
                }
            }
        }
        collectJobs.add(job)
    }

    private fun initSeatGridLayout(maxSeatCount: Int) {
        seatGridLayout.clearAllViews()
        val config = seatLayoutConfigManager.layoutConfig ?: return
        if (maxSeatCount > 0) {
            seatGridLayout.layout(config, maxSeatCount, seatGridLayoutAdapter)
        }
    }

    private fun onUserVolumeChanged(userId: String, volume: Int) {
        val seatInfoWrapper = seatLayoutConfigManager.seatUserMap[userId]
        val seatGridView = this@SeatGridView
        seatInfoWrapper?.let { seat ->
            seatGridLayout.getSeatView(seat.rowIndex, seat.columnIndex)?.let { seatView ->
                seatViewAdapter?.updateUserVolume(seatGridView, volume, seatView)
                    ?: (seatView as? SeatInfoView)?.updateUserVolume(seat.seatInfo, volume)
            }
        }
    }

    private fun acceptRequest(userId: String, callback: TUIRoomDefine.ActionCallback?) {
        if (coGuestStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        if (isOwner()) {
            coGuestStore?.acceptApplication(userId, ActionCallback(callback))
        } else {
            coGuestStore?.acceptInvitation(userId, ActionCallback(callback))
        }
    }

    private fun rejectRequest(userId: String, callback: TUIRoomDefine.ActionCallback?) {
        if (coGuestStore == null) {
            LOGGER.info("user is not in room, operation not allowed")
            callback?.onError(
                TUICommonDefine.Error.FAILED,
                "user is not in room, operation not allowed"
            )
            return
        }
        if (isOwner()) {
            coGuestStore?.rejectApplication(userId, ActionCallback(callback))
        } else {
            coGuestStore?.rejectInvitation(userId, ActionCallback(callback))
        }
    }

    private fun isOwner(): Boolean {
        val liveInfo = liveListStore.liveState.currentLive.value
        if (liveInfo.liveOwner.userId.isEmpty()) {
            return false
        }
        return TUIRoomEngine.getSelfInfo().userId == liveInfo.liveOwner.userId
    }

    private val seatGridLayoutAdapter = object : SeatGridLayout.Adapter {
        override fun createView(index: Int): View {
            val seatInfo = seatLayoutConfigManager.seatList[index].seatInfo
            val convertSeatInfo = seatInfo?.let {
                convertToSeatInfo(it)
            } ?: run {
                TUIRoomDefine.SeatInfo()
            }
            return seatViewAdapter?.createSeatView(this@SeatGridView, convertSeatInfo)
                ?: SeatInfoView(context, observerManager, seatInfo)
        }
    }

    private val onItemUpdateListener = object : SeatLayoutConfigManager.OnItemUpdateListener {
        override fun onItemUpdate(seat: SeatInfoWrapper) {
            val seatGridView = this@SeatGridView
            seat.seatInfo?.let {
                val tuiSeatInfo = convertToSeatInfo(it)
                seatGridLayout.getSeatView(seat.rowIndex, seat.columnIndex)?.let { seatView ->
                    seatViewAdapter?.updateSeatView(seatGridView, tuiSeatInfo, seatView)
                        ?: (seatView as? SeatInfoView)?.updateSeatView(seat.seatInfo)
                }
            }
        }
    }

    private val liveListEvent = object : LiveListListener() {
        override fun onLiveEnded(liveId: String, reason: LiveEndedReason, message: String) {
            observerManager.onRoomDismissed(liveId)
        }

        override fun onKickedOutOfLive(
            liveId: String,
            reason: LiveKickedOutReason,
            message: String,
        ) {
            observerManager.onKickedOutOfRoom(liveId, convertToKickedOutReason(reason), message)
        }
    }

    private val hostListener = object : HostListener() {
        override fun onGuestApplicationReceived(guestUser: LiveUserInfo) {
            observerManager.onSeatRequestReceived(
                VoiceRoomDefine.RequestType.APPLY_TO_TAKE_SEAT,
                convertToUserInfo(guestUser)
            )
        }

        override fun onGuestApplicationCancelled(guestUser: LiveUserInfo) {
            observerManager.onSeatRequestCancelled(
                VoiceRoomDefine.RequestType.APPLY_TO_TAKE_SEAT,
                convertToUserInfo(guestUser)
            )
        }

        override fun onGuestApplicationProcessedByOtherHost(
            guestUser: LiveUserInfo,
            hostUser: LiveUserInfo,
        ) {
            observerManager.onSeatRequestCancelled(
                VoiceRoomDefine.RequestType.APPLY_TO_TAKE_SEAT,
                convertToUserInfo(guestUser)
            )
            inviteCallbackMap.remove(guestUser.userId)
        }

        override fun onHostInvitationResponded(isAccept: Boolean, guestUser: LiveUserInfo) {
            if (isAccept) {
                inviteCallbackMap.get(guestUser.userId)?.onAccepted(convertToUserInfo(guestUser))
            } else {
                inviteCallbackMap.get(guestUser.userId)?.onRejected(convertToUserInfo(guestUser))
            }
            inviteCallbackMap.remove(guestUser.userId)
        }

        override fun onHostInvitationNoResponse(guestUser: LiveUserInfo, reason: NoResponseReason) {
            when (reason) {
                NoResponseReason.TIMEOUT -> inviteCallbackMap.get(guestUser.userId)
                    ?.onTimeout(convertToUserInfo(guestUser))

                NoResponseReason.ALREADY_SEATED -> inviteCallbackMap.get(guestUser.userId)
                    ?.onAccepted(
                        convertToUserInfo(guestUser)
                    )
            }
        }
    }

    private val guestListener = object : GuestListener() {
        override fun onHostInvitationReceived(hostUser: LiveUserInfo) {
            observerManager.onSeatRequestReceived(
                VoiceRoomDefine.RequestType.INVITE_TO_TAKE_SEAT,
                convertToUserInfo(hostUser)
            )
        }

        override fun onHostInvitationCancelled(hostUser: LiveUserInfo) {
            observerManager.onSeatRequestCancelled(
                VoiceRoomDefine.RequestType.INVITE_TO_TAKE_SEAT,
                convertToUserInfo(hostUser)
            )
        }

        override fun onGuestApplicationResponded(isAccept: Boolean, hostUser: LiveUserInfo) {
            if (isAccept) {
                applicationCallback?.onAccepted(convertToUserInfo(hostUser))
            } else {
                applicationCallback?.onRejected(convertToUserInfo(hostUser))
            }
            applicationCallback = null
        }

        override fun onGuestApplicationNoResponse(reason: NoResponseReason) {
            when (reason) {
                NoResponseReason.TIMEOUT -> applicationCallback?.onTimeout(
                    convertToUserInfo(
                        TUIRoomEngine.getSelfInfo()
                    )
                )

                NoResponseReason.ALREADY_SEATED -> applicationCallback?.onAccepted(
                    convertToUserInfo(TUIRoomEngine.getSelfInfo())
                )
            }
            applicationCallback = null
        }
    }

    private class ActionCallback(
        private val callback: TUIRoomDefine.ActionCallback?, private val success: () -> Unit = {},
        private val error: (Int, String) -> Unit = { _, _ -> },
    ) : CompletionHandler {
        override fun onSuccess() {
            success()
            callback?.onSuccess()
        }

        override fun onFailure(code: Int, desc: String) {
            error(code, desc)
            callback?.onError(TUICommonDefine.Error.fromInt(code), desc)
        }
    }
}