package com.trtc.uikit.livekit.component.audiencelist

import android.annotation.SuppressLint
import android.content.Context
import android.graphics.Point
import android.text.TextUtils
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.MotionEvent
import android.view.View
import android.widget.LinearLayout
import android.widget.RelativeLayout
import android.widget.TextView
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.qcloud.tuicore.util.ScreenUtil
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.common.reportEventData
import com.trtc.uikit.livekit.common.ui.BasicView
import com.trtc.uikit.livekit.component.audiencelist.view.AudienceListPopupDialog
import com.trtc.uikit.livekit.component.audiencelist.view.adapter.AudienceListIconAdapter
import com.trtc.uikit.livekit.component.audiencelist.viewmodel.AudienceListConstants.LIVEKIT_AUDIENCE_LIST_MAX_SHOW_AVATAR_COUNT
import com.trtc.uikit.livekit.component.audiencelist.viewmodel.AudienceListConstants.LIVEKIT_AUDIENCE_LIST_ROOM_MAX_SHOW_USER_COUNT
import com.trtc.uikit.livekit.component.audiencelist.viewmodel.AudienceListConstants.LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIENCE_LIST
import com.trtc.uikit.livekit.component.audiencelist.viewmodel.AudienceListConstants.LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIENCE_LIST
import io.trtc.tuikit.atomicxcore.api.LiveAudienceStore
import io.trtc.tuikit.atomicxcore.api.LiveEndedReason
import io.trtc.tuikit.atomicxcore.api.LiveListListener
import io.trtc.tuikit.atomicxcore.api.LiveListStore
import io.trtc.tuikit.atomicxcore.api.LiveUserInfo
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import java.util.Locale

@SuppressLint("ViewConstructor")
class AudienceListView @JvmOverloads constructor(
    private val context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr) {

    private var textAudienceCount: TextView
    private var layoutAudienceCount: LinearLayout
    private var recycleAudienceList: RecyclerView
    private var adapter: AudienceListIconAdapter? = null

    private var liveListStore: LiveListStore? = null
    private var audienceStore: LiveAudienceStore? = null

    private var audienceListPopupDialog: AudienceListPopupDialog? = null
    private var onUserItemClickListener: OnUserItemClickListener? = null

    private val logger = LiveKitLogger.getComponentLogger("AudienceListView")
    private val liveListListener = object : LiveListListener() {
        override fun onLiveEnded(liveId: String, reason: LiveEndedReason, message: String) {
            if (liveId != roomId) return
            audienceListPopupDialog?.dismiss()
        }
    }

    init {
        LayoutInflater.from(context).inflate(R.layout.audience_list_layout_icon, this, true)
        layoutAudienceCount = findViewById(R.id.ll_audience_count)
        textAudienceCount = findViewById(R.id.tv_audience_count)
        recycleAudienceList = findViewById(R.id.rv_audience_list)
    }

    @Deprecated("Use init(TUILiveListManager.LiveInfo) instead")
    fun init(roomInfo: TUIRoomDefine.RoomInfo) {
        init(convertToLiveInfo(roomInfo))
    }

    fun init(liveInfo: TUILiveListManager.LiveInfo) {
        super.init(liveInfo.roomId)
        initView()
        reportData(liveInfo.roomId)
    }

    fun setOnUserItemClickListener(listener: OnUserItemClickListener?) {
        onUserItemClickListener = listener
        audienceListPopupDialog?.setOnUserItemClickListener(listener)
    }

    fun setScreenOrientation(isPortrait: Boolean) {
        layoutAudienceCount.isEnabled = isPortrait
        recycleAudienceList.isEnabled = isPortrait
    }

    override fun initStore() {
        liveListStore = LiveListStore.shared()
        audienceStore = LiveAudienceStore.create(liveID = roomId)
    }

    override fun addObserver() {
        liveListStore?.addLiveListListener(liveListListener)
        subscribeStateJob = CoroutineScope(Dispatchers.Main).launch {
            launch {
                audienceStore?.liveAudienceState?.audienceList?.collect { audienceList ->
                    onAudienceListChange(audienceList)
                }
            }
            launch {
                audienceStore?.liveAudienceState?.audienceCount?.collect { audienceCount ->
                    updateAudienceCount()
                }
            }
        }
    }

    override fun removeObserver() {
        liveListStore?.removeLiveListListener(liveListListener)
        subscribeStateJob?.cancel()
    }

    private fun initView() {
        initAudienceCountView()
        initAudienceAvatarView()
    }

    private fun convertToLiveInfo(roomInfo: TUIRoomDefine.RoomInfo): TUILiveListManager.LiveInfo {
        return TUILiveListManager.LiveInfo().apply {
            roomId = roomInfo.roomId
            name = roomInfo.name
            ownerId = roomInfo.ownerId
            ownerName = roomInfo.ownerName
            ownerAvatarUrl = roomInfo.ownerAvatarUrl
        }
    }

    private fun initAudienceCountView() {
        layoutAudienceCount.setOnClickListener { showAudienceListPanelView() }
    }

    @SuppressLint("ClickableViewAccessibility")
    private fun initAudienceAvatarView() {
        recycleAudienceList.layoutManager =
            LinearLayoutManager(
                context,
                LinearLayoutManager.HORIZONTAL,
                false
            )
        adapter =
            AudienceListIconAdapter(
                context,
                audienceStore?.liveAudienceState
                ?: LiveAudienceStore.create(liveID = roomId).liveAudienceState
            )
        recycleAudienceList.adapter = adapter
        recycleAudienceList.setOnTouchListener(object : OnTouchListener {
            private val point = Point()
            private var scroll = false

            override fun onTouch(v: View, event: MotionEvent): Boolean {
                when (event.action) {
                    MotionEvent.ACTION_DOWN -> {
                        point.set(event.x.toInt(), event.y.toInt())
                        scroll = false
                    }

                    MotionEvent.ACTION_MOVE -> {
                        if (kotlin.math.abs(event.x - point.x) > 10 && !scroll) {
                            scroll = true
                        }
                    }

                    MotionEvent.ACTION_UP -> {
                        if (!scroll) {
                            showAudienceListPanelView()
                        }
                    }
                }
                return false
            }
        })
    }

    private fun showAudienceListPanelView() {
        audienceStore?.fetchAudienceList(null)
        if (audienceListPopupDialog == null) {
            audienceListPopupDialog = AudienceListPopupDialog(
                context, audienceStore?.liveAudienceState
                         ?: LiveAudienceStore.create(liveID = roomId).liveAudienceState
            )
            audienceListPopupDialog?.setOnUserItemClickListener(onUserItemClickListener)
        }
        audienceListPopupDialog?.show()
    }

    private fun onAudienceListChange(userInfoList: List<LiveUserInfo>) {
        val params = recycleAudienceList.layoutParams as RelativeLayout.LayoutParams
        if (userInfoList.size <= LIVEKIT_AUDIENCE_LIST_MAX_SHOW_AVATAR_COUNT) {
            params.width = RelativeLayout.LayoutParams.WRAP_CONTENT
        } else {
            params.width = ScreenUtil.dip2px(56F)
        }
        adapter?.updateData()
        updateAudienceCount()
    }

    private fun updateAudienceCount() {
        val userCount = audienceStore?.liveAudienceState?.audienceCount?.value ?: 0
        val listSize = audienceStore?.liveAudienceState?.audienceList?.value?.size ?: 0
        logger.info("updateAudienceCount userCount:$userCount listSize:$listSize")
        if (userCount >= LIVEKIT_AUDIENCE_LIST_ROOM_MAX_SHOW_USER_COUNT) {
            setUserCount(userCount)
        } else {
            setUserCount(listSize)
        }
    }

    @SuppressLint("SetTextI18n")
    private fun setUserCount(count: Int) {
        val text = formatUserCount(count)
        textAudienceCount.text = text
    }

    private fun formatUserCount(count: Int): String {
        var text = count.toString()
        if (count >= 10000) {
            val isChinese = Locale.CHINESE.language == Locale.getDefault().language
            if (isChinese) {
                text = String.format(Locale.getDefault(), "%.1f", count / 10000.0f)
                if (text.endsWith(".0")) {
                    text = text.replace(".0", "")
                }
                text += "万"
            } else {
                text = String.format(Locale.getDefault(), "%dk", count / 1000)
            }
        }
        return text
    }

    private fun reportData(roomId: String) {
        val isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_")
        if (isVoiceRoom) {
            reportEventData(LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIENCE_LIST)
        } else {
            reportEventData(LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIENCE_LIST)
        }
    }

    interface OnUserItemClickListener {
        fun onUserItemClick(userInfo: TUIRoomDefine.UserInfo)
    }
}