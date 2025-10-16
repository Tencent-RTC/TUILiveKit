package com.trtc.uikit.livekit.component.barrage

import android.annotation.SuppressLint
import android.content.Context
import android.text.TextUtils
import android.util.AttributeSet
import android.view.LayoutInflater
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.reportEventData
import com.trtc.uikit.livekit.common.ui.BasicView
import com.trtc.uikit.livekit.component.barrage.view.CustomRecyclerView
import com.trtc.uikit.livekit.component.barrage.view.IBarrageDisplayView
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageItemAdapter
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageItemDefaultAdapter
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageItemTypeDelegate
import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageMsgListAdapter
import com.trtc.uikit.livekit.component.barrage.viewmodel.BarrageConstants
import io.trtc.tuikit.atomicxcore.api.Barrage
import io.trtc.tuikit.atomicxcore.api.BarrageStore
import io.trtc.tuikit.atomicxcore.api.LiveAudienceListener
import io.trtc.tuikit.atomicxcore.api.LiveAudienceStore
import io.trtc.tuikit.atomicxcore.api.LiveUserInfo
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch

@SuppressLint("ViewConstructor")
class BarrageStreamView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr), IBarrageDisplayView {

    companion object {
        private const val BARRAGE_LIST_UPDATE_DURATION_MS = 250L
        private const val SMOOTH_SCROLL_COUNT_MAX = 100
    }

    private var timestampOnLastUpdate = 0L
    private var smoothScroll = true

    private val updateViewTask = Runnable { notifyDataSetChanged() }

    private val recyclerMsg: RecyclerView by lazy { findViewById(R.id.rv_msg) }
    private val msgList = mutableListOf<Barrage>()
    private val adapter = BarrageMsgListAdapter(msgList)
    private var barrageStore: BarrageStore? = null
    private var liveAudienceStore: LiveAudienceStore? = null

    init {
        LayoutInflater.from(context).inflate(R.layout.livekit_barrage_view_display, this)
        recyclerMsg.layoutManager = LinearLayoutManager(context)
        recyclerMsg.adapter = adapter
    }

    fun init(roomId: String, ownerId: String) {
        super.init(roomId)
        this.roomId = roomId
        adapter.setItemAdapter(0, BarrageItemDefaultAdapter(context, ownerId))
        reportData()
    }

    fun setItemTypeDelegate(delegate: BarrageItemTypeDelegate) {
        adapter.setItemTypeDelegate(delegate)
    }

    fun setItemAdapter(itemType: Int, adapter: BarrageItemAdapter) {
        this.adapter.setItemAdapter(itemType, adapter)
    }

    fun setOnMessageClickListener(listener: OnMessageClickListener) {
        adapter.setOnMessageClickListener(listener)
    }

    override fun initStore() {
        barrageStore = BarrageStore.create(roomId)
        liveAudienceStore = LiveAudienceStore.create(roomId)
    }

    override fun addObserver() {
        subscribeStateJob = CoroutineScope(Dispatchers.Main).launch {
            barrageStore?.barrageState?.messageList?.collect {
                onBarrageListChanged(it)
            }
        }
        liveAudienceStore?.addLiveAudienceListener(liveAudienceListener)
    }

    override fun removeObserver() {
        subscribeStateJob?.cancel()
        liveAudienceStore?.removeLiveAudienceListener(liveAudienceListener)
    }

    override fun insertBarrages(vararg barrages: Barrage) {
        barrages.forEach {
            barrageStore?.appendLocalTip(it)
        }
    }

    fun onBarrageListChanged(barrages: List<Barrage>) {
        smoothScroll = barrages.size - msgList.size < SMOOTH_SCROLL_COUNT_MAX
        msgList.clear()
        msgList.addAll(barrages)
        removeCallbacks(updateViewTask)

        if (System.currentTimeMillis() - timestampOnLastUpdate >= BARRAGE_LIST_UPDATE_DURATION_MS) {
            notifyDataSetChanged()
        } else {
            postDelayed(updateViewTask, BARRAGE_LIST_UPDATE_DURATION_MS)
        }
    }

    fun getBarrageCount(): Int = barrageStore?.barrageState?.messageList?.value?.size ?: 0

    @SuppressLint("NotifyDataSetChanged")
    private fun notifyDataSetChanged() {
        timestampOnLastUpdate = System.currentTimeMillis()
        adapter.notifyDataSetChanged()
        if ((recyclerMsg as CustomRecyclerView).isLongPressed) return

        val targetPosition = maxOf(0, adapter.itemCount - 1)
        if (smoothScroll) {
            recyclerMsg.smoothScrollToPosition(targetPosition)
        } else {
            recyclerMsg.scrollToPosition(targetPosition)
        }
    }

    private fun reportData() {
        val isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_")
        val key = if (isVoiceRoom) {
            BarrageConstants.LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_BARRAGE_SHOW
        } else {
            BarrageConstants.LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_BARRAGE_SHOW
        }
        reportEventData(key)
    }

    interface OnMessageClickListener {
        fun onMessageClick(userInfo: TUIRoomDefine.UserInfo)
    }

    private val liveAudienceListener = object : LiveAudienceListener() {
        override fun onAudienceJoined(audience: LiveUserInfo) {
            val barrage = Barrage().apply {
                textContent = this@BarrageStreamView.context.getString(R.string.common_entered_room)
                sender.apply {
                    userID = audience.userID
                    userName = audience.userName
                    avatarURL = audience.avatarURL
                }
            }
            insertBarrages(barrage)
        }
    }
}
