package com.trtc.uikit.livekit.voiceroom.view.dashboard

import android.app.Activity
import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.TextView
import androidx.constraintlayout.utils.widget.ImageFilterView
import com.trtc.tuikit.common.imageloader.ImageLoader
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager
import com.trtc.uikit.livekit.voiceroom.view.BasicView

class AudienceDashboardView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr) {

    private lateinit var textName: TextView
    private lateinit var imageHead: ImageFilterView

    override fun initView() {
        LayoutInflater.from(context).inflate(R.layout.livekit_audience_dashboard_view, this, true)
        textName = findViewById(R.id.tv_name)
        imageHead = findViewById(R.id.iv_head)

        findViewById<View>(R.id.iv_back).setOnClickListener {
            mVoiceRoomManager.roomManager.clearLiveState()
            (context as? Activity)?.finish()
        }
    }

    override fun init(voiceRoomManager: VoiceRoomManager) {
        super.init(voiceRoomManager)
        val ownerInfo = mRoomState.ownerInfo
        textName.text = ownerInfo.userName.ifEmpty { ownerInfo.userId }

        if (ownerInfo.avatarUrl.isEmpty()) {
            imageHead.setImageResource(R.drawable.livekit_ic_avatar)
        } else {
            ImageLoader.load(context, imageHead, ownerInfo.avatarUrl, R.drawable.livekit_ic_avatar)
        }
    }

    override fun addObserver() = Unit

    override fun removeObserver() = Unit
}
