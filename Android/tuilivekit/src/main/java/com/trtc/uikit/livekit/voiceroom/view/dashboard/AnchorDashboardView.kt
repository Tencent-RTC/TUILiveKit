package com.trtc.uikit.livekit.voiceroom.view.dashboard

import android.app.Activity
import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.TextView
import androidx.annotation.NonNull
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager
import com.trtc.uikit.livekit.voiceroom.view.BasicView
import java.util.Locale

class AnchorDashboardView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr) {

    override fun initView() {
        LayoutInflater.from(context).inflate(R.layout.livekit_anchor_dashboard_view, this, true)
    }

    override fun init(@NonNull voiceRoomManager: VoiceRoomManager) {
        super.init(voiceRoomManager)
        
        findViewById<TextView>(R.id.tv_duration).text = formatSecondsTo00(
            ((System.currentTimeMillis() - voiceRoomManager.roomState.createTime) / 1000).toInt()
        )

        with(voiceRoomManager.roomState.liveExtraInfo) {
            findViewById<TextView>(R.id.tv_viewers).text = maxAudienceCount.toString()
            findViewById<TextView>(R.id.tv_message).text = messageCount.toString()
            findViewById<TextView>(R.id.tv_gift_income).text = giftIncome.toString()
            findViewById<TextView>(R.id.tv_gift_people).text = giftSenderCount.toString()
            findViewById<TextView>(R.id.tv_like).text = likeCount.toString()
        }

        findViewById<View>(R.id.iv_back).setOnClickListener {
            (context as? Activity)?.finish()
        }
    }

    override fun addObserver() = Unit

    override fun removeObserver() = Unit

    private fun formatSecondsTo00(timeSeconds: Int): String {
        if (timeSeconds <= 0) return "-- --"
        
        val hour = timeSeconds / 3600
        val min = timeSeconds % 3600 / 60
        val sec = timeSeconds % 60
        return String.format(Locale.getDefault(), "%02d:%02d:%02d", hour, min, sec)
    }
}
