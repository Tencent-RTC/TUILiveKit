package com.trtc.uikit.livekit.features.livelist.access

import android.content.Context
import android.text.TextUtils
import android.util.AttributeSet
import android.view.LayoutInflater
import android.widget.FrameLayout
import android.widget.ImageView
import android.widget.TextView
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.trtc.tuikit.common.imageloader.ImageLoader
import com.trtc.uikit.livekit.R

class DoubleColumnWidgetView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : FrameLayout(context, attrs, defStyleAttr) {

    private val imageAvatar: ImageView
    private val textRoomName: TextView
    private val textAnchorName: TextView
    private val textAudienceCountInfo: TextView

    init {
        LayoutInflater.from(context).inflate(R.layout.livelist_double_column_widget_item, this, true)
        textRoomName = findViewById(R.id.tv_room_name)
        textAnchorName = findViewById(R.id.tv_anchor_name)
        imageAvatar = findViewById(R.id.iv_avatar)
        textAudienceCountInfo = findViewById(R.id.tv_audience_count_info)
    }

    fun init(liveInfo: TUILiveListManager.LiveInfo) {
        updateLiveInfoView(liveInfo)
    }

    fun updateLiveInfoView(liveInfo: TUILiveListManager.LiveInfo) {
        ImageLoader.load(context, imageAvatar, liveInfo.ownerAvatarUrl, R.drawable.livelist_default_avatar)
        textRoomName.text = if (TextUtils.isEmpty(liveInfo.name)) liveInfo.roomId else liveInfo.name
        textAnchorName.text = if (TextUtils.isEmpty(liveInfo.ownerName)) liveInfo.ownerId else liveInfo.ownerName
        textAudienceCountInfo.text = context.getString(R.string.livelist_viewed_audience_count, liveInfo.viewCount)
    }
}
