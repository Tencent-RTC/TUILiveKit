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
import com.trtc.tuikit.common.imageloader.ImageOptions
import com.trtc.uikit.livekit.R

class SingleColumnWidgetView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : FrameLayout(context, attrs, defStyleAttr) {

    private val imageAvatar: ImageView
    private val textRoomName: TextView
    private val textAnchorName: TextView

    init {
        LayoutInflater.from(context).inflate(R.layout.livelist_single_column_widget_item, this, true)
        textRoomName = findViewById(R.id.tv_room_name)
        textAnchorName = findViewById(R.id.tv_anchor_name)
        imageAvatar = findViewById(R.id.iv_avatar)
    }

    fun init(liveInfo: TUILiveListManager.LiveInfo) {
        updateLiveInfoView(liveInfo)
    }

    fun updateLiveInfoView(liveInfo: TUILiveListManager.LiveInfo) {
        val imageUrl = liveInfo.ownerAvatarUrl
        val builder = ImageOptions.Builder().setBlurEffect(80f)
        ImageLoader.load(context, imageAvatar, imageUrl, builder.build())
        textRoomName.text = if (TextUtils.isEmpty(liveInfo.name)) liveInfo.roomId else liveInfo.name
        textAnchorName.text = if (TextUtils.isEmpty(liveInfo.ownerName)) liveInfo.ownerId else liveInfo.ownerName
    }
}
