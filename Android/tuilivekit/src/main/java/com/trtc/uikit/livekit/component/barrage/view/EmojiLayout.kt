package com.trtc.uikit.livekit.component.barrage.view

import android.annotation.SuppressLint
import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.FrameLayout
import android.widget.ImageView
import androidx.cardview.widget.CardView
import androidx.core.content.res.ResourcesCompat
import androidx.recyclerview.widget.GridLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.tencent.qcloud.tuicore.util.ScreenUtil
import com.trtc.uikit.livekit.R

@SuppressLint("ViewConstructor")
class EmojiLayout(context: Context, emojiResIds: List<Int>) : FrameLayout(context) {
    private val emojiView: RecyclerView
    private val deleteView: CardView
    private val emojiSpace = ScreenUtil.dip2px(13f)
    private var emojiListener: EmojiListener? = null
    private val emojiResIds = emojiResIds.toMutableList()

    init {
        inflate(context, R.layout.livekit_barrage_emoji_view, this)
        emojiView = findViewById(R.id.rv_emoji_list)
        deleteView = findViewById(R.id.cd_delete)
        initViews()
    }

    private fun initViews() {
        val padding = emojiSpace / 2
        emojiView.setPadding(padding, 0, padding, 0)
        val emojiSpanCount = 8
        emojiView.layoutManager = GridLayoutManager(context, emojiSpanCount)
        emojiView.adapter = object : RecyclerView.Adapter<ImageViewHolder>() {
            override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ImageViewHolder {
                val parentWidth = (emojiView.parent as ViewGroup).width
                val itemSize = (parentWidth - 2 * (emojiSpanCount + 1) * padding) / emojiSpanCount + 2 * padding
                return ImageViewHolder(ImageView(context).apply {
                    layoutParams = LayoutParams(itemSize, itemSize)
                }, padding)
            }

            override fun onBindViewHolder(holder: ImageViewHolder, position: Int) {
                val resId = emojiResIds[position]
                holder.imageView.setOnClickListener {
                    emojiListener?.onAddEmoji(resId)
                }
                holder.bind(resId)
            }

            override fun getItemCount() = emojiResIds.size
        }
        deleteView.setOnClickListener {
            emojiListener?.onDelete()
        }
    }

    fun setDeleteViewEnable(enable: Boolean) {
        deleteView.getChildAt(0).isEnabled = enable
    }

    fun setEmojiListener(listener: EmojiListener) {
        emojiListener = listener
    }

    private class ImageViewHolder(itemView: View, private val padding: Int) : RecyclerView.ViewHolder(itemView) {
        val imageView: ImageView = itemView as ImageView

        fun bind(resId: Int) {
            val drawable = ResourcesCompat.getDrawable(itemView.resources, resId, null)
            imageView.setPadding(padding, padding, padding, padding)
            imageView.setImageDrawable(drawable)
        }
    }

    interface EmojiListener {
        fun onDelete()
        fun onAddEmoji(resId: Int)
    }
}
