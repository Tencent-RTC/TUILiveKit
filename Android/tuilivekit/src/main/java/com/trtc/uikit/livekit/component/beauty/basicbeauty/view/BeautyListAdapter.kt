package com.trtc.uikit.livekit.component.beauty.basicbeauty.view

import android.annotation.SuppressLint
import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.LinearLayout
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView
import com.trtc.uikit.livekit.R

class BeautyListAdapter(
    context: Context
) : RecyclerView.Adapter<BeautyListAdapter.ViewHolder>() {
    private var currentBeautyType = BeautyType.NONE
    private var onItemClickListener: OnItemClickListener? = null

    private val data = listOf(
        BeautyItem(
            context.getString(R.string.common_beauty_item_close),
            R.drawable.beauty_item_close,
            BeautyType.CLOSE,
            null
        ),
        BeautyItem(
            context.getString(R.string.common_beauty_item_smooth),
            R.drawable.beauty_item_smooth,
            BeautyType.SMOOTH,
            null
        ),
        BeautyItem(
            context.getString(R.string.common_beauty_item_whiteness),
            R.drawable.beauty_item_whiteness,
            BeautyType.WHITENESS,
            null
        ),
        BeautyItem(
            context.getString(R.string.common_beauty_item_ruddy),
            R.drawable.beauty_item_ruddy,
            BeautyType.RUDDY,
            null
        )
    )

    fun setOnItemClickListener(listener: OnItemClickListener) {
        onItemClickListener = listener
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int) =
        ViewHolder(
            LayoutInflater.from(parent.context)
                .inflate(R.layout.beauty_panel_item, parent, false)
        )

    @SuppressLint("RecyclerView", "NotifyDataSetChanged")
    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        val item = data[position]
        with(holder) {
            textTitle.text = item.title
            imageIcon.run {
                setImageResource(item.icon)
                setBackgroundResource(
                    if (item.type == currentBeautyType) {
                        R.drawable.beauty_item_select_background
                    } else {
                        R.drawable.beauty_item_not_select_background
                    }
                )
            }
            layoutRoot.apply {
                tag = item.type
                setOnClickListener {
                    currentBeautyType = item.type
                    onItemClickListener?.onItemClick(currentBeautyType)
                    notifyDataSetChanged()
                }
            }
        }
    }

    override fun getItemCount() = data.size

    class ViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        val layoutRoot: LinearLayout = itemView.findViewById(R.id.ll_root)
        val textTitle: TextView = itemView.findViewById(R.id.tv_title)
        val imageIcon: ImageView = itemView.findViewById(R.id.iv_icon)
    }

    data class BeautyItem(
        val title: String,
        val icon: Int,
        val type: BeautyType,
        val listener: View.OnClickListener?
    )

    enum class BeautyType(val value: Int) {
        NONE(0),
        CLOSE(1),
        SMOOTH(2),
        WHITENESS(3),
        RUDDY(4)
    }

    fun interface OnItemClickListener {
        fun onItemClick(type: BeautyType)
    }
}
