package com.trtc.uikit.livekit.component.audiencelist.view.adapter

import android.annotation.SuppressLint
import android.content.Context
import android.text.TextUtils
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.TextView
import androidx.constraintlayout.utils.widget.ImageFilterView
import androidx.recyclerview.widget.RecyclerView
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.trtc.tuikit.common.imageloader.ImageLoader
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.convertToUserInfo
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView
import io.trtc.tuikit.atomicxcore.api.LiveAudienceState
import io.trtc.tuikit.atomicxcore.api.LiveListStore
import io.trtc.tuikit.atomicxcore.api.LiveUserInfo
import java.util.concurrent.CopyOnWriteArrayList

class AudienceListPanelAdapter(
    private val context: Context,
    private val audienceState: LiveAudienceState
) : RecyclerView.Adapter<AudienceListPanelAdapter.ViewHolder>() {

    private var data: CopyOnWriteArrayList<LiveUserInfo> =
        CopyOnWriteArrayList(audienceState.audienceList.value)
    private var onItemClickListener: AudienceListView.OnUserItemClickListener? = null

    fun setOnItemClickListener(listener: AudienceListView.OnUserItemClickListener?) {
        onItemClickListener = listener
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder {
        val view = LayoutInflater.from(parent.context).inflate(
            R.layout.audience_list_layout_panel_item, parent, false
        )
        return ViewHolder(view)
    }

    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        holder.itemView.setOnClickListener {
            onItemClickListener?.onUserItemClick(convertToUserInfo(data[position]))
        }

        if (TextUtils.isEmpty(data[position].avatarURL)) {
            holder.imageHead.setImageResource(R.drawable.audience_list_default_avatar)
        } else {
            ImageLoader.load(
                context, holder.imageHead, data[position].avatarURL,
                R.drawable.audience_list_default_avatar
            )
        }

        if (TextUtils.isEmpty(data[position].userName)) {
            holder.textName.text = data[position].userId
        } else {
            holder.textName.text = data[position].userName
        }

        val selfUserId = TUIRoomEngine.getSelfInfo().userId
        if (!TextUtils.isEmpty(selfUserId) && selfUserId == LiveListStore.shared().liveState.currentLive.value.liveOwner.userId) {
            holder.more.visibility = if (onItemClickListener == null) View.GONE else View.VISIBLE
        } else {
            holder.more.visibility = View.GONE
        }
        holder.textLevel.visibility = View.GONE
    }

    override fun getItemCount(): Int {
        return data.size
    }

    @SuppressLint("NotifyDataSetChanged")
    fun updateData() {
        data = CopyOnWriteArrayList(audienceState.audienceList.value)
        notifyDataSetChanged()
    }

    class ViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        val imageHead: ImageFilterView = itemView.findViewById(R.id.iv_head)
        val textName: TextView = itemView.findViewById(R.id.tv_name)
        val textLevel: TextView = itemView.findViewById(R.id.tv_level)
        val more: ImageView = itemView.findViewById(R.id.more)
    }
}