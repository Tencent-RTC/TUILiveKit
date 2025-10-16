package com.trtc.uikit.livekit.component.barrage.view.adapter

import android.annotation.SuppressLint
import android.util.Log
import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.trtc.uikit.livekit.component.barrage.BarrageStreamView.OnMessageClickListener
import io.trtc.tuikit.atomicxcore.api.Barrage

class BarrageMsgListAdapter(
    private val msgList: List<Barrage>
) : RecyclerView.Adapter<RecyclerView.ViewHolder>() {

    companion object {
        private const val TAG = "BarrageMsgListAdapter"
    }

    private val adapterMap = mutableMapOf<Int, BarrageItemAdapter>()
    private var viewTypeDelegate: BarrageItemTypeDelegate? = null
    private var onMessageClickListener: OnMessageClickListener? = null

    fun setItemTypeDelegate(delegate: BarrageItemTypeDelegate) {
        viewTypeDelegate = delegate
    }

    fun setItemAdapter(itemType: Int, adapter: BarrageItemAdapter) {
        adapterMap[itemType] = adapter
        Log.i(TAG, "setItemAdapter: [itemType:$itemType, adapter:$adapter]")
    }

    fun setOnMessageClickListener(listener: OnMessageClickListener?) {
        onMessageClickListener = listener
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): RecyclerView.ViewHolder {
        return adapterMap[viewType]?.onCreateViewHolder(parent)
            ?: throw IllegalArgumentException("No adapter found for viewType: $viewType")
    }

    @SuppressLint("SetTextI18n")
    override fun onBindViewHolder(holder: RecyclerView.ViewHolder, position: Int) {
        val barrage = msgList[position]
        val viewType = getItemViewType(position)

        adapterMap[viewType]?.let { adapter ->
            adapter.onBindViewHolder(holder, position, barrage)
            holder.itemView.setOnClickListener {
                val userInfo = TUIRoomDefine.UserInfo().apply {
                    userId = barrage.sender.userID
                    userName = barrage.sender.userName
                    avatarUrl = barrage.sender.avatarURL
                }
                onMessageClickListener?.onMessageClick(userInfo)
            }
        }
    }

    override fun getItemCount(): Int = msgList.size

    override fun getItemViewType(position: Int): Int {
        return viewTypeDelegate?.getItemType(position, msgList[position]) ?: 0
    }
}
