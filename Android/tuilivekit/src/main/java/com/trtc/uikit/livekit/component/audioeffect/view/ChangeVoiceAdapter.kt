package com.trtc.uikit.livekit.component.audioeffect.view

import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.LinearLayout
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView
import com.trtc.uikit.livekit.R
import io.trtc.tuikit.atomicxcore.api.AudioChangerType
import io.trtc.tuikit.atomicxcore.api.AudioEffectStore

class ChangeVoiceAdapter(
    private val context: Context,
    private val audioEffectStore: AudioEffectStore
) :
    RecyclerView.Adapter<ChangeVoiceAdapter.ViewHolder>() {

    private var data: MutableList<ChangeVoiceItem> = ArrayList()
    private var selectedPosition: Int = 0

    init {
        initData()
    }

    private fun initData() {
        data.add(
            ChangeVoiceItem(
                context.getString(R.string.common_change_voice_none),
                R.drawable.audio_effect_select_none, AudioChangerType.NONE
            )
        )
        data.add(
            ChangeVoiceItem(
                context.getString(R.string.common_change_voice_child),
                R.drawable.audio_effect_change_voice_child, AudioChangerType.CHILD
            )
        )
        data.add(
            ChangeVoiceItem(
                context.getString(R.string.common_change_voice_girl),
                R.drawable.audio_effect_change_voice_girl, AudioChangerType.LITTLE_GIRL
            )
        )
        data.add(
            ChangeVoiceItem(
                context.getString(R.string.common_change_voice_uncle),
                R.drawable.audio_effect_change_voice_uncle, AudioChangerType.MAN
            )
        )
        data.add(
            ChangeVoiceItem(
                context.getString(R.string.common_change_voice_ethereal),
                R.drawable.audio_effect_change_voice_ethereal, AudioChangerType.ETHEREAL
            )
        )
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder {
        val itemView = LayoutInflater.from(parent.context).inflate(
            R.layout.audio_effect_layout_change_voice_item,
            parent, false
        )
        return ViewHolder(itemView)
    }

    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        val changeVoiceItem = data[position]
        holder.textTitle.text = changeVoiceItem.title
        holder.imageIcon.setImageResource(changeVoiceItem.icon)
        if (changeVoiceItem.type == audioEffectStore.audioEffectState.audioChangerType.value) {
            holder.imageIcon.setBackgroundResource(R.drawable.audio_effect_settings_item_select_background)
            selectedPosition = data.indexOf(changeVoiceItem)
        } else {
            holder.imageIcon.setBackgroundResource(R.drawable.audio_effect_settings_item_not_select_background)
        }
        holder.layoutRoot.setOnClickListener { view ->
            val index = holder.bindingAdapterPosition
            if (index != RecyclerView.NO_POSITION) {
                audioEffectStore.setAudioChangerType(changeVoiceItem.type)
                notifyItemChanged(index)
                notifyItemChanged(selectedPosition)
            }
        }
    }

    override fun getItemCount(): Int {
        return data.size
    }

    class ViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        val layoutRoot: LinearLayout = itemView.findViewById(R.id.ll_root)
        val textTitle: TextView = itemView.findViewById(R.id.tv_title)
        val imageIcon: ImageView = itemView.findViewById(R.id.iv_icon)
    }

    class ChangeVoiceItem(
        val title: String,
        val icon: Int,
        val type: AudioChangerType
    )
}