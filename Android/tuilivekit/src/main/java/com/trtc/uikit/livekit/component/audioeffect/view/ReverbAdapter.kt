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
import io.trtc.tuikit.atomicxcore.api.AudioEffectStore
import io.trtc.tuikit.atomicxcore.api.AudioReverbType

class ReverbAdapter(private val context: Context, private val audioEffectStore: AudioEffectStore) :
    RecyclerView.Adapter<ReverbAdapter.ViewHolder>() {

    private var data: MutableList<ReverbItem> = ArrayList()
    private var selectedPosition: Int = 0

    init {
        initData()
    }

    private fun initData() {
        data.add(
            ReverbItem(
                context.getString(R.string.common_reverb_none),
                R.drawable.audio_effect_select_none, AudioReverbType.NONE
            )
        )
        data.add(
            ReverbItem(
                context.getString(R.string.common_reverb_karaoke),
                R.drawable.audio_effect_reverb_ktv, AudioReverbType.KTV
            )
        )
        data.add(
            ReverbItem(
                context.getString(R.string.common_reverb_metallic_sound),
                R.drawable.audio_effect_reverb_metallic_sound,
                AudioReverbType.METALLIC
            )
        )
        data.add(
            ReverbItem(
                context.getString(R.string.common_reverb_low),
                R.drawable.audio_effect_reverb_low, AudioReverbType.DEEP
            )
        )
        data.add(
            ReverbItem(
                context.getString(R.string.common_reverb_loud_and_loud),
                R.drawable.audio_effect_reverb_loud_and_loud,
                AudioReverbType.LOUD
            )
        )
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder {
        val itemView = LayoutInflater.from(parent.context).inflate(
            R.layout.audio_effect_layout_reverb_item,
            parent, false
        )
        return ViewHolder(itemView)
    }

    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        val reverbItem = data[position]
        holder.textTitle.text = reverbItem.title
        holder.imageIcon.setImageResource(reverbItem.icon)
        if (reverbItem.type == audioEffectStore.audioEffectState.audioReverbType.value) {
            holder.imageIcon.setBackgroundResource(R.drawable.audio_effect_settings_item_select_background)
            selectedPosition = data.indexOf(reverbItem)
        } else {
            holder.imageIcon.setBackgroundResource(R.drawable.audio_effect_settings_item_not_select_background)
        }
        holder.layoutRoot.setOnClickListener { view ->
            val index = holder.bindingAdapterPosition
            if (index != RecyclerView.NO_POSITION) {
                audioEffectStore.setAudioReverbType(reverbItem.type)
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

    class ReverbItem(
        val title: String,
        val icon: Int,
        val type: AudioReverbType
    )
}