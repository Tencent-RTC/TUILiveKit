package com.trtc.uikit.component.audioeffect.view;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType;
import com.trtc.uikit.component.audioeffect.R;
import com.trtc.uikit.component.audioeffect.service.AudioEffectService;

import java.util.ArrayList;
import java.util.List;

public class ReverbAdapter extends RecyclerView.Adapter<ReverbAdapter.ViewHolder> {

    private final Context          mContext;
    private       List<ReverbItem>   mData;
    private final AudioEffectService mAudioEffectService;
    private       int                mSelectedPosition;

    public ReverbAdapter(Context context, AudioEffectService audioEffectService) {
        mContext = context;
        mAudioEffectService = audioEffectService;
        initData();
    }

    private void initData() {
        mData = new ArrayList<>();
        mData.add(new ReverbItem(mContext.getString(R.string.livekit_reverb_none),
                R.drawable.livekit_select_none, TXVoiceReverbType.TXLiveVoiceReverbType_0));
        mData.add(new ReverbItem(mContext.getString(R.string.livekit_reverb_karaoke),
                R.drawable.livekit_reverb_ktv, TXVoiceReverbType.TXLiveVoiceReverbType_1));
        mData.add(new ReverbItem(mContext.getString(R.string.livekit_reverb_metallic_sound),
                R.drawable.livekit_reverb_metallic_sound, TXVoiceReverbType.TXLiveVoiceReverbType_6));
        mData.add(new ReverbItem(mContext.getString(R.string.livekit_reverb_low),
                R.drawable.livekit_reverb_low, TXVoiceReverbType.TXLiveVoiceReverbType_4));
        mData.add(new ReverbItem(mContext.getString(R.string.livekit_reverb_loud_and_loud),
                R.drawable.livekit_reverb_loud_and_loud, TXVoiceReverbType.TXLiveVoiceReverbType_5));
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_anchor_settings_panel_item,
                parent, false);
        return new ViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(ViewHolder holder, int position) {
        final ReverbItem reverbItem = mData.get(position);
        holder.textTitle.setText(reverbItem.title);
        holder.imageIcon.setImageResource(reverbItem.icon);
        if (reverbItem.type == mAudioEffectService.mAudioEffectState.reverbType.get()) {
            holder.imageIcon.setBackgroundResource(R.drawable.livekit_settings_item_select_background);
            mSelectedPosition = mData.indexOf(reverbItem);
        } else {
            holder.imageIcon.setBackgroundResource(R.drawable.livekit_settings_item_not_select_background);
        }
        holder.layoutRoot.setOnClickListener(view -> {
            int index = holder.getBindingAdapterPosition();
            if (index != RecyclerView.NO_POSITION) {
                mAudioEffectService.setVoiceReverbType(reverbItem.type);
                notifyItemChanged(index);
                notifyItemChanged(mSelectedPosition);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public LinearLayout layoutRoot;
        public TextView     textTitle;
        public ImageView    imageIcon;

        public ViewHolder(View itemView) {
            super(itemView);
            layoutRoot = itemView.findViewById(R.id.ll_root);
            textTitle = itemView.findViewById(R.id.tv_title);
            imageIcon = itemView.findViewById(R.id.iv_icon);
        }
    }

    public static class ReverbItem {
        public String            title;
        public int               icon;
        public TXVoiceReverbType type;

        public ReverbItem(String title, int icon, TXVoiceReverbType type) {
            this.title = title;
            this.icon = icon;
            this.type = type;
        }
    }
}