package com.trtc.uikit.livekit.common.uicomponent.audioeffect;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.state.operation.MediaState;

import java.util.ArrayList;
import java.util.List;

public class ChangeVoiceAdapter extends RecyclerView.Adapter<ChangeVoiceAdapter.ViewHolder> {

    private final Context               mContext;
    private final LiveController        mLiveController;
    private final MediaState            mMediaState;
    private       List<ChangeVoiceItem> mData;
    private       int                   mSelectedPosition;

    public ChangeVoiceAdapter(Context context, LiveController liveController) {
        mContext = context;
        mLiveController = liveController;
        mMediaState = liveController.getMediaState();
        initData();
    }

    private void initData() {
        mData = new ArrayList<>();
        mData.add(new ChangeVoiceItem(mContext.getString(R.string.livekit_change_voice_none),
                R.drawable.livekit_select_none, TXVoiceChangerType.TXLiveVoiceChangerType_0));
        mData.add(new ChangeVoiceItem(mContext.getString(R.string.livekit_change_voice_child),
                R.drawable.livekit_change_voice_child, TXVoiceChangerType.TXLiveVoiceChangerType_1));
        mData.add(new ChangeVoiceItem(mContext.getString(R.string.livekit_change_voice_girl),
                R.drawable.livekit_change_voice_girl, TXVoiceChangerType.TXLiveVoiceChangerType_2));
        mData.add(new ChangeVoiceItem(mContext.getString(R.string.livekit_change_voice_uncle),
                R.drawable.livekit_change_voice_uncle, TXVoiceChangerType.TXLiveVoiceChangerType_3));
        mData.add(new ChangeVoiceItem(mContext.getString(R.string.livekit_change_voice_ethereal),
                R.drawable.livekit_change_voice_ethereal, TXVoiceChangerType.TXLiveVoiceChangerType_11));
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
        final ChangeVoiceItem changeVoiceItem = mData.get(position);
        holder.textTitle.setText(changeVoiceItem.title);
        holder.imageIcon.setImageResource(changeVoiceItem.icon);
        if (changeVoiceItem.type == mMediaState.audioInfo.changerType.get()) {
            holder.imageIcon.setBackgroundResource(R.drawable.livekit_settings_item_select_background);
            mSelectedPosition = mData.indexOf(changeVoiceItem);
        } else {
            holder.imageIcon.setBackgroundResource(R.drawable.livekit_settings_item_not_select_background);
        }
        holder.layoutRoot.setOnClickListener(view -> {
            int index = holder.getBindingAdapterPosition();
            if (index != RecyclerView.NO_POSITION) {
                mLiveController.getMediaController().setVoiceChangerType(changeVoiceItem.type);
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

    public static class ChangeVoiceItem {
        public String             title;
        public int                icon;
        public TXVoiceChangerType type;

        public ChangeVoiceItem(String title, int icon, TXVoiceChangerType type) {
            this.title = title;
            this.icon = icon;
            this.type = type;
        }
    }
}