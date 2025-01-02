package com.trtc.uikit.component.audiencelist.view.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.component.audiencelist.R;
import com.trtc.uikit.component.audiencelist.store.AudienceListState;

import java.util.concurrent.CopyOnWriteArrayList;

public class AudienceListPanelAdapter extends RecyclerView.Adapter<AudienceListPanelAdapter.ViewHolder> {

    private final Context                                      mContext;
    private       CopyOnWriteArrayList<TUIRoomDefine.UserInfo> mData;
    private final AudienceListState                            mAudienceListState;

    public AudienceListPanelAdapter(Context context, AudienceListState audienceListState) {
        mContext = context;
        mAudienceListState = audienceListState;
        mData = new CopyOnWriteArrayList<>(mAudienceListState.audienceList.get());
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(
                R.layout.livekit_layout_anchor_live_audience_list_panel_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        if (TextUtils.isEmpty(mData.get(position).avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl,
                    R.drawable.livekit_ic_avatar);
        }

        if (TextUtils.isEmpty(mData.get(position).userName)) {
            holder.textName.setText(mData.get(position).userId);
        } else {
            holder.textName.setText(mData.get(position).userName);
        }

        holder.textLevel.setVisibility(View.GONE);
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        mData = new CopyOnWriteArrayList<>(mAudienceListState.audienceList.get());
        notifyDataSetChanged();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;

        public ViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
        }
    }
}
