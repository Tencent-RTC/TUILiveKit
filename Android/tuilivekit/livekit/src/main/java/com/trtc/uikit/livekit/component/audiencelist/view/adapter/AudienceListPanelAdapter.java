package com.trtc.uikit.livekit.component.audiencelist.view.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.audiencelist.store.AudienceListState;

import java.util.concurrent.CopyOnWriteArrayList;

public class AudienceListPanelAdapter extends RecyclerView.Adapter<AudienceListPanelAdapter.ViewHolder> {

    private final Context                                      mContext;
    private       CopyOnWriteArrayList<TUIRoomDefine.UserInfo> mData;
    private final AudienceListState                            mAudienceListState;
    private       AudienceListView.OnUserItemClickListener     mOnItemClickListener;

    public AudienceListPanelAdapter(Context context, AudienceListState audienceListState) {
        mContext = context;
        mAudienceListState = audienceListState;
        mData = new CopyOnWriteArrayList<>(mAudienceListState.audienceList.getValue());
    }

    public void setOnItemClickListener(AudienceListView.OnUserItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(
                R.layout.audience_list_layout_panel_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.itemView.setOnClickListener(v -> {
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onUserItemClick(mData.get(position));
            }
        });
        if (TextUtils.isEmpty(mData.get(position).avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.audience_list_default_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl,
                    R.drawable.audience_list_default_avatar);
        }

        if (TextUtils.isEmpty(mData.get(position).userName)) {
            holder.textName.setText(mData.get(position).userId);
        } else {
            holder.textName.setText(mData.get(position).userName);
        }

        String selfUserId = TUIRoomEngine.getSelfInfo().userId;
        if (!TextUtils.isEmpty(selfUserId) && selfUserId.equals(mAudienceListState.ownerId)) {
            holder.more.setVisibility(mOnItemClickListener == null ? View.GONE : View.VISIBLE);
        } else {
            holder.more.setVisibility(View.GONE);
        }
        holder.textLevel.setVisibility(View.GONE);
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        mData = new CopyOnWriteArrayList<>(mAudienceListState.audienceList.getValue());
        notifyDataSetChanged();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;
        public ImageView       more;

        public ViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
            more = itemView.findViewById(R.id.more);
        }
    }
}
