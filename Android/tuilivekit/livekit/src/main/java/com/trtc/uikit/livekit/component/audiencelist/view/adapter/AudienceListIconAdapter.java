package com.trtc.uikit.livekit.component.audiencelist.view.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.audiencelist.store.AudienceListState;

import java.util.ArrayList;
import java.util.List;

public class AudienceListIconAdapter extends RecyclerView.Adapter<AudienceListIconAdapter.ViewHolder> {

    private final Context                      mContext;
    private       List<TUIRoomDefine.UserInfo> mData;
    private final AudienceListState            mAudienceListState;

    public AudienceListIconAdapter(Context context, AudienceListState audienceListState) {
        mContext = context;
        mAudienceListState = audienceListState;
        mData = new ArrayList<>(mAudienceListState.audienceList.getValue());
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(
                R.layout.audience_list_layout_icon_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        if (TextUtils.isEmpty(mData.get(position).avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.audience_list_default_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl,
                    R.drawable.audience_list_default_avatar);
        }
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        mData = new ArrayList<>(mAudienceListState.audienceList.getValue());
        notifyDataSetChanged();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public RelativeLayout  layoutRoot;

        public ViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            layoutRoot = itemView.findViewById(R.id.rl_root);
        }
    }
}
