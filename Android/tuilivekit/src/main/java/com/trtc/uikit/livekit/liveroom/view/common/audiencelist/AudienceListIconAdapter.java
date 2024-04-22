package com.trtc.uikit.livekit.liveroom.view.common.audiencelist;

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

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.concurrent.CopyOnWriteArrayList;

public class AudienceListIconAdapter extends RecyclerView.Adapter<AudienceListIconAdapter.ViewHolder> {

    private final Context                        mContext;
    private final LiveRoomInfo                   mLiveRoomInfo;
    private       CopyOnWriteArrayList<UserInfo> mData;

    public AudienceListIconAdapter(Context context, LiveRoomInfo roomInfo) {
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mData = new CopyOnWriteArrayList<>(mLiveRoomInfo.audienceList.get());
    }


    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(
                R.layout.livekit_layout_anchor_live_audience_list_icon_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(ViewHolder holder, int position) {
        RecyclerView.LayoutParams layoutParams = (RecyclerView.LayoutParams) holder.layoutRoot.getLayoutParams();
        if (position == 0) {
            layoutParams.leftMargin = 0;
        } else {
            layoutParams.leftMargin = ScreenUtil.dip2px(-9);
        }
        holder.layoutRoot.setLayoutParams(layoutParams);
        if (TextUtils.isEmpty(mData.get(position).avatarUrl.get())) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl.get(),
                    R.drawable.livekit_ic_avatar);
        }
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        mData = new CopyOnWriteArrayList<>(mLiveRoomInfo.audienceList.get());
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
