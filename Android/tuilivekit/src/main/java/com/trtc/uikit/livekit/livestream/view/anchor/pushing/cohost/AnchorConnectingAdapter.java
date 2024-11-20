package com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost;

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

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoHostState;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorConnectingAdapter extends RecyclerView.Adapter<AnchorConnectingAdapter.LinkMicViewHolder> {

    private final LiveStreamManager mLiveStreamManager;
    private final Context           mContext;

    private final CopyOnWriteArrayList<CoHostState.ConnectionUser> mData = new CopyOnWriteArrayList<>();

    public AnchorConnectingAdapter(Context context, LiveStreamManager liveStreamManager) {
        mContext = context;
        mLiveStreamManager = liveStreamManager;
        initData();
    }

    @NonNull
    @Override
    public LinkMicViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_connecting_item, parent,
                false);
        return new LinkMicViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull LinkMicViewHolder holder, int position) {
        CoHostState.ConnectionUser connectionUser = mData.get(position);
        if (TextUtils.isEmpty(connectionUser.userName)) {
            holder.textName.setText(connectionUser.roomId);
        } else {
            holder.textName.setText(connectionUser.userName);
        }
        if (TextUtils.isEmpty(connectionUser.avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, connectionUser.avatarUrl,
                    R.drawable.livekit_ic_avatar);
        }
    }

    private void initData() {
        mData.clear();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData(List<CoHostState.ConnectionUser> liveList) {
        mData.clear();
        for (CoHostState.ConnectionUser user : liveList) {
            if (!TextUtils.equals(user.userId, mLiveStreamManager.getUserState().selfInfo.userId)) {
                mData.add(user);
            }
        }
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class LinkMicViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;

        public LinkMicViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
        }
    }
}
