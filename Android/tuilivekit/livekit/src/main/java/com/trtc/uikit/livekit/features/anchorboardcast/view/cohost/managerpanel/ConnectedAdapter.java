package com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.managerpanel;

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

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class ConnectedAdapter extends RecyclerView.Adapter<ConnectedAdapter.LinkMicViewHolder> {

    private final AnchorManager mManager;
    private final Context       mContext;

    private final CopyOnWriteArrayList<TUILiveConnectionManager.ConnectionUser> mData = new CopyOnWriteArrayList<>();

    public ConnectedAdapter(Context context, AnchorManager liveStreamManager) {
        mContext = context;
        mManager = liveStreamManager;
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
        TUILiveConnectionManager.ConnectionUser connectionUser = mData.get(position);
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
    public void updateData(List<TUILiveConnectionManager.ConnectionUser> liveList) {
        mData.clear();
        for (TUILiveConnectionManager.ConnectionUser user : liveList) {
            if (!TextUtils.equals(user.userId, mManager.getState().loginUserInfo.userId)) {
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
