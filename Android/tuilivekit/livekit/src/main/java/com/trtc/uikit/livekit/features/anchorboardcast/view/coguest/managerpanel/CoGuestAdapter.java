package com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.managerpanel;

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

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.concurrent.CopyOnWriteArrayList;

public class CoGuestAdapter extends RecyclerView.Adapter<CoGuestAdapter.LinkMicViewHolder> {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("AnchorCoGuestAdapter");

    private final AnchorManager mLiveManager;
    private final LiveCoreView  mCoreView;
    private final Context                                      mContext;
    private final CopyOnWriteArrayList<TUIRoomDefine.UserInfo> mData = new CopyOnWriteArrayList<>();

    public CoGuestAdapter(Context context, AnchorManager manager) {
        mContext = context;
        mLiveManager = manager;
        mCoreView = manager.getCoreView();

        initData();
    }

    @NonNull
    @Override
    public LinkMicViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view;
        view = LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_link_mic_item, parent, false);
        return new LinkMicViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull LinkMicViewHolder holder, int position) {
        if (TextUtils.isEmpty(mData.get(position).userName)) {
            holder.textName.setText(mData.get(position).userId);
        } else {
            holder.textName.setText(mData.get(position).userName);
        }

        if (TextUtils.isEmpty(mData.get(position).avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl,
                    R.drawable.livekit_ic_avatar);
        }
        holder.textHangUp.setTag(mData.get(position));
        holder.textHangUp.setEnabled(true);
        holder.textHangUp.setOnClickListener((view) -> {
            view.setEnabled(false);
            final CoGuestState.SeatInfo seatInfo = (CoGuestState.SeatInfo) view.getTag();
            mCoreView.disconnectUser(seatInfo.userId.getValue(), new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {

                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorLocalized.onError(error);
                    LOGGER.error("AnchorCoGuestAdapter" + " disconnectUser failed:error:" + error + "," +
                            "errorCode:" + error.getValue() + "message:" + message);
                }
            });
        });
    }

    private void initData() {
        mData.clear();
        mData.addAll(mLiveManager.getCoreState().coGuestState.connectedUserList.getValue());

        for (TUIRoomDefine.UserInfo userInfo : mData) {
            if (userInfo.userId.equals(mLiveManager.getState().loginUserInfo.userId)) {
                mData.remove(userInfo);
                break;
            }
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        initData();
        notifyDataSetChanged();
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class LinkMicViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;
        public TextView        textHangUp;

        public LinkMicViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
            textHangUp = itemView.findViewById(R.id.tv_hang_up);
        }
    }
}
