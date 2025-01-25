package com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest;

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
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorCoGuestAdapter extends RecyclerView.Adapter<AnchorCoGuestAdapter.LinkMicViewHolder> {

    private final LiveStreamManager                           mLiveManager;
    private final LiveCoreView                                mLiveStream;
    private final Context                                     mContext;
    private final CopyOnWriteArrayList<CoGuestState.SeatInfo> mData = new CopyOnWriteArrayList<>();

    public AnchorCoGuestAdapter(Context context, LiveStreamManager manager, LiveCoreView liveStream) {
        mContext = context;
        mLiveManager = manager;
        mLiveStream = liveStream;

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
        if (TextUtils.isEmpty(mData.get(position).name.get())) {
            holder.textName.setText(mData.get(position).userId.get());
        } else {
            holder.textName.setText(mData.get(position).name.get());
        }

        if (TextUtils.isEmpty(mData.get(position).avatarUrl.get())) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, mData.get(position).avatarUrl.get(),
                    R.drawable.livekit_ic_avatar);
        }
        holder.textHangUp.setTag(mData.get(position));
        holder.textHangUp.setEnabled(true);
        holder.textHangUp.setOnClickListener((view) -> {
            view.setEnabled(false);
            final CoGuestState.SeatInfo seatInfo = (CoGuestState.SeatInfo) view.getTag();
            mLiveStream.disconnectUser(seatInfo.userId.get(), new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {

                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {

                }
            });
        });
    }

    private void initData() {
        mData.clear();
        mData.addAll(mLiveManager.getCoGuestState().connectedUserList.get());

        CoGuestState.SeatInfo selfSeatInfo = new CoGuestState.SeatInfo();
        selfSeatInfo.userId.set(mLiveManager.getUserState().selfInfo.userId);
        mData.remove(selfSeatInfo);
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
