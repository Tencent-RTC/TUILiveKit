package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.link;

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
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;

import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorLinkMicAdapter extends RecyclerView.Adapter<AnchorLinkMicAdapter.LinkMicViewHolder> {

    private final LiveController                           mLiveController;
    private final Context                                  mContext;
    private final CopyOnWriteArrayList<SeatState.SeatInfo> mData = new CopyOnWriteArrayList<>();

    public AnchorLinkMicAdapter(Context context, LiveController liveController) {
        mContext = context;
        mLiveController = liveController;

        initData();
    }

    @NonNull
    @Override
    public AnchorLinkMicAdapter.LinkMicViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view;
        view = LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_link_mic_item, parent, false);
        return new LinkMicViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull AnchorLinkMicAdapter.LinkMicViewHolder holder, int position) {
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
        holder.textHangUp.setOnClickListener((view) -> {
            final SeatState.SeatInfo seatInfo = (SeatState.SeatInfo) view.getTag();
            mLiveController.getSeatController().kickUserOffSeatByAdmin(seatInfo);
        });
    }

    private void initData() {
        mData.clear();
        mData.addAll(mLiveController.getSeatState().seatList.get());

        SeatState.SeatInfo selfSeatInfo = new SeatState.SeatInfo();
        selfSeatInfo.userId.set(mLiveController.getUserState().selfInfo.userId);
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
