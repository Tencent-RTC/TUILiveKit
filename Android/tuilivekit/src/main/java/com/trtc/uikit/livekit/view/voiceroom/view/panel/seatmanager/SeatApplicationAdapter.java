package com.trtc.uikit.livekit.view.voiceroom.view.panel.seatmanager;

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

import java.util.ArrayList;
import java.util.List;

public class SeatApplicationAdapter extends RecyclerView.Adapter<SeatApplicationAdapter.ViewHolder> {
    private final Context                         mContext;
    private final SeatState                       mSeatState;
    private final LiveController                  mLiveController;
    private final List<SeatState.SeatApplication> mData;

    public SeatApplicationAdapter(Context context, LiveController liveController) {
        mContext = context;
        mLiveController = liveController;
        mSeatState = liveController.getSeatState();
        mData = new ArrayList<>(mSeatState.seatApplicationList.get());
    }

    @NonNull
    @Override
    public SeatApplicationAdapter.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_voiceroom_item_seat_application,
                parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SeatApplicationAdapter.ViewHolder holder, int position) {
        SeatState.SeatApplication request = mData.get(position);
        if (TextUtils.isEmpty(request.userName)) {
            holder.textName.setText(request.userName);
        } else {
            holder.textName.setText(request.userId);
        }
        if (TextUtils.isEmpty(request.avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, request.avatarUrl, R.drawable.livekit_ic_avatar);
        }
        holder.textAccept.setOnClickListener((view) -> mLiveController.getSeatController().acceptRequest(request.id));
        holder.textReject.setOnClickListener((view) -> mLiveController.getSeatController().rejectRequest(request.id));
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        mData.clear();
        mData.addAll(mSeatState.seatApplicationList.get());
        notifyDataSetChanged();
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public ImageFilterView imageHead;
        public TextView        textName;
        public TextView        textLevel;
        public TextView        textAccept;
        public TextView        textReject;

        public ViewHolder(View itemView) {
            super(itemView);
            imageHead = itemView.findViewById(R.id.iv_head);
            textName = itemView.findViewById(R.id.tv_name);
            textLevel = itemView.findViewById(R.id.tv_level);
            textAccept = itemView.findViewById(R.id.tv_accept);
            textReject = itemView.findViewById(R.id.tv_reject);
        }
    }
}
