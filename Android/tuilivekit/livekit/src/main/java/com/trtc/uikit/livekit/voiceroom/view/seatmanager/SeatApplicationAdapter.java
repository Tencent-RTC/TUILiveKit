package com.trtc.uikit.livekit.voiceroom.view.seatmanager;

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
import com.trtc.uikit.livekit.voiceroom.manager.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;

import java.util.ArrayList;
import java.util.List;

public class SeatApplicationAdapter extends RecyclerView.Adapter<SeatApplicationAdapter.ViewHolder> {
    private static final String FILE = "SeatApplicationAdapter";

    private final Context                         mContext;
    private final SeatState                       mSeatState;
    private final VoiceRoomManager                mVoiceRoomManager;
    private final SeatGridView                    mSeatGridView;
    private final List<SeatState.SeatApplication> mData;

    public SeatApplicationAdapter(Context context, VoiceRoomManager voiceRoomManager, SeatGridView seatGridView) {
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        mSeatGridView = seatGridView;
        mSeatState = voiceRoomManager.getSeatState();
        mData = new ArrayList<>(mSeatState.seatApplicationList.getValue());
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
        if (!TextUtils.isEmpty(request.userName)) {
            holder.textName.setText(request.userName);
        } else {
            holder.textName.setText(request.userId);
        }
        if (TextUtils.isEmpty(request.avatarUrl)) {
            holder.imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, holder.imageHead, request.avatarUrl, R.drawable.livekit_ic_avatar);
        }
        holder.textAccept.setOnClickListener((view) -> responseSeatInvitation(request.userId, true));
        holder.textReject.setOnClickListener((view) -> responseSeatInvitation(request.userId, false));
    }

    private void responseSeatInvitation(String userId, boolean isAgree) {
        mSeatGridView.responseRemoteRequest(userId, isAgree, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVoiceRoomManager.getSeatManager().removeSeatApplication(userId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "responseRemoteRequest failed,error:" + error + ",message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        mData.clear();
        mData.addAll(mSeatState.seatApplicationList.getValue());
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
