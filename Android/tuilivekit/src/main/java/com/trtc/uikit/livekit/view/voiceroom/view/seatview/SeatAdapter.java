package com.trtc.uikit.livekit.view.voiceroom.view.seatview;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;

import java.util.ArrayList;
import java.util.List;

public class SeatAdapter extends RecyclerView.Adapter<SeatAdapter.ViewHolder> {
    private final Context                  mContext;
    private       List<SeatState.SeatInfo> mList;
    private       SeatState                mSeatState;
    private final LiveController           mLiveController;

    public SeatAdapter(Context context, LiveController liveController) {
        mContext = context;
        mLiveController = liveController;
        mSeatState = liveController.getSeatState();
        mList = new ArrayList<>(mSeatState.seatList.get());
        mSeatState = liveController.getSeatState();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateData() {
        mList = new ArrayList<>(mSeatState.seatList.get());
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        Context context = parent.getContext();
        LayoutInflater inflater = LayoutInflater.from(context);
        View view = inflater.inflate(R.layout.livekit_voiceroom_item_seat_layout, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(ViewHolder holder, int position) {
        SeatState.SeatInfo seatInfo = mList.get(position);
        SeatInfoView seatInfoView = new SeatInfoView(mContext, mLiveController, seatInfo);
        holder.mLayoutSeatInfoContainer.removeAllViews();
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        holder.mLayoutSeatInfoContainer.addView(seatInfoView, layoutParams);
    }

    @Override
    public int getItemCount() {
        return mList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public LinearLayout mLayoutSeatInfoContainer;

        public ViewHolder(View itemView) {
            super(itemView);
            initView(itemView);
        }

        private void initView(@NonNull final View itemView) {
            mLayoutSeatInfoContainer = itemView.findViewById(R.id.ll_seat_item);
        }
    }
}