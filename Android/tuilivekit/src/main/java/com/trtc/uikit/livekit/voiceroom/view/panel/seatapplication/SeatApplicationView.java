package com.trtc.uikit.livekit.voiceroom.view.panel.seatapplication;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.state.operation.SeatState;
import com.trtc.uikit.livekit.common.view.BasicView;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class SeatApplicationView extends BasicView {

    private SeatApplicationAdapter    mAdapter;
    private OnBackButtonClickListener mOnBackButtonClickListener;

    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mSeatApplicationListObserver =
            (list) -> mAdapter.updateData();

    public SeatApplicationView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    public void setOnBackButtonClickListener(OnBackButtonClickListener listener) {
        mOnBackButtonClickListener = listener;
    }

    @Override
    protected void initView() {
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_seat_application_panel, this,
                true);
        RecyclerView recycleAudienceList = rootView.findViewById(R.id.rv_link_user_list);
        recycleAudienceList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mAdapter = new SeatApplicationAdapter(mContext, mLiveController);
        recycleAudienceList.setAdapter(mAdapter);
        ImageView imageBack = rootView.findViewById(R.id.iv_back);
        imageBack.setOnClickListener(view -> {
            if (mOnBackButtonClickListener != null) {
                mOnBackButtonClickListener.onClick();
            }
        });
    }

    @Override
    protected void addObserver() {
        mSeatState.seatApplicationList.observe(mSeatApplicationListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatApplicationList.remove(mSeatApplicationListObserver);
    }

    public interface OnBackButtonClickListener {
        void onClick();
    }
}

