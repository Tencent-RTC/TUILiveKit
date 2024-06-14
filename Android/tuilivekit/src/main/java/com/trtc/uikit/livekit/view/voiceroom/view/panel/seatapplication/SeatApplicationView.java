package com.trtc.uikit.livekit.view.voiceroom.view.panel.seatapplication;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.common.view.BottomPanelView;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class SeatApplicationView extends BottomPanelView {

    private SeatApplicationAdapter mAdapter;

    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mSeatApplicationListObserver =
            (list) -> mAdapter.updateData();

    public SeatApplicationView(Context context, LiveController liveController) {
        super(context, liveController);
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
        imageBack.setOnClickListener(view -> onBackButtonClick());
    }

    @Override
    protected void addObserver() {
        mSeatState.seatApplicationList.observe(mSeatApplicationListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatApplicationList.removeObserver(mSeatApplicationListObserver);
    }
}

