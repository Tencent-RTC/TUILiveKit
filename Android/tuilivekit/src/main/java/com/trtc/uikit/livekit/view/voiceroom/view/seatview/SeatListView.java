package com.trtc.uikit.livekit.view.voiceroom.view.seatview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.common.view.BasicView;

import java.util.List;

@SuppressLint("ViewConstructor")
public class SeatListView extends BasicView {

    private       SeatAdapter                        mAdapter;
    private final Observer<List<SeatState.SeatInfo>> mSeatListObserver =
            (List<SeatState.SeatInfo> audienceList) -> mAdapter.updateData();

    public SeatListView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_seat_list, this, true);
        GridLayoutManager gridLayoutManager = new GridLayoutManager(mContext, 4) {
            @Override
            public boolean canScrollVertically() {
                return false;
            }
        };
        RecyclerView recycleView = rootView.findViewById(R.id.rv_seat_list);
        recycleView.setLayoutManager(gridLayoutManager);
        mAdapter = new SeatAdapter(mContext, mLiveController);
        recycleView.setAdapter(mAdapter);
    }

    @Override
    protected void addObserver() {
        mSeatState.seatList.observe(mSeatListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatList.removeObserver(mSeatListObserver);
    }
}