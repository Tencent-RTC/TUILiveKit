package com.trtc.uikit.livekit.view.voiceroom.view.seatview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;

import java.util.List;

@SuppressLint("ViewConstructor")
public class SeatListView extends BasicView {

    private final Config mConfig;
    private       SeatAdapter                        mAdapter;
    private       GridLayoutManager                  mGridLayoutManager;
    private final Observer<List<SeatState.SeatInfo>> mSeatListObserver = this::updateSeatListView;

    public SeatListView(Context context, LiveController liveController, Config config) {
        super(context, liveController);
        mConfig = config;
    }

    @Override
    protected void initView() {
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_seat_list, this, true);
        mGridLayoutManager = new GridLayoutManager(mContext, 5) {
            @Override
            public boolean canScrollVertically() {
                return false;
            }
        };
        RecyclerView recycleView = rootView.findViewById(R.id.rv_seat_list);
        recycleView.setLayoutManager(mGridLayoutManager);
        mAdapter = new SeatAdapter(mContext, mLiveController, mConfig);
        recycleView.addItemDecoration(new SeatAdapter.SpaceItemDecoration(mContext));
        recycleView.setAdapter(mAdapter);
    }

    private void updateSeatListView(List<SeatState.SeatInfo> seatList) {
        int size = seatList.size();
        switch (size) {
            case 3:
            case 6:
            case 9:
                mGridLayoutManager.setSpanCount(3);
                break;
            case 4:
            case 8:
            case 12:
            case 16:
                mGridLayoutManager.setSpanCount(4);
                break;
            default:
                mGridLayoutManager.setSpanCount(5);
                break;
        }
        mAdapter.updateData();
    }

    @Override
    protected void addObserver() {
        mSeatState.seatList.observe(mSeatListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatList.removeObserver(mSeatListObserver);
    }

    public static class Config {
        public boolean isPreview = false;
    }
}