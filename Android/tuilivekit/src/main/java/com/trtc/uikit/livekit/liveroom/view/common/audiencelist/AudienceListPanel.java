package com.trtc.uikit.livekit.liveroom.view.common.audiencelist;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.concurrent.CopyOnWriteArraySet;

@SuppressLint("ViewConstructor")
public class AudienceListPanel extends LinearLayout {

    private final Context                                 mContext;
    private final LiveRoomInfo                            mLiveRoomInfo;
    private       AudienceListPanelAdapter                mAdapter;
    private final Observer<CopyOnWriteArraySet<UserInfo>> mAudienceListObserver = (audienceList) -> {
        mAdapter.updateData();
    };

    public AudienceListPanel(Context context, LiveRoomInfo roomInfo) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        init();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mLiveRoomInfo.audienceList.observe(mAudienceListObserver);
    }

    private void removeObserver() {
        mLiveRoomInfo.audienceList.removeObserver(mAudienceListObserver);
    }

    private void init() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_audience_list_panel
                , this, true);

        initView();
    }

    private void initView() {
        RecyclerView mRecycleAudienceList = findViewById(R.id.rv_audience_list);

        mRecycleAudienceList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));

        mAdapter = new AudienceListPanelAdapter(mContext, mLiveRoomInfo);
        mRecycleAudienceList.setAdapter(mAdapter);
    }
}
