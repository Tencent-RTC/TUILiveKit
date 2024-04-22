package com.trtc.uikit.livekit.liveroom.view.anchor.component.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

@SuppressLint("ViewConstructor")
public class SettingsPanel extends LinearLayout {

    private final Context           mContext;
    private final LiveRoomInfo      mLiveRoomInfo;
    private final RoomEngineService mRoomEngineService;

    public SettingsPanel(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        init();
    }

    private void init() {
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_settings_panel, this,
                true);

        RecyclerView mRecycleSettingsList = rootView.findViewById(R.id.rv_settings_list);
        mRecycleSettingsList.setLayoutManager(new GridLayoutManager(mContext, 5));

        SettingsListAdapter mAdapter = new SettingsListAdapter(mContext, mLiveRoomInfo, mRoomEngineService);
        mRecycleSettingsList.setAdapter(mAdapter);
    }
}
