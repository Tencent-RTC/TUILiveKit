package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class SettingsPanel extends BasicView {
    public SettingsPanel(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_settings_panel, this, true);

        initSettingsListView();
    }

    private void initSettingsListView() {
        RecyclerView mRecycleSettingsList = findViewById(R.id.rv_settings_list);
        mRecycleSettingsList.setLayoutManager(new GridLayoutManager(mContext, 5));

        SettingsListAdapter mAdapter = new SettingsListAdapter(mContext, mLiveController);
        mRecycleSettingsList.setAdapter(mAdapter);
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }
}
