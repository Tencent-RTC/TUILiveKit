package com.trtc.uikit.livekit.view.voiceroom.view.panel.settings;

import static com.trtc.uikit.livekit.view.voiceroom.view.panel.settings.SettingsListAdapter.ITEM_COUNT;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class SettingsPanelView extends BottomPanelView {
    public SettingsPanelView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_settings_panel, this, true);
        initSettingsListView();
    }

    private void initSettingsListView() {
        RecyclerView recycleSettingsList = findViewById(R.id.rv_settings_list);
        recycleSettingsList.setLayoutManager(new GridLayoutManager(mContext, ITEM_COUNT));
        SettingsListAdapter adapter = new SettingsListAdapter(mContext, mLiveController);
        recycleSettingsList.addItemDecoration(new SettingsListAdapter.SpaceItemDecoration(mContext));
        recycleSettingsList.setAdapter(adapter);
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }
}
