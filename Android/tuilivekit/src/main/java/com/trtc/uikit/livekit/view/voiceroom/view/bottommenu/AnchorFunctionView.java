package com.trtc.uikit.livekit.view.voiceroom.view.bottommenu;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.TextView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.BottomPanel;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.voiceroom.view.panel.seatmanager.SeatManagerView;
import com.trtc.uikit.livekit.view.voiceroom.view.panel.settings.SettingsPanelView;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class AnchorFunctionView extends BasicView {
    private TextView    mSeatApplicationCountText;
    private BottomPanel mSettingsPanelView;
    private BottomPanel mSeatManagerPanel;

    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mSeatApplicationListObserver =
            this::updateSeatApplicationCountText;

    public AnchorFunctionView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_anchor_function, this, true);
        mSeatApplicationCountText = findViewById(R.id.application_count);
        findViewById(R.id.iv_settings).setOnClickListener(v -> showSettingsPanel());
        findViewById(R.id.iv_seat_management).setOnClickListener(v -> showSeatManagementPanel());
    }

    @Override
    protected void addObserver() {
        mSeatState.seatApplicationList.observe(mSeatApplicationListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatApplicationList.removeObserver(mSeatApplicationListObserver);
    }

    private void showSettingsPanel() {
        if (mSettingsPanelView == null) {
            SettingsPanelView panelView = new SettingsPanelView(mContext, mLiveController);
            mSettingsPanelView = BottomPanel.create(panelView);
        }
        mSettingsPanelView.show();
    }

    private void showSeatManagementPanel() {
        if (mSeatManagerPanel == null) {
            SeatManagerView panelView = new SeatManagerView(mContext, mLiveController);
            mSeatManagerPanel = BottomPanel.create(panelView);
        }
        mSeatManagerPanel.show();
    }

    private void updateSeatApplicationCountText(LinkedHashSet<SeatState.SeatApplication> list) {
        if (list.isEmpty()) {
            mSeatApplicationCountText.setVisibility(INVISIBLE);
        } else {
            mSeatApplicationCountText.setVisibility(VISIBLE);
            mSeatApplicationCountText.setText(String.valueOf(list.size()));
        }
    }
}
