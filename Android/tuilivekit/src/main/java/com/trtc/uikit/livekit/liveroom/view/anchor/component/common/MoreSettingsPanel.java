package com.trtc.uikit.livekit.liveroom.view.anchor.component.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;

import androidx.appcompat.widget.SwitchCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

@SuppressLint("ViewConstructor")
public class MoreSettingsPanel extends LinearLayout {

    private final Context                          mContext;
    private final PopupDialog.DialogActionListener mDismissListener;
    private final LiveRoomInfo                     mLiveRoomInfo;

    public MoreSettingsPanel(Context context, LiveRoomInfo roomInfo, PopupDialog.DialogActionListener listener) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mDismissListener = listener;
        init();
    }

    private void init() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_settings_more_panel, this,
                true);
        SwitchCompat switchHideAudienceNickname = findViewById(R.id.sc_enable_hide_nickname);
        findViewById(R.id.iv_back).setOnClickListener((view) -> {
            if (mDismissListener != null) {
                mDismissListener.dismiss();
            }
        });

        switchHideAudienceNickname.setOnCheckedChangeListener((compoundButton, b) -> {
            mLiveRoomInfo.roomConfig.enableHiddenNickname.set(b);
        });

    }
}
