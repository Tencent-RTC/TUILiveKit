package com.trtc.uikit.livekit.voiceroom.view.preview;

import android.content.Context;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.SwitchCompat;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;

public class SettingsDialog extends PopupDialog {
    private final Context          mContext;
    private final VoiceRoomManager mVoiceRoomManager;

    private SwitchCompat mSwitchCompat;

    private final Observer<TUIRoomDefine.SeatMode> mSeatModeObserver = this::updateSeatMode;

    public SettingsDialog(@NonNull Context context, VoiceRoomManager voiceRoomManager) {
        super(context);
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        initView();
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    protected void initView() {
        View rootView = View.inflate(mContext, R.layout.livekit_voiceroom_preview_settings, null);
        setView(rootView);
        mSwitchCompat = rootView.findViewById(R.id.need_request);
        mSwitchCompat.setOnCheckedChangeListener((compoundButton, enable) -> onSeatModeClicked(enable));
    }

    protected void addObserver() {
        mVoiceRoomManager.getRoomState().seatMode.observeForever(mSeatModeObserver);
    }

    protected void removeObserver() {
        mVoiceRoomManager.getRoomState().seatMode.removeObserver(mSeatModeObserver);
    }

    private void onSeatModeClicked(boolean enable) {
        TUIRoomDefine.SeatMode seatMode = enable ? TUIRoomDefine.SeatMode.APPLY_TO_TAKE :
                TUIRoomDefine.SeatMode.FREE_TO_TAKE;
        mVoiceRoomManager.getRoomManager().updateSeatMode(seatMode);
    }

    private void updateSeatMode(TUIRoomDefine.SeatMode seatMode) {
        mSwitchCompat.setChecked(seatMode == TUIRoomDefine.SeatMode.APPLY_TO_TAKE);
    }
}

