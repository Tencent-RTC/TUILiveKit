package com.trtc.uikit.livekit.voiceroom.view.preview;

import static com.trtc.uikit.livekit.voiceroom.state.RoomState.LayoutType.KTVRoom;
import static com.trtc.uikit.livekit.voiceroom.state.RoomState.LayoutType.VoiceRoom;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.RoomManager;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;

public class LayoutSettingPanel extends PopupDialog {
    private final Context mContext;
    private LinearLayout mLayoutKTVRoom;
    private LinearLayout mLayoutVoiceRoom;
    private final RoomState mRoomState;
    private final RoomManager mRoomManager;
    private final Observer<RoomState.LayoutType> mVoiceRoomLayoutObserver = this::onVoiceRoomLayoutChanged;

    public LayoutSettingPanel(@NonNull Context context, VoiceRoomManager voiceRoomManager) {
        super(context);
        mContext = context;
        mRoomState = voiceRoomManager.getState().roomState;
        mRoomManager = voiceRoomManager.getRoomManager();
        initView();
    }

    private void initView() {
        View view = LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_anchor_preview_layout, null);
        initChatRoomView(view);
        initKTVView(view);

        setView(view);
    }

    private void initChatRoomView(View view) {
        mLayoutVoiceRoom = view.findViewById(R.id.ll_voice_room);
        mLayoutVoiceRoom.setOnClickListener(v -> {
            mRoomManager.setVoiceRoomLayout(VoiceRoom);
            dismiss();
        });
    }

    private void initKTVView(View view) {
        mLayoutKTVRoom = view.findViewById(R.id.ll_ktv);
        mLayoutKTVRoom.setOnClickListener(v -> {
            mRoomManager.setVoiceRoomLayout(KTVRoom);
            dismiss();
        });
    }

    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    protected void addObserver() {
        mRoomState.layoutType.observeForever(mVoiceRoomLayoutObserver);
    }

    protected void removeObserver() {
        mRoomState.layoutType.removeObserver(mVoiceRoomLayoutObserver);
    }

    private void onVoiceRoomLayoutChanged(RoomState.LayoutType layoutType) {
        if (VoiceRoom == layoutType) {
            mLayoutVoiceRoom.setBackgroundResource(R.drawable.livekit_settings_item_select_background);
            mLayoutKTVRoom.setBackgroundResource(R.drawable.livekit_settings_item_not_select_background);
        } else {
            mLayoutVoiceRoom.setBackgroundResource(R.drawable.livekit_settings_item_not_select_background);
            mLayoutKTVRoom.setBackgroundResource(R.drawable.livekit_settings_item_select_background);
        }
    }

}