package com.trtc.uikit.livekit.view.voiceroom;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.VoiceRoomDefine;

public class VoiceRoomActivity extends FullScreenActivity {

    public static final String INTENT_KEY_ROOM_ID            = "intent_key_room_id";
    public static final String INTENT_KEY_CREATE_ROOM_PARAMS = "intent_key_create_room_params";
    public static final String INTENT_KEY_IS_ANCHOR          = "intent_key_is_anchor";

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.livekit_activity_video_live_audience);

        String roomId = getIntent().getStringExtra(INTENT_KEY_ROOM_ID);
        boolean isAnchor = getIntent().getBooleanExtra(INTENT_KEY_IS_ANCHOR, false);

        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        TUIVoiceRoomFragment voiceRoomFragment;
        TUIVoiceRoomFragment.RoomParams params = new TUIVoiceRoomFragment.RoomParams();
        if (isAnchor) {
            VoiceRoomDefine.CreateRoomParams createRoomParams =
                    (VoiceRoomDefine.CreateRoomParams) getIntent().getSerializableExtra(INTENT_KEY_CREATE_ROOM_PARAMS);
            if (createRoomParams != null) {
                params.maxSeatCount = createRoomParams.maxAnchorCount;
                params.seatMode = createRoomParams.seatMode;
            }
            voiceRoomFragment = new TUIVoiceRoomFragment(roomId, TUIVoiceRoomFragment.RoomBehavior.PREPARE_CREATE,
                    params);
        } else {
            voiceRoomFragment = new TUIVoiceRoomFragment(roomId, TUIVoiceRoomFragment.RoomBehavior.JOIN, null);
        }

        fragmentTransaction.add(R.id.fl_container, voiceRoomFragment);
        fragmentTransaction.commit();
    }
}