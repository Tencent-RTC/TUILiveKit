package com.trtc.uikit.livekit.voiceroom.view;

import android.content.Context;
import android.content.res.Configuration;
import android.os.Bundle;
import android.view.View;
import android.view.WindowManager;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomDefine;
import com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomParams;
import com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior;

public class VoiceRoomActivity extends FullScreenActivity {

    public static final String INTENT_KEY_ROOM_ID            = "intent_key_room_id";
    public static final String INTENT_KEY_CREATE_ROOM_PARAMS = "intent_key_create_room_params";
    public static final String INTENT_KEY_ROOM_BEHAVIOR      = "intent_key_room_behavior";

    @Override
    protected void attachBaseContext(Context context) {
        super.attachBaseContext(context);
        if (context != null) {
            Configuration configuration = context.getResources().getConfiguration();
            configuration.fontScale = 1;
            applyOverrideConfiguration(configuration);
        }
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(null);
        setContentView(R.layout.livekit_activity_video_live_audience);
        getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        getWindow().getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN);

        String roomId = getIntent().getStringExtra(INTENT_KEY_ROOM_ID);
        RoomBehavior behavior = RoomBehavior.values()[getIntent().getIntExtra(INTENT_KEY_ROOM_BEHAVIOR, 0)];
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        RoomParams params = new RoomParams();
        VoiceRoomDefine.CreateRoomParams createRoomParams =
                (VoiceRoomDefine.CreateRoomParams) getIntent().getSerializableExtra(INTENT_KEY_CREATE_ROOM_PARAMS);
        if (createRoomParams != null) {
            params.maxSeatCount = createRoomParams.maxAnchorCount;
            params.seatMode = createRoomParams.seatMode;
        }
        TUIVoiceRoomFragment voiceRoomFragment = new TUIVoiceRoomFragment(roomId, behavior, params);
        fragmentTransaction.add(R.id.fl_container, voiceRoomFragment);
        fragmentTransaction.commit();
    }

    @Override
    public void onBackPressed() {
    }
}