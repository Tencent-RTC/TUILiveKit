package com.trtc.uikit.livekit.view.liveroom;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.R;

public class VideoLiveAudienceActivity extends FullScreenActivity {

    public static final String INTENT_KEY_ROOM_ID = "intent_key_room_id";

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        String roomId = getIntent().getStringExtra(INTENT_KEY_ROOM_ID);
        setContentView(R.layout.livekit_activity_video_live_audience);
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        TUILiveRoomAudienceFragment audienceFragment = new TUILiveRoomAudienceFragment(roomId);
        fragmentTransaction.add(R.id.fl_container, audienceFragment);
        fragmentTransaction.commit();
    }
}