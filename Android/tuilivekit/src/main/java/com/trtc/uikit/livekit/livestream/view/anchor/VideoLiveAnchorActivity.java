package com.trtc.uikit.livekit.livestream.view.anchor;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.view.anchor.TUILiveRoomAnchorFragment;

public class VideoLiveAnchorActivity extends FullScreenActivity {

    public static final String INTENT_KEY_ROOM_ID     = "intent_key_room_id";

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        String roomId = getIntent().getStringExtra(INTENT_KEY_ROOM_ID);
        setContentView(R.layout.livekit_activity_video_live_anchor);
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        TUILiveRoomAnchorFragment anchorFragment = new TUILiveRoomAnchorFragment(roomId);
        fragmentTransaction.add(R.id.fl_container, anchorFragment);
        fragmentTransaction.commit();
    }
}
