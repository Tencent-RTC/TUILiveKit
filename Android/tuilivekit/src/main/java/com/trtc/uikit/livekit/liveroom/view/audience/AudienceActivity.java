package com.trtc.uikit.livekit.liveroom.view.audience;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.TUILiveRoomAudienceFragment;

public class AudienceActivity extends AppCompatActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.livekit_activity_audience);

        String roomId = getIntent().getStringExtra("roomId");
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        TUILiveRoomAudienceFragment audienceFragment = new TUILiveRoomAudienceFragment(roomId);
        fragmentTransaction.add(R.id.fl_container, audienceFragment);
        fragmentTransaction.commit();
    }
}