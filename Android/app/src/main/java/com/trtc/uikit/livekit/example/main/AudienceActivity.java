package com.trtc.uikit.livekit.example.main;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.liveroom.TUILiveRoomAudienceFragment;

public class AudienceActivity extends AppCompatActivity {
    public String mRoomId = "";

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_audience);
        mRoomId = getIntent().getStringExtra("roomId");

        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        TUILiveRoomAudienceFragment audienceFragment = new TUILiveRoomAudienceFragment(mRoomId);
        fragmentTransaction.add(R.id.fl_container, audienceFragment);
        fragmentTransaction.commit();
    }
}
