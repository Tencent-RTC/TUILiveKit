package com.trtc.uikit.livekit.example.main;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.view.TUILiveAudienceFragment;

public class AudienceActivity extends FullScreenActivity {
    public String mRoomId = "";

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_audience);
        mRoomId = getIntent().getStringExtra("roomId");

        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        TUILiveAudienceFragment audienceFragment = new TUILiveAudienceFragment(mRoomId);
        fragmentTransaction.add(R.id.fl_container, audienceFragment);
        fragmentTransaction.commit();
    }
}
