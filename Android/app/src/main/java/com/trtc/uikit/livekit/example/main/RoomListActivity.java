package com.trtc.uikit.livekit.example.main;


import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.tuikit.common.util.ActivityLauncher;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.settings.SettingsConfig;
import com.trtc.uikit.livekit.view.TUILiveListFragment;

public class RoomListActivity extends FullScreenActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_room_list);

        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        TUILiveListFragment roomListFragment = new TUILiveListFragment();
        fragmentTransaction.add(R.id.fl_container, roomListFragment);
        fragmentTransaction.commit();

        ((Toolbar) findViewById(R.id.toolbar)).setNavigationOnClickListener((view -> finish()));

        findViewById(R.id.btn_trtclivekit_link).setOnClickListener(v -> {
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setData(Uri.parse(SettingsConfig.TRTC_LIVE_ROOM_DOCUMENT_URL));
            ActivityLauncher.startActivity(RoomListActivity.this, intent);
        });
    }
}


