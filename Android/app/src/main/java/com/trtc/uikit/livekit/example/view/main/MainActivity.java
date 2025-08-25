package com.trtc.uikit.livekit.example.view.main;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;

import com.trtc.tuikit.common.util.ActivityLauncher;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.store.AppStore;
import com.trtc.uikit.livekit.livestream.VideoLiveListActivity;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomListActivity;

public class MainActivity extends BaseActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_main);
        initWebsiteLinkView();
        initVideoLiveView();
        initVoiceRoomView();
    }


    private void initWebsiteLinkView() {
        findViewById(R.id.btn_multi_function).setOnClickListener(v -> {
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setData(Uri.parse(AppStore.TRTC_VIDEO_LIVE_DOCUMENT_URL));
            ActivityLauncher.startActivity(MainActivity.this, intent);
        });
    }

    private void initVideoLiveView() {
        findViewById(R.id.video_live).setOnClickListener(v -> {
            Intent intent = new Intent(MainActivity.this, VideoLiveListActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(intent);
        });
    }

    private void initVoiceRoomView() {
        findViewById(R.id.voice_room).setOnClickListener(v -> {
            Intent intent = new Intent(MainActivity.this, VoiceRoomListActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(intent);
        });
    }
}