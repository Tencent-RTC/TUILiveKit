package com.trtc.uikit.livekit.example.view.scene;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;

import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.tuikit.common.util.ActivityLauncher;
import com.trtc.uikit.livekit.livestream.VideoLiveKit;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.store.AppStore;
import com.trtc.uikit.livekit.LiveIdentityGenerator;
import com.trtc.uikit.livekit.component.roomlist.TUILiveListFragment;

public class VideoLiveActivity extends FullScreenActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_video_live);

        initWebsiteLinkView();
        initLiveListFragment();
        initStartLiveView();
        initBackButton();
    }

    private void initWebsiteLinkView() {
        findViewById(R.id.btn_multi_function).setOnClickListener(v -> {
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setData(Uri.parse(AppStore.TRTC_VIDEO_LIVE_DOCUMENT_URL));
            ActivityLauncher.startActivity(VideoLiveActivity.this, intent);
        });
    }

    private void initStartLiveView() {
        findViewById(R.id.iv_start).setOnClickListener(view -> {
            LiveIdentityGenerator identityGenerator = LiveIdentityGenerator.getInstance();
            String roomId = identityGenerator.generateId(TUILogin.getUserId(), LiveIdentityGenerator.RoomType.LIVE);
            VideoLiveKit.createInstance(getApplicationContext()).startLive(roomId);
        });
    }

    private void initBackButton() {
        findViewById(R.id.iv_back).setOnClickListener(v -> onBackPressed());
    }

    private void initLiveListFragment() {
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction transaction = fragmentManager.beginTransaction();
        transaction.replace(R.id.fl_live_list, new TUILiveListFragment());
        transaction.commit();
    }
}