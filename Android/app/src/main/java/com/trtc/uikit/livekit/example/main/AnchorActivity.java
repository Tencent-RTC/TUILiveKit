package com.trtc.uikit.livekit.example.main;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.TUILivePreviewFragment;
import com.trtc.uikit.livekit.example.R;

public class AnchorActivity extends FullScreenActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_anchor);

        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        String liveRoomID = TUILogin.getUserId();
        String voiceRoomId = TUILogin.getUserId();
        TUILivePreviewFragment anchorFragment = new TUILivePreviewFragment(liveRoomID, voiceRoomId);
        fragmentTransaction.add(R.id.fl_container, anchorFragment);
        fragmentTransaction.commit();
    }
}
