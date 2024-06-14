package com.trtc.uikit.livekit.example.main;

import com.trtc.uikit.livekit.view.LiveIdentityGenerator.RoomType;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.view.LiveIdentityGenerator;
import com.trtc.uikit.livekit.view.TUILiveAnchorFragment;
import com.trtc.uikit.livekit.example.R;

public class AnchorActivity extends FullScreenActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_anchor);

        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        LiveIdentityGenerator identityGenerator = LiveIdentityGenerator.getInstance();
        String liveRoomId = identityGenerator.generateId(TUILogin.getUserId(), RoomType.LIVE);
        String voiceRoomId = identityGenerator.generateId(TUILogin.getUserId(), RoomType.VOICE);
        TUILiveAnchorFragment anchorFragment = new TUILiveAnchorFragment(liveRoomId, voiceRoomId);
        fragmentTransaction.add(R.id.fl_container, anchorFragment);
        fragmentTransaction.commit();
    }
}
