package com.trtc.uikit.livekit.example.main;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.voiceroom.TUIVoiceRoomFragment;

public class LiveActivity extends FullScreenActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_live);
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            String roomId = bundle.getString("roomId");
            LiveDefine.RoomBehavior roomBehavior = (LiveDefine.RoomBehavior) bundle.getSerializable("roomBehavior");
            LiveDefine.RoomParams roomParams = (LiveDefine.RoomParams) bundle.getSerializable("roomParams");
            TUIVoiceRoomFragment fragment = new TUIVoiceRoomFragment(roomId, roomBehavior, roomParams);
            fragmentTransaction.add(R.id.fl_container, fragment);
            fragmentTransaction.commit();
        }
    }
}
