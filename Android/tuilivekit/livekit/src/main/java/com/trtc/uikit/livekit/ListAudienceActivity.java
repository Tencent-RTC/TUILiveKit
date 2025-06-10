package com.trtc.uikit.livekit;

import static com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior.JOIN;

import android.content.Context;
import android.content.res.Configuration;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.common.LiveIdentityGenerator;
import com.trtc.uikit.livekit.livestream.view.audience.TUILiveRoomAudienceFragment;
import com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment;

public class ListAudienceActivity extends FullScreenActivity {

    @Override
    protected void attachBaseContext(Context context) {
        super.attachBaseContext(context);
        if (context != null) {
            Configuration configuration = context.getResources().getConfiguration();
            configuration.fontScale = 1;
            applyOverrideConfiguration(configuration);
        }
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(null);
        getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        getWindow().getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN);
        setContentView(R.layout.livelist_activity_list_audience);
        Bundle liveBundle = getIntent().getExtras();
        if (liveBundle == null) {
            Log.e("ListAudienceActivity", "liveBundle is null");
            return;
        }
        TUILiveListManager.LiveInfo liveInfo = LiveInfoUtils.convertBundleToLiveInfo(liveBundle);
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        Fragment fragment;
        if (LiveIdentityGenerator.getInstance().getIDType(liveInfo.roomInfo.roomId) == LiveIdentityGenerator.RoomType.VOICE) {
            fragment = new TUIVoiceRoomFragment(liveInfo.roomInfo.roomId, JOIN, null);
        } else {
            fragment = new TUILiveRoomAudienceFragment(liveInfo);
        }
        fragmentTransaction.add(R.id.fl_container, fragment);
        fragmentTransaction.commit();
    }

    @Override
    public void onBackPressed() {
    }
}