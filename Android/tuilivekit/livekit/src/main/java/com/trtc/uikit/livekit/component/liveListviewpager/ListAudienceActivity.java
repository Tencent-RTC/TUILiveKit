package com.trtc.uikit.livekit.component.liveListviewpager;

import static com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior.JOIN;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveIdentityGenerator;
import com.trtc.uikit.livekit.livestream.view.audience.TUILiveRoomAudienceFragment;
import com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment;

public class ListAudienceActivity extends AppCompatActivity {

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
        setContentView(R.layout.livekit_activity_list_audience);
        initStatusBar();
        Bundle liveBundle = getIntent().getExtras();
        if (liveBundle == null) {
            Log.e("ListAudienceActivity", "liveBundle is null");
            return;
        }
        TUILiveListManager.LiveInfo liveInfo = convertBundleToLiveInfo(liveBundle);
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

    private void initStatusBar() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            Window window = getWindow();
            window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                window.getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
                        | View.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR);
            }
            window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
            window.setStatusBarColor(Color.TRANSPARENT);
        } else {
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
        }
    }

    private static TUILiveListManager.LiveInfo convertBundleToLiveInfo(Bundle liveBundle) {
        TUILiveListManager.LiveInfo liveInfo = new TUILiveListManager.LiveInfo();
        liveInfo.coverUrl = liveBundle.getString("coverUrl");
        liveInfo.backgroundUrl = liveBundle.getString("backgroundUrl");
        liveInfo.categoryList = liveBundle.getIntegerArrayList("categoryList");
        liveInfo.isPublicVisible = liveBundle.getBoolean("isPublicVisible", false);
        liveInfo.activityStatus = liveBundle.getInt("activityStatus", 0);
        liveInfo.viewCount = liveBundle.getInt("viewCount", 0);

        TUIRoomDefine.RoomInfo roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo = roomInfo;

        Bundle roomBundle = liveBundle.getBundle("roomInfo");
        if (roomBundle != null) {
            roomInfo.roomId = roomBundle.getString("roomId");
            roomInfo.ownerId = roomBundle.getString("ownerId");
            roomInfo.ownerName = roomBundle.getString("ownerName");
            roomInfo.ownerAvatarUrl = roomBundle.getString("ownerAvatarUrl");
            roomInfo.roomType = TUIRoomDefine.RoomType.fromInt(roomBundle.getInt("roomType", 0));
            roomInfo.name = roomBundle.getString("name");
            roomInfo.isCameraDisableForAllUser = roomBundle.getBoolean("isCameraDisableForAllUser", false);
            roomInfo.isMicrophoneDisableForAllUser = roomBundle.getBoolean("isMicrophoneDisableForAllUser", false);
            roomInfo.isScreenShareDisableForAllUser = roomBundle.getBoolean("isScreenShareDisableForAllUser", false);
            roomInfo.isMessageDisableForAllUser = roomBundle.getBoolean("isMessageDisableForAllUser", false);
            roomInfo.isSeatEnabled = roomBundle.getBoolean("isSeatEnabled", false);
            roomInfo.seatMode = TUIRoomDefine.SeatMode.fromInt(roomBundle.getInt("seatMode", 0));
            roomInfo.maxSeatCount = roomBundle.getInt("maxSeatCount", 0);
            roomInfo.createTime = roomBundle.getLong("createTime", 0);
            roomInfo.memberCount = roomBundle.getInt("memberCount", 0);
            roomInfo.password = roomBundle.getString("password");
        }
        return liveInfo;
    }
}