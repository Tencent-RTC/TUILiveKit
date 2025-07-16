package com.trtc.uikit.livekit.livestream;

import static com.trtc.uikit.livekit.common.utils.MutableLiveDataUtils.setValue;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;
import android.widget.FrameLayout;

import androidx.annotation.Nullable;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore;
import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerView;
import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerViewDefine;
import com.trtc.uikit.livekit.features.endstatistics.AudienceEndStatisticsView;
import com.trtc.uikit.livekit.livestream.impl.LiveInfoUtils;
import com.trtc.uikit.livekit.livestream.impl.VideoLiveKitImpl;

import java.util.HashMap;
import java.util.Map;

public class VideoLiveAudienceActivity extends FullScreenActivity implements ITUINotification,
        VideoLiveKitImpl.CallingAPIListener, AudienceContainerViewDefine.AudienceContainerViewListener {
    public static final String KEY_EXTENSION_NAME     = "TEBeautyExtension";
    public static final String NOTIFY_START_ACTIVITY  = "onStartActivityNotifyEvent";
    public static final String METHOD_ACTIVITY_RESULT = "onActivityResult";

    private FrameLayout               mLayoutContainer;
    private AudienceContainerView     mAudienceContainerView;
    private AudienceEndStatisticsView mAudienceEndStatisticsView;

    private final Observer<Boolean> mEndStatisticsViewObserver = this::onEndStatisticsView;

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
        setContentView(R.layout.livekit_activity_video_live_audience);

        Bundle liveBundle = getIntent().getExtras();
        if (liveBundle == null) {
            Log.e("VideoLiveAudience", "liveBundle is null");
            return;
        }
        mLayoutContainer = findViewById(R.id.fl_container);
        TUILiveListManager.LiveInfo liveInfo = LiveInfoUtils.convertBundleToLiveInfo(liveBundle);
        mAudienceContainerView = new AudienceContainerView(this);
        mAudienceContainerView.init(this, liveInfo);
        mAudienceContainerView.addListener(this);
        mLayoutContainer.addView(mAudienceContainerView);
        VideoLiveKitImpl.createInstance(getApplicationContext()).addCallingAPIListener(this);
        startForegroundService();
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (TextUtils.equals(key, KEY_EXTENSION_NAME) && TextUtils.equals(subKey, NOTIFY_START_ACTIVITY)) {
            Intent intent = (Intent) param.get("intent");
            if (param.containsKey("requestCode")) {
                int requestCode = (int) param.get("requestCode");
                startActivityForResult(intent, requestCode);
            } else {
                startActivity(intent);
            }
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        VideoLiveKitImpl.createInstance(getApplicationContext()).removeCallingAPIListener(this);
        stopForegroundService();
        mAudienceContainerView.removeListener(this);
        setValue(PictureInPictureStore.sharedInstance().getState().roomId, "");
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        Map<String, Object> param = new HashMap<>();
        param.put("requestCode", requestCode);
        param.put("resultCode", resultCode);
        param.put("data", data);
        TUICore.callService(KEY_EXTENSION_NAME, METHOD_ACTIVITY_RESULT, param);
    }

    @Override
    public void onBackPressed() {
    }

    @Override
    protected void onStop() {
        super.onStop();
        if (!isFinishing()) {
            VideoLiveKitImpl.createInstance(getApplicationContext()).stopPushLocalVideoOnStop();
        }
    }

    @Override
    public void onLeaveLive() {
        finish();
    }

    @Override
    public void onStopLive() {
        finish();
    }

    @Override
    protected void onResume() {
        super.onResume();
        VideoLiveKitImpl.createInstance(getApplicationContext()).startPushLocalVideoOnResume();
    }

    @Override
    public void onPictureInPictureModeChanged(boolean isInPictureInPictureMode) {
        super.onPictureInPictureModeChanged(isInPictureInPictureMode);
        PictureInPictureStore.sharedInstance().getState().audienceIsPictureInPictureMode = isInPictureInPictureMode;
        if (mAudienceContainerView != null) {
            mAudienceContainerView.enablePictureInPictureMode(isInPictureInPictureMode);
        }
        if (!isInPictureInPictureMode && getLifecycle().getCurrentState() == Lifecycle.State.CREATED) {
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER, null);
        }
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        boolean isPortrait = this.getRequestedOrientation() == ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
        if (mAudienceContainerView != null) {
            mAudienceContainerView.setScreenOrientation(isPortrait);
        }
    }

    private void startForegroundService() {
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.start(context, context.getString(context.getApplicationInfo().labelRes),
                context.getString(R.string.common_app_running), 0);
    }

    private void stopForegroundService() {
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.stop(context);
    }

    @Override
    public void onLiveEnded(String roomId, String ownerName, String ownerAvatarUrl) {
        if (PictureInPictureStore.sharedInstance().getState().audienceIsPictureInPictureMode) {
            finish();
            return;
        }
        mAudienceEndStatisticsView = new AudienceEndStatisticsView(this);
        mAudienceEndStatisticsView.init(roomId, ownerName, ownerAvatarUrl);
        mLayoutContainer.removeAllViews();
        mLayoutContainer.addView(mAudienceEndStatisticsView);
        mAudienceEndStatisticsView.getState().exitClick.observeForever(mEndStatisticsViewObserver);
    }

    @Override
    public void onPictureInPictureClick() {
        boolean success = VideoLiveKitImpl.createInstance(getApplicationContext()).enterPictureInPictureMode(this);
        if (success) {
            String roomId = mAudienceContainerView.getRoomId();
            setValue(PictureInPictureStore.sharedInstance().getState().roomId, roomId);
        }
    }

    private void onEndStatisticsView(Boolean isClick) {
        if (!isClick) {
            return;
        }
        mAudienceEndStatisticsView.getState().exitClick.removeObserver(mEndStatisticsViewObserver);
        finish();
    }
}
