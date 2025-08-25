package com.trtc.uikit.livekit.livestream;

import static com.trtc.uikit.livekit.common.utils.MutableLiveDataUtils.setValue;
import static com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine.RoomBehavior.CREATE_ROOM;
import static com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine.RoomBehavior.ENTER_ROOM;

import android.Manifest;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.os.Build;
import android.os.Bundle;
import android.provider.MediaStore;
import android.text.TextUtils;
import android.view.View;
import android.view.WindowManager;
import android.widget.FrameLayout;

import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.lifecycle.Lifecycle;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore;
import com.trtc.uikit.livekit.features.anchorboardcast.AnchorView;
import com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareView;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine;
import com.trtc.uikit.livekit.features.endstatistics.AnchorEndStatisticsView;
import com.trtc.uikit.livekit.features.endstatistics.EndStatisticsDefine.AnchorEndStatisticsInfo;
import com.trtc.uikit.livekit.livestream.impl.LiveInfoUtils;
import com.trtc.uikit.livekit.livestream.impl.VideoLiveKitImpl;

import java.util.HashMap;
import java.util.Map;

public class VideoLiveAnchorActivity extends FullScreenActivity implements VideoLiveKitImpl.CallingAPIListener,
        AnchorPrepareViewDefine.AnchorPrepareViewListener, AnchorViewDefine.AnchorViewListener, ITUINotification {

    public static final  String INTENT_KEY_ROOM_ID       = "intent_key_room_id";
    public static final  String INTENT_KEY_NEED_CREATE   = "intent_key_need_create";
    public static final  String KEY_EXTENSION_NAME       = "TEBeautyExtension";
    public static final  String NOTIFY_START_ACTIVITY    = "onStartActivityNotifyEvent";
    public static final  String METHOD_ACTIVITY_RESULT   = "onActivityResult";
    public static final  String PICK_CONTENT_ALL         = "image/*|video/*";
    private static final int    REQUEST_CODE_PERMISSIONS = 1001;

    private       int                         mStartActivityRequestCode = 0;
    private       FrameLayout                 mLayoutContainer;
    private       AnchorPrepareView           mAnchorPrepareView;
    private       AnchorView                  mAnchorView;
    private       AnchorEndStatisticsView     mAnchorEndStatisticsView;
    private       Boolean                     mNeedCreateRoom           = true;
    private       String                      mRoomId                   = "";
    private       TUILiveListManager.LiveInfo mLiveInfo                 = new TUILiveListManager.LiveInfo();
    private final AnchorEndStatisticsInfo     mAnchorEndStatisticsInfo  = new AnchorEndStatisticsInfo();

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
        setContentView(R.layout.livekit_activity_video_live_anchor);
        mRoomId = getIntent().getStringExtra(INTENT_KEY_ROOM_ID);
        mLiveInfo.roomId = mRoomId;
        mNeedCreateRoom = getIntent().getBooleanExtra(INTENT_KEY_NEED_CREATE, true);
        Bundle liveBundle = getIntent().getExtras();
        if (liveBundle != null && !liveBundle.containsKey(INTENT_KEY_ROOM_ID)) {
            mLiveInfo = LiveInfoUtils.convertBundleToLiveInfo(liveBundle);
        }
        mLayoutContainer = findViewById(R.id.fl_container);
        if (mNeedCreateRoom) {
            addPrepareView();
        } else {
            addAnchorView();
        }
        TUICore.registerEvent(KEY_EXTENSION_NAME, NOTIFY_START_ACTIVITY, this);
        VideoLiveKitImpl.createInstance(getApplicationContext()).addCallingAPIListener(this);
    }

    @Override
    protected void onResume() {
        super.onResume();
        VideoLiveKitImpl.createInstance(getApplicationContext()).startPushLocalVideoOnResume();
    }

    @Override
    protected void onStop() {
        super.onStop();
        if (!isFinishing()) {
            VideoLiveKitImpl.createInstance(getApplicationContext()).stopPushLocalVideoOnStop();
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        TUICore.unRegisterEvent(this);
        if (mAnchorPrepareView != null) {
            mAnchorPrepareView.removeAnchorPrepareViewListener(this);
        }

        if (mAnchorView != null) {
            mAnchorView.removeAnchorViewListener(this);
        }

        setValue(PictureInPictureStore.sharedInstance().getState().roomId, "");
    }

    @Override
    public void onBackPressed() {
    }

    @Override
    public void onPictureInPictureModeChanged(boolean isInPictureInPictureMode) {
        super.onPictureInPictureModeChanged(isInPictureInPictureMode);
        PictureInPictureStore.sharedInstance().getState().anchorIsPictureInPictureMode = isInPictureInPictureMode;
        if (mAnchorView != null) {
            mAnchorView.enablePipMode(isInPictureInPictureMode);
        }
        if (!isInPictureInPictureMode && getLifecycle().getCurrentState() == Lifecycle.State.CREATED) {
            finishAndRemoveTask();
            mAnchorView.unInit();
        }
    }

    private void addPrepareView() {
        mAnchorPrepareView = new AnchorPrepareView(this);
        mAnchorPrepareView.init(mLiveInfo.roomId, null);
        FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT,
                FrameLayout.LayoutParams.MATCH_PARENT);
        mLayoutContainer.addView(mAnchorPrepareView, layoutParams);

        mAnchorPrepareView.addAnchorPrepareViewListener(this);
    }

    private void addAnchorView() {
        mAnchorView = new AnchorView(this);
        Map<String, Object> params = new HashMap<>();
        if (mAnchorPrepareView != null) {
            params.put("coHostTemplateId", mAnchorPrepareView.getState().coHostTemplateId.getValue());
            mAnchorView.init(mLiveInfo, mAnchorPrepareView.getCoreView(), mNeedCreateRoom ? CREATE_ROOM : ENTER_ROOM,
                    params);
        } else {
            mAnchorView.init(mLiveInfo, null, mNeedCreateRoom ? CREATE_ROOM : ENTER_ROOM, params);
        }

        FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT,
                FrameLayout.LayoutParams.MATCH_PARENT);
        mLayoutContainer.addView(mAnchorView, layoutParams);

        mAnchorView.addAnchorViewListener(this);
    }

    private void removeAnchorPrepareView() {
        mLayoutContainer.removeAllViews();
    }

    private void initLiveInfo() {
        mLiveInfo = new TUILiveListManager.LiveInfo();
        mLiveInfo.roomId = mRoomId;
        if (mAnchorPrepareView != null && mAnchorPrepareView.getState() != null) {
            mLiveInfo.name = mAnchorPrepareView.getState().roomName.getValue();
            mLiveInfo.isPublicVisible =
                    mAnchorPrepareView.getState().liveMode.getValue() == AnchorPrepareViewDefine.LiveStreamPrivacyStatus.PUBLIC;
            mLiveInfo.coverUrl = mAnchorPrepareView.getState().coverURL.getValue();
            mLiveInfo.backgroundUrl = mAnchorPrepareView.getState().coverURL.getValue();
            mLiveInfo.seatLayoutTemplateId = mAnchorPrepareView.getState().coGuestTemplateId.getValue();
        }
    }

    private void initEndStatisticsInfo(AnchorViewDefine.AnchorState state) {
        if (state != null) {
            mAnchorEndStatisticsInfo.roomId = mLiveInfo.roomId;
            mAnchorEndStatisticsInfo.liveDurationMS = state.duration;
            mAnchorEndStatisticsInfo.maxViewersCount = state.viewCount;
            mAnchorEndStatisticsInfo.messageCount = state.messageCount;
            mAnchorEndStatisticsInfo.giftIncome = state.giftIncome;
            mAnchorEndStatisticsInfo.giftSenderCount = state.giftSenderCount;
            mAnchorEndStatisticsInfo.likeCount = state.likeCount;
        }
    }

    private void removeAnchorView() {
        mLayoutContainer.removeAllViews();
    }

    private void addAnchorEndStatisticsView() {
        mAnchorEndStatisticsView = new AnchorEndStatisticsView(this);
        mAnchorEndStatisticsView.init(mAnchorEndStatisticsInfo);

        FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT,
                FrameLayout.LayoutParams.MATCH_PARENT);
        mLayoutContainer.addView(mAnchorEndStatisticsView, layoutParams);
        mAnchorEndStatisticsView.setListener(this::finish);
    }

    @Override
    public void onClickStartButton() {
        mAnchorPrepareView.removeAnchorPrepareViewListener(this);
        initLiveInfo();
        removeAnchorPrepareView();
        addAnchorView();
    }

    @Override
    public void onClickBackButton() {
        finish();
    }

    @Override
    public void onEndLiving(AnchorViewDefine.AnchorState state) {
        mAnchorView.removeAnchorViewListener(this);

        initEndStatisticsInfo(state);
        removeAnchorView();
        addAnchorEndStatisticsView();
    }

    @Override
    public void onClickFloatWindow() {
        boolean success = VideoLiveKitImpl.createInstance(getApplicationContext()).enterPictureInPictureMode(this);
        if (success) {
            setValue(PictureInPictureStore.sharedInstance().getState().roomId, mLiveInfo.roomId);
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
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (TextUtils.equals(key, KEY_EXTENSION_NAME) && TextUtils.equals(subKey, NOTIFY_START_ACTIVITY)) {
            if (param.containsKey("requestCode")) {
                mStartActivityRequestCode = (int) param.get("requestCode");
                if (ContextCompat.checkSelfPermission(this, Manifest.permission.READ_EXTERNAL_STORAGE)
                        == PackageManager.PERMISSION_GRANTED) {
                    Intent intentToPickPic = new Intent(Intent.ACTION_PICK,
                            MediaStore.Video.Media.EXTERNAL_CONTENT_URI);
                    intentToPickPic.setDataAndType(MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                            PICK_CONTENT_ALL);
                    startActivityForResult(intentToPickPic, mStartActivityRequestCode);
                } else {
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                        ActivityCompat.requestPermissions(
                                this,
                                new String[]{Manifest.permission.READ_EXTERNAL_STORAGE},
                                REQUEST_CODE_PERMISSIONS
                        );
                    }
                }
            }
        }
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
    public void onRequestPermissionsResult(int requestCode,
                                           String[] permissions,
                                           int[] grantResults) {
        if (requestCode == REQUEST_CODE_PERMISSIONS) {
            Intent intentToPickPic = new Intent(Intent.ACTION_PICK, MediaStore.Video.Media.EXTERNAL_CONTENT_URI);
            intentToPickPic.setDataAndType(MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                    PICK_CONTENT_ALL);
            startActivityForResult(intentToPickPic, mStartActivityRequestCode);
        } else {
            super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        }
    }
}
