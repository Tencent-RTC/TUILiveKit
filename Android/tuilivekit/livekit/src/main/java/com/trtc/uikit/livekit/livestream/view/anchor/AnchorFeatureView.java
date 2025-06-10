package com.trtc.uikit.livekit.livestream.view.anchor;

import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.tuikit.common.util.ScreenUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.Constants;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.ui.RoundFrameLayout;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareView;
import com.trtc.uikit.livekit.features.anchorboardcast.AnchorView;
import com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.module.DashboardManager;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveBattleManagerObserver;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveStreamObserver;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestream.view.anchor.dashboard.AnchorDashboardView;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle.BattleCountdownDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest.AnchorManagerDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost.StandardDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.usermanage.UserManagerDialog;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Map;

public class AnchorFeatureView extends BasicView implements ITUINotification {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("AnchorView");

    private       LiveCoreView                           mLiveCoreView;
    private       RoundFrameLayout                       mLayoutVideoContainer;
    private       FrameLayout                            mLayoutFeatureContainer;
    private AnchorPrepareView   mAnchorPrepareView;
    private AnchorView          mAnchorStreamingView;
    private AnchorDashboardView mAnchorDashboardView;
    private       View                                   mBackView;
    private       StandardDialog                         mProcessConnectionDialog;
    private       StandardDialog                         mProcessBattleDialog;
    private       BattleCountdownDialog                  mBattleCountdownDialog;
    private       AnchorManagerDialog                    mAnchorManagerDialog;
    private       UserManagerDialog                      mUserManagerDialog;
    private       TUILiveRoomAnchorFragment.RoomBehavior mRoomBehavior;
    private       boolean                                mIsExit                 = false;
    private       boolean                                mUseCachedCoreView;
    private       LiveStreamObserver                     mLiveStreamObserver;
    private       LiveBattleManagerObserver              mLiveBattleManagerObserver;
    private final Observer<Boolean>                      mStartLiveClickObserver = this::onStartLiveClick;
    private final Observer<RoomState.LiveStatus>         mLiveStatusObserver     = this::onLiveStatusChange;


    public AnchorFeatureView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorFeatureView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorFeatureView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(LiveStreamManager liveStreamManager, TUILiveRoomAnchorFragment.RoomBehavior behavior) {
        mRoomBehavior = behavior;
        init(liveStreamManager);
    }

    public LiveCoreViewDefine.CoreState getCoreState() {
        return mLiveCoreView.getCoreState();
    }

    public void destroy() {

    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (!FloatWindowManager.getInstance().isWillOpenFloatWindow()) {
            mLiveCoreView.stopCamera();
            mLiveCoreView.stopMicrophone();
        }
        mLiveManager.getUserManager().clearEnterUserInfo();
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_livestream_anchor_feature_view, this, true);
        mLayoutVideoContainer = findViewById(R.id.lsv_video_view_container);
        mLayoutFeatureContainer = findViewById(R.id.fl_feature_container);
        mBackView = findViewById(R.id.iv_back);

        mLiveCoreView = FloatWindowManager.getInstance().getCoreView();
        if (mLiveCoreView == null) {
            mLiveCoreView = new LiveCoreView(ContextProvider.getApplicationContext());
        } else {
            mUseCachedCoreView = true;
            FloatWindowManager.getInstance().setCoreView(null);
        }

        mLayoutVideoContainer.setRadius(ScreenUtil.dip2px(16));
        mLayoutVideoContainer.addView(mLiveCoreView);
    }

    @Override
    protected void refreshView() {
        initBackView();
        initLiveCoreView();
    }

    private void initBackView() {
        mBackView.setOnClickListener(v -> {
            mIsExit = true;
            ((Activity) getContext()).finish();
        });
    }

    private void initLiveCoreView() {
        setComponent();
        if (!mUseCachedCoreView) {
            mLiveStreamObserver = new LiveStreamObserver(mLiveManager);
            mLiveBattleManagerObserver = new LiveBattleManagerObserver(mLiveManager);
            mLiveCoreView.registerConnectionObserver(mLiveStreamObserver);
            mLiveCoreView.registerBattleObserver(mLiveBattleManagerObserver);
        }
    }

    private void setComponent() {
        try {
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "component");
            jsonObject.put("component", Constants.DATA_REPORT_COMPONENT_LIVE_ROOM);
            LiveCoreView.callExperimentalAPI(jsonObject.toString());
        } catch (JSONException e) {
            LOGGER.error("dataReport:" + Log.getStackTraceString(e));
        }
    }

    private void initComponentView() {
        if (mRoomState.liveStatus.getValue() == RoomState.LiveStatus.PREVIEWING) {
            initPreviewStatus();
        } else if (mRoomState.liveStatus.getValue() == RoomState.LiveStatus.PUSHING) {
            initPushingStatus();
        } else if (mRoomState.liveStatus.getValue() == RoomState.LiveStatus.DASHBOARD) {
            initDashboardStatus();
        }
    }

    private void initDashboardStatus() {
        DashboardManager dashboardManager = mLiveManager.getDashboardManager();
        dashboardManager.updateDuration(System.currentTimeMillis() - mRoomState.createTime);
        dashboardManager.updateLikeNumber(1);
        dashboardManager.updateMessageCount(1);

        mLayoutFeatureContainer.removeAllViews();
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT);
        mAnchorDashboardView = new AnchorDashboardView(getContext());
        mAnchorDashboardView.init(mLiveManager);
        mLayoutFeatureContainer.addView(mAnchorDashboardView, params);
    }

    private void initPreviewStatus() {
        mAnchorPrepareView = new AnchorPrepareView(mContext);
        mLayoutFeatureContainer.removeAllViews();
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT);
        mLayoutFeatureContainer.addView(mAnchorPrepareView, params);
        mAnchorPrepareView.init(mLiveManager.getRoomState().roomId, mLiveCoreView);
        mAnchorPrepareView.getState().startLiveClick.observeForever(mStartLiveClickObserver);
    }

    private void initPushingStatus() {
        mLayoutFeatureContainer.removeAllViews();
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT);
        mAnchorStreamingView = new AnchorView(mContext);
        mAnchorStreamingView.init(mLiveManager.getRoomState().roomId, mLiveCoreView,
                AnchorViewDefine.Behavior.CREATE_AND_ENTER_ROOM);
        mLayoutFeatureContainer.addView(mAnchorStreamingView, params);
    }

    @Override
    protected void addObserver() {
        mRoomState.liveStatus.observeForever(mLiveStatusObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.liveStatus.removeObserver(mLiveStatusObserver);
    }

    private void onLiveStatusChange(RoomState.LiveStatus liveStatus) {
        initComponentView();
    }

    private void onStartLiveClick(Boolean isStart) {
        if (isStart) {
            mRoomManager.startStreaming(mAnchorPrepareView.getState().roomName.getValue(),
                    mAnchorPrepareView.getState().coverURL.getValue(), RoomState.LiveStreamPrivacyStatus.PUBLIC);
        }
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {

    }
}
