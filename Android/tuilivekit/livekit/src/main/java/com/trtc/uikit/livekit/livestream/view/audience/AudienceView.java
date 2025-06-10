package com.trtc.uikit.livekit.livestream.view.audience;

import static android.app.Activity.RESULT_OK;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_ROOM_ID;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_USER_INFO;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW;
import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.LINKING;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.DASHBOARD;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.NONE;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.PLAYING;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewConfiguration;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomInfoCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.imageloader.ImageOptions;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.component.barrage.BarrageInputView;
import com.trtc.uikit.component.barrage.BarrageStreamView;
import com.trtc.uikit.component.barrage.store.BarrageStore;
import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.Constants;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyStore;
import com.trtc.uikit.livekit.component.beauty.tebeauty.store.TEBeautyStore;
import com.trtc.uikit.livekit.component.dashboard.StreamDashboardDialog;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.component.gift.GiftPlayView;
import com.trtc.uikit.livekit.component.gift.LikeButton;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.giftaccess.GiftButton;
import com.trtc.uikit.livekit.component.giftaccess.service.GiftCacheService;
import com.trtc.uikit.livekit.component.giftaccess.store.GiftStore;
import com.trtc.uikit.livekit.component.giftaccess.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.giftaccess.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.component.roominfo.LiveInfoView;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveBattleManagerObserver;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveStreamObserver;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionUser;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestream.view.VideoLiveKitImpl;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest.AnchorManagerDialog;
import com.trtc.uikit.livekit.livestream.view.audience.dashboard.AudienceDashboardView;
import com.trtc.uikit.livekit.livestream.view.audience.playing.AudiencePlayingRootView;
import com.trtc.uikit.livekit.livestream.view.audience.playing.EndLiveStreamDialog;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.CoGuestRequestFloatView;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.dialog.CancelRequestDialog;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.dialog.StopCoGuestDialog;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.dialog.TypeSelectDialog;
import com.trtc.uikit.livekit.livestream.view.audience.playing.userinfo.UserInfoDialog;
import com.trtc.uikit.livekit.livestream.view.widgets.battle.BattleInfoView;
import com.trtc.uikit.livekit.livestream.view.widgets.battle.BattleMemberInfoView;
import com.trtc.uikit.livekit.livestream.view.widgets.coguest.CoGuestWidgetsView;
import com.trtc.uikit.livekit.livestream.view.widgets.cohost.CoHostWidgetsView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import org.json.JSONException;
import org.json.JSONObject;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class AudienceView extends BasicView implements ITUINotification {
    private static final LiveKitLogger LOGGER            = LiveKitLogger.getLiveStreamLogger("AudienceView");
    private static final String        DEFAULT_COVER_URL =
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png";

    private       LiveCoreView                         mLiveCoreView;
    private       FrameLayout                          mLayoutPlaying;
    private       FrameLayout                          mLayoutLiveCoreView;
    private       ImageView                            mIvVideoViewBackground;
    private       AudienceDashboardView                mAudienceDashboardView;
    private       ImageView                            mImageDashboard;
    private       GiftButton                           mButtonGift;
    private       LikeButton                           mButtonLike;
    private       ImageView                            mImageCoGuest;
    private       BarrageInputView                     mBarrageInputView;
    private       LiveInfoView                         mRoomInfoView;
    private       GiftPlayView                         mGiftPlayView;
    private       AudienceListView                     mAudienceListView;
    private       BarrageStreamView                    mBarrageStreamView;
    private       ImageView                            mImageFloatWindow;
    private       ImageView                            mImageStandardExit;
    private       ImageView                            mImageCompactExit;
    private       CoGuestRequestFloatView              mWaitingCoGuestPassView;
    private       AudiencePlayingRootView              mAudiencePlayingRootView;
    private       UserInfoDialog                       mUserInfoDialog;
    private       AnchorManagerDialog                  mAnchorManagerDialog;
    private       ViewObserver                         mViewObserver;
    private       boolean                              mIsLoading;
    private       LiveStreamObserver                   mLiveStreamObserver;
    private       LiveBattleManagerObserver            mLiveBattleManagerObserver;
    private       VideoViewAdapterImpl                 mVideoViewAdapterImpl;
    private final Observer<RoomState.LiveStatus>       mLiveStatusChangeObserver =
            this::onLiveStatusChange;
    private final Observer<CoGuestState.CoGuestStatus> mLinkStatusObserver       =
            this::onLinkStatusChange;
    private final Observer<UserState.UserInfo>         mEnterUserObserver        =
            this::onEnterUserChange;
    private final Observer<List<ConnectionUser>>       mConnectedObserver        =
            this::onConnectedUserChange;

    private       float   mTouchX;
    private       float   mTouchY;
    private final int     mTouchSlop;
    private       boolean mIsSwiping;

    public AudienceView(@NonNull Context context) {
        this(context, null);
    }

    public AudienceView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mTouchSlop = ViewConfiguration.get(getContext()).getScaledTouchSlop();
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_livestream_audience_view, this, true);
        mLayoutPlaying = findViewById(R.id.fl_playing);
        mAudienceDashboardView = findViewById(R.id.audience_dashboard_view);
        mImageCoGuest = findViewById(R.id.iv_co_guest);
        mButtonGift = findViewById(R.id.btn_gift);
        mButtonLike = findViewById(R.id.btn_like);
        mBarrageInputView = findViewById(R.id.barrage_input_view);
        mImageDashboard = findViewById(R.id.iv_dashboard);
        mRoomInfoView = findViewById(R.id.room_info_view);
        mAudienceListView = findViewById(R.id.audience_list_view);
        mBarrageStreamView = findViewById(R.id.barrage_stream_view);
        mWaitingCoGuestPassView = findViewById(R.id.btn_waiting_pass);
        mGiftPlayView = findViewById(R.id.gift_play_view);
        mImageFloatWindow = findViewById(R.id.iv_float_window);
        mImageStandardExit = findViewById(R.id.iv_standard_exit_room);
        mImageCompactExit = findViewById(R.id.iv_compact_exit_room);
        mIvVideoViewBackground = findViewById(R.id.video_view_background);
        mLayoutLiveCoreView = findViewById(R.id.live_core_view);
        mAudiencePlayingRootView = findViewById(R.id.fl_playing_root);
    }

    @Override
    public boolean dispatchTouchEvent(MotionEvent event) {
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mTouchX = event.getX();
                mTouchY = event.getY();
                mIsSwiping = false;
                break;
            case MotionEvent.ACTION_MOVE:
                float deltaX = event.getX() - mTouchX;
                float deltaY = event.getY() - mTouchY;
                if (Math.abs(deltaX) > mTouchSlop && Math.abs(deltaX) > Math.abs(deltaY)) {
                    mIsSwiping = true;
                }
                break;
        }

        if (mIsSwiping) {
            return mAudiencePlayingRootView.dispatchTouchEvent(event);
        } else {
            return super.dispatchTouchEvent(event);
        }
    }

    @Override
    protected void refreshView() {
        initComponentView();
    }

    public void startPreviewLiveStream() {
        if (mLiveCoreView != null) {
            mLiveCoreView.startPreviewLiveStream(mRoomState.roomId, true, null);
        }
    }

    public void stopPreviewLiveStream() {
        if (mLiveCoreView == null) {
            return;
        }
        FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
        if (floatWindowManager.isShowingFloatWindow() && mLiveCoreView == floatWindowManager.getCoreView()) {
            LOGGER.info("AudienceView" + " float window is show");
            return;
        }
        mLiveCoreView.stopPreviewLiveStream(mRoomState.roomId);
    }

    public void joinRoom() {
        if (mLiveStreamObserver == null) {
            mLiveStreamObserver = new LiveStreamObserver(mLiveManager);
            mLiveCoreView.registerConnectionObserver(mLiveStreamObserver);
        }

        if (mLiveBattleManagerObserver == null) {
            mLiveBattleManagerObserver = new LiveBattleManagerObserver(mLiveManager);
            mLiveCoreView.registerBattleObserver(mLiveBattleManagerObserver);
        }
        if (mRoomState.liveStatus.getValue() == PLAYING) {
            return;
        }
        mLiveManager.addObserver();
        mLayoutPlaying.setVisibility(GONE);
        onViewLoading();
        mLiveManager.getMediaManager().setCustomVideoProcess();
        mLiveCoreView.joinLiveStream(mRoomState.roomId, new GetRoomInfoCallback() {
            @Override
            public void onSuccess(RoomInfo roomInfo) {
                Activity activity = (Activity) mContext;
                if (activity.isFinishing() || activity.isDestroyed()) {
                    LOGGER.warn("activity is exit, leaveLiveStream");
                    mLiveCoreView.leaveLiveStream(null);
                    mLiveCoreView.unregisterConnectionObserver(mLiveStreamObserver);
                    mLiveCoreView.unregisterBattleObserver(mLiveBattleManagerObserver);
                    return;
                }
                mRoomManager.updateRoomState(roomInfo);
                mRoomManager.getLiveInfo(roomInfo.roomId);
                mRoomState.liveStatus.setValue(PLAYING);
                onViewFinished();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                onViewFinished();
                ErrorLocalized.onError(error);
                finishActivity();
            }
        });
    }

    public void leaveRoom() {
        String roomId = mLiveManager.getRoomState().roomId;
        stopPreviewLiveStream();
        mLiveManager.removeObserver();
        mLiveCoreView.unregisterConnectionObserver(mLiveStreamObserver);
        mLiveStreamObserver = null;
        mLiveCoreView.unregisterBattleObserver(mLiveBattleManagerObserver);
        mLiveBattleManagerObserver = null;
        mLiveCoreView.leaveLiveStream(null);
        mLiveManager.getState().reset();
        BarrageStore.sharedInstance().unInit(roomId);
        com.trtc.uikit.livekit.component.gift.store.GiftStore.sharedInstance().unInit(roomId);
        BasicBeautyStore.getInstance().unInit();
        TEBeautyStore.getInstance().unInit();
    }

    public void setViewObserver(ViewObserver observer) {
        mViewObserver = observer;
    }

    private void setLayoutBackground(String imageUrl) {
        ImageOptions.Builder builder = new ImageOptions.Builder();
        builder.setBlurEffect(80f);
        if (TextUtils.isEmpty(imageUrl)) {
            ImageLoader.load(getContext(), mIvVideoViewBackground, DEFAULT_COVER_URL, builder.build());
        } else {
            ImageLoader.load(getContext(), mIvVideoViewBackground, imageUrl, builder.build());
        }
    }

    private void onViewLoading() {
        mIsLoading = true;
        if (mViewObserver != null) {
            mViewObserver.onLoading();
        }
    }

    private void onViewFinished() {
        mIsLoading = false;
        if (mViewObserver != null) {
            mViewObserver.onFinished();
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

    private void showCoGuestManageDialog(UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        if (mLiveCoreView.getCoreState().coGuestState.connectedUserList.getValue().size() <= 1) {
            return;
        }
        if (userInfo.userId.equals(mUserState.selfInfo.userId)) {
            showAnchorManagerDialog(userInfo);
        } else {
            showUserInfoDialog(userInfo);
        }
    }

    private void showAnchorManagerDialog(UserInfo userInfo) {
        if (mAnchorManagerDialog == null) {
            mAnchorManagerDialog = new AnchorManagerDialog(mContext, mLiveManager, mLiveCoreView);
        }
        mAnchorManagerDialog.init(userInfo);
        mAnchorManagerDialog.show();
    }

    private void showUserInfoDialog(UserInfo userInfo) {
        if (mUserInfoDialog == null) {
            mUserInfoDialog = new UserInfoDialog(mContext, mLiveManager);
        }
        mUserInfoDialog.init(userInfo);
        mUserInfoDialog.show();
    }

    private void setVideoViewAdapter() {
        if (mVideoViewAdapterImpl == null) {
            mVideoViewAdapterImpl = new VideoViewAdapterImpl(getContext());
        }
        mLiveCoreView.setVideoViewAdapter(mVideoViewAdapterImpl);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mLiveManager.getUserManager().clearEnterUserInfo();
        mLiveCoreView.setVideoViewAdapter(null);
    }

    private void initComponentView() {
        if (mRoomState.liveStatus.getValue() == PLAYING) {
            updatePlayingStatus();
        } else if (mRoomState.liveStatus.getValue() == DASHBOARD) {
            updateDashboardStatus();
        }
    }

    private void updatePlayingStatus() {
        mAudienceDashboardView.setVisibility(GONE);
        mLayoutPlaying.setVisibility(VISIBLE);
        if (mCoGuestState.coGuestStatus.getValue() == LINKING) {
            stopCoGuest();
        } else {
            initCoGuestIcon();
        }
        initRoomInfoView();
        initAudienceListView();
        initFloatWindowView();
        initExitRoomView();
        initBarrageStreamView();
        initBarrageInputView();
        initDashboardIcon();
        initGiftView();
        initLikeView();
        initGiftPlayView();
        initWaitingCoGuestPassView();
    }


    private void updateDashboardStatus() {
        mLayoutPlaying.setVisibility(GONE);
        mAudienceDashboardView.setVisibility(VISIBLE);

        initAudienceDashboardView();
    }

    private void initAudienceListView() {
        mAudienceListView.init(mRoomState.roomInfo);
    }

    private void initExitRoomView() {
        mImageStandardExit.setOnClickListener(view -> onExitButtonClick());
        mImageCompactExit.setOnClickListener(view -> onExitButtonClick());
    }

    private void initFloatWindowView() {
        mImageFloatWindow.setOnClickListener(v -> {
            FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
            if (floatWindowManager.hasPermission()) {
                floatWindowManager.setCoreView(mLiveCoreView);
                floatWindowManager.setLiveStreamManager(mLiveManager);
                floatWindowManager.showFloatWindow();
                mLiveCoreView.removeView(this);
                finishActivity();
            } else {
                floatWindowManager.requestPermission();
            }
        });
    }

    private void initRoomInfoView() {
        boolean enableFollow = VideoLiveKitImpl.createInstance(mContext).isEnableFollowFeature();
        mRoomInfoView.init(mRoomState.roomInfo, enableFollow);
    }

    public void init(TUILiveListManager.LiveInfo liveInfo) {
        LiveStreamManager liveStreamManager = FloatWindowManager.getInstance().getLiveStreamManager();
        mLiveCoreView = FloatWindowManager.getInstance().getCoreView();
        if (liveStreamManager != null && TextUtils.equals(liveStreamManager.getRoomState().roomId,
                liveInfo.roomInfo.roomId)) {
            FloatWindowManager.getInstance().setLiveStreamManager(null);
            FloatWindowManager.getInstance().setCoreView(null);
            if (mLiveCoreView != null) {
                ViewParent parent = mLiveCoreView.getParent();
                if (parent instanceof ViewGroup) {
                    ((ViewGroup) parent).removeView(mLiveCoreView);
                }
            }
        } else {
            mLiveCoreView = new LiveCoreView(ContextProvider.getApplicationContext());
            liveStreamManager = new LiveStreamManager();
            liveStreamManager.setRoomId(liveInfo.roomInfo.roomId);
            liveStreamManager.getRoomManager().updateLiveInfo(liveInfo);
            liveStreamManager.getMediaManager().setCustomVideoProcess();
            liveStreamManager.setCoreStateProvider(mLiveCoreView::getCoreState);
        }
        mLayoutLiveCoreView.addView(mLiveCoreView);
        init(liveStreamManager);
        setComponent();
        setVideoViewAdapter();
        setLayoutBackground(liveStreamManager.getRoomState().coverURL.getValue());
    }

    private void initBarrageStreamView() {
        mBarrageStreamView.init(mRoomState.roomId, mRoomState.ownerInfo.userId);
        mBarrageStreamView.setItemTypeDelegate(new BarrageViewTypeDelegate());
        mBarrageStreamView.setItemAdapter(GIFT_VIEW_TYPE_1, new GiftBarrageAdapter(mContext));
        mBarrageStreamView.setOnMessageClickListener(userInfo -> {
            if (TextUtils.isEmpty(userInfo.userId)) {
                return;
            }
            if (userInfo.userId.equals(mUserState.selfInfo.userId)) {
                return;
            }

            if (mUserInfoDialog == null) {
                mUserInfoDialog = new UserInfoDialog(mContext, mLiveManager);
            }
            mUserInfoDialog.init(userInfo);
            mUserInfoDialog.show();
        });
    }

    private void initBarrageInputView() {
        mBarrageInputView.init(mRoomState.roomId);
    }

    private void initGiftView() {
        mButtonGift.init(mRoomState.roomId, mRoomState.ownerInfo.userId, mRoomState.ownerInfo.name.getValue(),
                mRoomState.ownerInfo.avatarUrl.getValue());
    }

    private void initLikeView() {
        mButtonLike.init(mRoomState.roomId);
    }

    private void initGiftPlayView() {
        mGiftPlayView.init(mRoomState.roomId);
        GiftCacheService giftCacheService = GiftStore.getInstance().mGiftCacheService;
        mGiftPlayView.setListener(new GiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(Gift gift, int giftCount, GiftUser sender, GiftUser receiver) {
                mLiveManager.getDashboardManager().updateGiftIncome(
                        gift.price * giftCount + mDashboardState.giftIncome);
                mLiveManager.getDashboardManager().insertGiftPeople(sender.userId);
                if (mBarrageStreamView == null) {
                    return;
                }
                Barrage barrage = new Barrage();
                barrage.content = "gift";
                barrage.user.userId = sender.userId;
                barrage.user.userName = TextUtils.isEmpty(sender.userName) ? sender.userId : sender.userName;
                barrage.user.avatarUrl = sender.avatarUrl;
                barrage.extInfo.put(GIFT_VIEW_TYPE, GIFT_VIEW_TYPE_1);
                barrage.extInfo.put(GIFT_NAME, gift.giftName);
                barrage.extInfo.put(GIFT_COUNT, giftCount);
                barrage.extInfo.put(GIFT_ICON_URL, gift.imageUrl);
                barrage.extInfo.put(GIFT_RECEIVER_USERNAME,
                        TextUtils.isEmpty(receiver.userName) ? receiver.userId : receiver.userName);
                mBarrageStreamView.insertBarrages(barrage);
            }

            @Override
            public void onPlayGiftAnimation(GiftPlayView view, Gift gift) {
                giftCacheService.request(gift.animationUrl, (error, result) -> {
                    if (error == 0) {
                        view.playGiftAnimation(result);
                    }
                });
            }
        });
    }

    private void initCoGuestIcon() {
        mImageCoGuest.setImageResource(R.drawable.livekit_function_link_default);
        mImageCoGuest.setOnClickListener(view -> {
            TypeSelectDialog typeSelectDialog = new TypeSelectDialog(mContext, mLiveManager, mLiveCoreView);
            typeSelectDialog.show();
        });
    }

    private void initDashboardIcon() {
        mImageDashboard.setOnClickListener(view -> {
            StreamDashboardDialog streamDashboardDialog = new StreamDashboardDialog(mContext);
            streamDashboardDialog.show();
        });
    }

    private void initWaitingCoGuestPassView() {
        mWaitingCoGuestPassView.setOnClickListener(view -> showCancelCoGuestRequestDialog());
    }

    private void initAudienceDashboardView() {
        mAudienceDashboardView.init(mLiveManager);
    }

    private void cancelCoGuestRequest() {
        mImageCoGuest.setImageResource(R.drawable.livekit_function_link_request);
        mImageCoGuest.setOnClickListener(view -> {
            showCancelCoGuestRequestDialog();
        });
    }

    private void stopCoGuest() {
        mImageCoGuest.setImageResource(R.drawable.livekit_function_linked);
        mImageCoGuest.setOnClickListener(view -> {
            showStopCoGuestDialog();
        });
    }

    private void showStopCoGuestDialog() {
        StopCoGuestDialog stopCoGuestDialog = new StopCoGuestDialog(mContext, mLiveCoreView, mLiveManager);
        stopCoGuestDialog.show();
    }

    private void showCancelCoGuestRequestDialog() {
        CancelRequestDialog linkMicDialog = new CancelRequestDialog(mContext, mLiveCoreView, mLiveManager);
        linkMicDialog.show();
    }

    @Override
    protected void addObserver() {
        mCoGuestState.coGuestStatus.observeForever(mLinkStatusObserver);
        mRoomState.liveStatus.observeForever(mLiveStatusChangeObserver);
        mUserState.enterUserInfo.observeForever(mEnterUserObserver);
        mCoHostState.connectedUsers.observeForever(mConnectedObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW, this);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, this);
    }

    @Override
    protected void removeObserver() {
        mCoGuestState.coGuestStatus.removeObserver(mLinkStatusObserver);
        mRoomState.liveStatus.removeObserver(mLiveStatusChangeObserver);
        mUserState.enterUserInfo.removeObserver(mEnterUserObserver);
        mCoHostState.connectedUsers.removeObserver(mConnectedObserver);
        TUICore.unRegisterEvent(this);
    }

    public void onExitButtonClick() {
        LOGGER.info("onExitButtonClick, mIsLoading:" + mIsLoading);
        if (mIsLoading) {
            return;
        }
        if (mCoGuestState.coGuestStatus.getValue() == LINKING) {
            showLiveStreamEndDialog();
        } else {
            leaveRoom();
            finishActivity();
        }
    }

    private void onLinkStatusChange(CoGuestState.CoGuestStatus linkStatus) {
        switch (linkStatus) {
            case NONE:
                mWaitingCoGuestPassView.setVisibility(GONE);
                initCoGuestIcon();
                break;
            case APPLYING:
                mWaitingCoGuestPassView.setVisibility(VISIBLE);
                cancelCoGuestRequest();
                break;
            case LINKING:
                mWaitingCoGuestPassView.setVisibility(GONE);
                stopCoGuest();
                break;
            default:
                break;
        }
        if (mRoomState.liveStatus.getValue() == NONE) {
            return;
        }
        Map<String, Object> params = new HashMap<>();
        if (LINKING == linkStatus) {
            params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, false);
        } else {
            params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, true);
        }
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, params);
    }

    private void onLiveStatusChange(RoomState.LiveStatus liveStatus) {
        initComponentView();
    }

    private void onEnterUserChange(UserState.UserInfo userInfo) {
        if (userInfo != null && mBarrageStreamView != null) {
            Barrage barrage = new Barrage();
            barrage.content = mContext.getString(R.string.common_entered_room);
            barrage.user.userId = userInfo.userId;
            barrage.user.userName = TextUtils.isEmpty(userInfo.name.getValue()) ? userInfo.userId :
                    userInfo.name.getValue();
            barrage.user.avatarUrl = userInfo.avatarUrl.getValue();
            mBarrageStreamView.insertBarrages(barrage);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onConnectedUserChange(List<CoHostState.ConnectionUser> connectedList) {
        post(() -> {
            enableView(mImageCoGuest, connectedList.isEmpty());
        });
    }

    private void enableView(View view, boolean enable) {
        view.setEnabled(enable);
        view.setAlpha(enable ? 1.0f : 0.5f);
    }

    private void showLiveStreamEndDialog() {
        EndLiveStreamDialog dialog = new EndLiveStreamDialog(mContext, mLiveCoreView, mLiveManager,
                view -> {
                    leaveRoom();
                    finishActivity();
                }
        );
        dialog.show();
    }

    private void finishActivity() {
        if (mContext instanceof Activity) {
            Activity activity = (Activity) mContext;
            Intent intent = new Intent();
            activity.setResult(RESULT_OK, intent);
            activity.finish();
        }
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_FINISH_ACTIVITY.equals(subKey)) {
            if (param != null && param.containsKey("roomId")) {
                String roomId = (String) param.get("roomId");
                if (roomId != null && roomId.equals(mLiveManager.getRoomState().roomId)) {
                    leaveRoom();
                    finishActivity();
                }
            }
        } else if (TextUtils.equals(subKey, EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW)) {
            UserInfo userInfo = (UserInfo) param.get(EVENT_PARAMS_KEY_USER_INFO);
            String roomId = (String) param.get(EVENT_PARAMS_KEY_ROOM_ID);
            if (!TextUtils.isEmpty(roomId) && roomId.equals(mLiveManager.getRoomState().roomId)) {
                showCoGuestManageDialog(userInfo);
            }
        }
    }

    public class VideoViewAdapterImpl implements LiveCoreViewDefine.VideoViewAdapter {

        private final WeakReference<Context> mWeakContext;

        public VideoViewAdapterImpl(Context context) {
            mWeakContext = new WeakReference<>(context);
        }

        @Override
        public View createCoGuestView(UserInfo userInfo) {
            Context context = mWeakContext.get();
            if (context == null) {
                LOGGER.error("createCoGuestView: context is null");
                return null;
            }
            CoGuestWidgetsView coGuestWidgetsView = new CoGuestWidgetsView(context);
            coGuestWidgetsView.init(mLiveManager, userInfo);
            coGuestWidgetsView.setOnClickListener(v -> {
                Map<String, Object> params = new HashMap<>();
                params.put(EVENT_PARAMS_KEY_USER_INFO, userInfo);
                params.put(EVENT_PARAMS_KEY_ROOM_ID, mLiveManager.getRoomState().roomId);
                TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW, params);
            });
            return coGuestWidgetsView;
        }

        @Override
        public void updateCoGuestView(View coGuestView, UserInfo userInfo,
                                      List<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlag) {
            LOGGER.info("updateCoGuestView: userInfo = " + new Gson().toJson(userInfo)
                    + ",modifyFlag = " + new Gson().toJson(modifyFlag) + ",coGuestView = " + coGuestView);
        }

        @Override
        public View createCoHostView(LiveCoreViewDefine.CoHostUser coHostUser) {
            Context context = mWeakContext.get();
            if (context == null) {
                LOGGER.error("createCoHostView: context is null");
                return null;
            }
            CoHostWidgetsView coHostWidgetsView = new CoHostWidgetsView(context);
            coHostWidgetsView.init(mLiveManager, coHostUser);
            return coHostWidgetsView;
        }

        @Override
        public void updateCoHostView(View coHostView, LiveCoreViewDefine.CoHostUser coHostUser,
                                     List<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlag) {
            LOGGER.info("updateCoHostView: coHostUser = " + new Gson().toJson(coHostUser)
                    + ",modifyFlag = " + new Gson().toJson(modifyFlag) + ",coHostView = " + coHostView);
        }

        @Override
        public View createBattleView(TUILiveBattleManager.BattleUser battleUser) {
            Context context = mWeakContext.get();
            if (context == null) {
                LOGGER.error("createBattleView: context is null");
                return null;
            }
            BattleMemberInfoView battleMemberInfoView = new BattleMemberInfoView(context);
            battleMemberInfoView.init(mLiveManager, battleUser.userId);
            return battleMemberInfoView;
        }

        @Override
        public void updateBattleView(View battleView, TUILiveBattleManager.BattleUser battleUser) {

        }

        @Override
        public View createBattleContainerView() {
            Context context = mWeakContext.get();
            if (context == null) {
                LOGGER.error("createBattleContainerView: context is null");
                return null;
            }
            BattleInfoView battleInfoView = new BattleInfoView(context);
            battleInfoView.init(mLiveManager);
            return battleInfoView;
        }

        @Override
        public void updateBattleContainerView(View battleContainnerView,
                                              List<LiveCoreViewDefine.BattleUserViewModel> userInfos) {
            BattleInfoView battleInfoView = (BattleInfoView) battleContainnerView;
            battleInfoView.updateView(userInfos);
        }
    }

    public interface ViewObserver {
        void onLoading();

        void onFinished();
    }
}
