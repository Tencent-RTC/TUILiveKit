package com.trtc.uikit.livekit.features.audiencecontainer.view;

import static com.tencent.qcloud.tuicore.util.ScreenUtil.dip2px;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;
import static com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState.CoGuestStatus.APPLYING;
import static com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState.CoGuestStatus.LINKING;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.pm.ActivityInfo;
import android.graphics.Point;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.Display;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewConfiguration;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomInfoCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUIThemeManager;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.imageloader.ImageOptions;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.Constants;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.barrage.BarrageInputView;
import com.trtc.uikit.livekit.component.barrage.BarrageStreamView;
import com.trtc.uikit.livekit.component.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyStore;
import com.trtc.uikit.livekit.component.beauty.tebeauty.store.TEBeautyStore;
import com.trtc.uikit.livekit.component.dashboard.StreamDashboardDialog;
import com.trtc.uikit.livekit.component.gift.GiftPlayView;
import com.trtc.uikit.livekit.component.gift.LikeButton;
import com.trtc.uikit.livekit.component.gift.store.TUIGiftStore;
import com.trtc.uikit.livekit.component.giftaccess.GiftButton;
import com.trtc.uikit.livekit.component.giftaccess.service.GiftCacheService;
import com.trtc.uikit.livekit.component.giftaccess.store.GiftStore;
import com.trtc.uikit.livekit.component.giftaccess.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.giftaccess.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.component.networkInfo.NetworkInfoView;
import com.trtc.uikit.livekit.component.roominfo.LiveInfoView;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.AudienceContainerViewListenerList;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.LiveBattleManagerObserver;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.LiveStreamObserver;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceContainerConfig;
import com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState.CoGuestStatus;
import com.trtc.uikit.livekit.features.audiencecontainer.view.battle.widgets.BattleInfoView;
import com.trtc.uikit.livekit.features.audiencecontainer.view.battle.widgets.BattleMemberInfoView;
import com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel.AnchorManagerDialog;
import com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel.CancelRequestDialog;
import com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel.CoGuestRequestFloatView;
import com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel.StopCoGuestDialog;
import com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel.TypeSelectDialog;
import com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.widgets.CoGuestWidgetsView;
import com.trtc.uikit.livekit.features.audiencecontainer.view.cohost.widgets.CoHostWidgetsView;
import com.trtc.uikit.livekit.features.audiencecontainer.view.userinfo.UserInfoDialog;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import org.json.JSONException;
import org.json.JSONObject;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class AudienceView extends BasicView {
    private static final LiveKitLogger LOGGER            = LiveKitLogger.getLiveStreamLogger("AudienceView");
    private static final String        DEFAULT_COVER_URL =
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png";

    private TUILiveListManager.LiveInfo mLiveInfo;
    private AudienceManager             mAudienceManager;
    private LiveCoreView                mLiveCoreView;
    private FrameLayout                 mLayoutPlaying;
    private FrameLayout                 mLayoutLiveCoreView;
    private ImageView                   mIvVideoViewBackground;
    private ImageView                   mImageDashboard;
    private GiftButton                  mButtonGift;
    private LikeButton                  mButtonLike;
    private ImageView                   mImageCoGuest;
    private BarrageInputView            mBarrageInputView;
    private LiveInfoView                mRoomInfoView;
    private GiftPlayView                mGiftPlayView;
    private AudienceListView            mAudienceListView;
    private BarrageStreamView           mBarrageStreamView;
    private NetworkInfoView             mNetworkInfoView;
    private ImageView                   mImageFloatWindow;
    private ImageView                   mImageStandardExit;
    private ImageView                   mImageCompactExit;
    private FrameLayout                 mLayoutSwitchOrientationButton;
    private ImageView                   mImageSwitchOrientationIcon;
    private CoGuestRequestFloatView     mWaitingCoGuestPassView;
    private AudiencePlayingRootView     mAudiencePlayingRootView;
    private UserInfoDialog              mUserInfoDialog;
    private AnchorManagerDialog         mAnchorManagerDialog;
    private ViewObserver                mViewObserver;
    private boolean                     mIsLoading;
    private LiveStreamObserver          mLiveStreamObserver;
    private LiveBattleManagerObserver   mLiveBattleManagerObserver;
    private VideoViewAdapterImpl        mVideoViewAdapterImpl;

    private       float   mTouchX;
    private       float   mTouchY;
    private final int     mTouchSlop;
    private       boolean mIsSwiping;
    private       boolean mIsJoinRoom;

    private final Observer<List<ConnectionUser>> mCoHostConnectedUsersObserver    = this::onCoHostConnectedUsersChanged;
    private final Observer<CoGuestStatus>        mLinkStatusObserver              = this::onLinkStatusChange;
    private final Observer<Boolean>              mDisableHeaderFloatWinObserver   = this::onHeaderFloatWinDisable;
    private final Observer<Boolean>              mDisableHeaderLiveDataObserver   = this::onHeaderLiveDataDisable;
    private final Observer<Boolean>              mDisableHeaderVisitorCntObserver = this::onHeaderVisitorCntDisable;
    private final Observer<Boolean>              mDisableFooterCoGuestObserver    = this::onFooterCoGuestDisable;
    private final Observer<Boolean>              mVideoOrientationObserver        = this::onVideoOrientationChanged;

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

    public void init(TUILiveListManager.LiveInfo liveInfo) {
        LOGGER.info("AudienceView init:" + this);
        mLiveInfo = liveInfo;
        mLiveCoreView = new LiveCoreView(ContextProvider.getApplicationContext());
        mAudienceManager = new AudienceManager();
        mAudienceManager.setCoreState(mLiveCoreView.getCoreState());
        init(mAudienceManager);
        mRoomManager.updateRoomState(liveInfo.roomInfo);
        mAudienceManager.getMediaManager().setCustomVideoProcess();
        mLayoutLiveCoreView.addView(mLiveCoreView);
        createVideoMuteBitmap();
        setComponent();
        setVideoViewAdapter();
        setLayoutBackground(mLiveInfo.coverUrl);
    }

    public String getRoomId() {
        if (mLiveInfo != null && mLiveInfo.roomInfo != null) {
            return mLiveInfo.roomInfo.roomId;
        }
        return "";
    }

    public void setAudienceContainerViewListenerList(AudienceContainerViewListenerList viewListenerList) {
        mAudienceManager.setAudienceContainerViewListenerList(viewListenerList);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_livestream_audience_view, this, true);
        mLayoutPlaying = findViewById(R.id.fl_playing);
        mImageCoGuest = findViewById(R.id.iv_co_guest);
        mButtonGift = findViewById(R.id.btn_gift);
        mButtonLike = findViewById(R.id.btn_like);
        mBarrageInputView = findViewById(R.id.barrage_input_view);
        mImageDashboard = findViewById(R.id.iv_dashboard);
        mRoomInfoView = findViewById(R.id.room_info_view);
        mAudienceListView = findViewById(R.id.audience_list_view);
        mBarrageStreamView = findViewById(R.id.barrage_stream_view);
        mNetworkInfoView = findViewById(R.id.network_info_view);
        mWaitingCoGuestPassView = findViewById(R.id.btn_waiting_pass);
        mGiftPlayView = findViewById(R.id.gift_play_view);
        mImageFloatWindow = findViewById(R.id.iv_float_window);
        mImageStandardExit = findViewById(R.id.iv_standard_exit_room);
        mImageCompactExit = findViewById(R.id.iv_compact_exit_room);
        mIvVideoViewBackground = findViewById(R.id.video_view_background);
        mLayoutLiveCoreView = findViewById(R.id.live_core_view);
        mAudiencePlayingRootView = findViewById(R.id.fl_playing_root);
        mLayoutSwitchOrientationButton = findViewById(R.id.fl_switch_orientation_button);
        mImageSwitchOrientationIcon = findViewById(R.id.img_switch_orientation_button_icon);
        initFloatWindowView();
    }

    private void initFloatWindowView() {
        mImageFloatWindow.setOnClickListener(v -> {
            if (((Activity) mContext).getRequestedOrientation() == ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE) {
                setScreenOrientation(true);
            }
            mAudienceManager.notifyPictureInPictureClick();
        });
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

    public void enablePictureInPictureMode(boolean enable) {
        if (enable) {
            mAudiencePlayingRootView.setVisibility(GONE);
        } else {
            mAudiencePlayingRootView.setVisibility(VISIBLE);
        }
        mAudienceManager.enablePictureInPictureMode(enable);
    }

    public void startPreviewLiveStream() {
        if (mLiveCoreView != null) {
            mLiveCoreView.startPreviewLiveStream(mLiveInfo.roomInfo.roomId, true, null);
        }
    }

    public void stopPreviewLiveStream() {
        if (mLiveCoreView != null) {
            mLiveCoreView.stopPreviewLiveStream(mLiveInfo.roomInfo.roomId);
        }
    }

    public void joinRoom() {
        mIsJoinRoom = true;
        if (mLiveStreamObserver == null) {
            mLiveStreamObserver = new LiveStreamObserver(mAudienceManager);
            mLiveCoreView.registerConnectionObserver(mLiveStreamObserver);
        }

        if (mLiveBattleManagerObserver == null) {
            mLiveBattleManagerObserver = new LiveBattleManagerObserver(mAudienceManager);
            mLiveCoreView.registerBattleObserver(mLiveBattleManagerObserver);
        }
        mAudienceManager.addObserver();
        mLayoutPlaying.setVisibility(GONE);
        onViewLoading();
        mAudienceManager.getMediaManager().setCustomVideoProcess();
        mLiveCoreView.setLocalVideoMuteImage(mMediaState.bigMuteBitmap, mMediaState.smallMuteBitmap);
        mLiveCoreView.joinLiveStream(mLiveInfo.roomInfo.roomId, new GetRoomInfoCallback() {
            @Override
            public void onSuccess(RoomInfo roomInfo) {
                Activity activity = (Activity) mContext;
                if (activity.isFinishing() || activity.isDestroyed()) {
                    LOGGER.warn("activity is exit, leaveLiveStream");
                    mLiveCoreView.leaveLiveStream(null);
                    mLiveCoreView.setLocalVideoMuteImage(null, null);
                    mLiveCoreView.unregisterConnectionObserver(mLiveStreamObserver);
                    mLiveCoreView.unregisterBattleObserver(mLiveBattleManagerObserver);
                    return;
                }
                mRoomManager.updateRoomState(roomInfo);
                initComponentView(roomInfo);
                onViewFinished();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                onViewFinished();
                ErrorLocalized.onError(error);
                TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER, null);
            }
        });
    }

    public void leaveRoom() {
        if (!mIsJoinRoom) {
            return;
        }
        mIsJoinRoom = false;
        String roomId = mLiveInfo.roomInfo.roomId;
        stopPreviewLiveStream();
        mAudienceManager.removeObserver();
        mLiveCoreView.unregisterConnectionObserver(mLiveStreamObserver);
        mLiveStreamObserver = null;
        mLiveCoreView.unregisterBattleObserver(mLiveBattleManagerObserver);
        mLiveBattleManagerObserver = null;
        mLiveCoreView.leaveLiveStream(null);
        mLiveCoreView.setLocalVideoMuteImage(null, null);
        mMediaManager.releaseVideoMuteBitmap();
        mAudienceManager.getState().reset();
        mRoomInfoView.unInit();
        BarrageStore.sharedInstance().unInit(roomId);
        TUIGiftStore.sharedInstance().unInit(roomId);
        BasicBeautyStore.getInstance().unInit();
        TEBeautyStore.getInstance().unInit();
    }

    public void setViewObserver(ViewObserver observer) {
        mViewObserver = observer;
    }

    private void initSwitchOrientationButtonView() {
        updateViewByOrientation(((Activity) mContext).getRequestedOrientation() == ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        mLayoutSwitchOrientationButton.setOnClickListener(v -> {
            boolean isPortrait =
                    ((Activity) mContext).getRequestedOrientation() == ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
            setScreenOrientation(!isPortrait);
        });
    }

    private void setScreenOrientation(boolean isPortrait) {
        ((Activity) mContext).setRequestedOrientation(
                isPortrait ? ActivityInfo.SCREEN_ORIENTATION_PORTRAIT : ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        updateViewByOrientation(isPortrait);
    }

    private void updateViewByOrientation(boolean isPortrait) {
        mLayoutSwitchOrientationButton.setLayoutParams(getSwitchScreenButtonPosition(isPortrait));
        mImageSwitchOrientationIcon.setImageResource(isPortrait ?
                R.drawable.livekit_ic_switch_landscape_button : R.drawable.livekit_ic_switch_portrait_button);
        mBarrageInputView.setVisibility(isPortrait ? VISIBLE : GONE);
        mButtonGift.setVisibility(isPortrait ? VISIBLE : GONE);
        mImageDashboard.setVisibility(isPortrait ? VISIBLE : GONE);
        mButtonLike.setVisibility(isPortrait ? VISIBLE : GONE);

        FrameLayout.LayoutParams audienceListParams = (FrameLayout.LayoutParams) mAudienceListView.getLayoutParams();
        audienceListParams.topMargin = isPortrait ? dip2px(60) : dip2px(30);
        mAudienceListView.setScreenOrientation(isPortrait);
        mAudienceListView.setLayoutParams(audienceListParams);

        FrameLayout.LayoutParams barrageStreamParams = (FrameLayout.LayoutParams) mBarrageStreamView.getLayoutParams();
        barrageStreamParams.width = isPortrait ? ViewGroup.LayoutParams.MATCH_PARENT : dip2px(305);
        barrageStreamParams.bottomMargin = isPortrait ? dip2px(70) : dip2px(74);
        mBarrageStreamView.setLayoutParams(barrageStreamParams);

        FrameLayout.LayoutParams standardExitParams = (FrameLayout.LayoutParams) mImageStandardExit.getLayoutParams();
        standardExitParams.topMargin = isPortrait ? dip2px(60) : dip2px(30);
        mImageStandardExit.setLayoutParams(standardExitParams);

        FrameLayout.LayoutParams floatWindowParams = (FrameLayout.LayoutParams) mImageFloatWindow.getLayoutParams();
        floatWindowParams.topMargin = isPortrait ? dip2px(60) : dip2px(30);
        mImageFloatWindow.setLayoutParams(floatWindowParams);

        FrameLayout.LayoutParams roomInfoParams = (FrameLayout.LayoutParams) mRoomInfoView.getLayoutParams();
        roomInfoParams.topMargin = isPortrait ? dip2px(52) : dip2px(30);
        mRoomInfoView.setLayoutParams(roomInfoParams);
        mRoomInfoView.setScreenOrientation(isPortrait);

        mNetworkInfoView.setScreenOrientation(isPortrait);
    }

    private Point getScreenPoint() {
        WindowManager windowManager = (WindowManager) mContext.getSystemService(Context.WINDOW_SERVICE);
        Display defaultDisplay = windowManager.getDefaultDisplay();
        Point point = new Point();
        defaultDisplay.getSize(point);
        return point;
    }

    private FrameLayout.LayoutParams getSwitchScreenButtonPosition(boolean isPortrait) {
        Point point = getScreenPoint();
        int screenWidth = point.x;
        int screenHeight = point.y;
        int videoWidth, videoHeight;
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                Gravity.BOTTOM | Gravity.END);

        if (isPortrait) {
            videoWidth = screenWidth;
            videoHeight = videoWidth * 9 / 16;
            int videoTop = (screenHeight - videoHeight) / 2;
            params.rightMargin = dip2px(12);
            params.bottomMargin = videoTop + dip2px(24);
        } else {
            videoHeight = screenWidth;
            videoWidth = videoHeight * 16 / 9;
            int videoRightMargin = (screenHeight - videoWidth) / 2;
            int videoBottomMargin = (videoHeight / 2);
            params.rightMargin = videoRightMargin + dip2px(24);
            params.bottomMargin = videoBottomMargin;
        }
        return params;
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
        if (userInfo.userId.equals(mCoreState.userState.selfInfo.getValue().userId)) {
            showAnchorManagerDialog(userInfo);
        } else {
            showUserInfoDialog(userInfo);
        }
    }

    private void showAnchorManagerDialog(UserInfo userInfo) {
        if (mAnchorManagerDialog == null) {
            mAnchorManagerDialog = new AnchorManagerDialog(mContext, mAudienceManager, mLiveCoreView);
        }
        mAnchorManagerDialog.init(userInfo);
        mAnchorManagerDialog.show();
    }

    private void showUserInfoDialog(UserInfo userInfo) {
        if (mUserInfoDialog == null) {
            mUserInfoDialog = new UserInfoDialog(mContext, mAudienceManager);
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

    private void createVideoMuteBitmap() {
        int bigMuteImageResId = Locale.ENGLISH.getLanguage().equals(TUIThemeManager.getInstance().getCurrentLanguage())
                ? R.drawable.livekit_local_mute_image_en : R.drawable.livekit_local_mute_image_zh;
        int smallMuteImageResId = R.drawable.livekit_local_mute_image_multi;
        mMediaManager.createVideoMuteBitmap(getContext(), bigMuteImageResId, smallMuteImageResId);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mLiveCoreView.setVideoViewAdapter(null);
    }

    private void initComponentView(RoomInfo roomInfo) {
        mLayoutPlaying.setVisibility(VISIBLE);
        initCoGuestIcon();
        initRoomInfoView(roomInfo);
        initAudienceListView(roomInfo);
        initNetworkView(roomInfo);
        initExitRoomView();
        initBarrageStreamView(roomInfo);
        initBarrageInputView(roomInfo);
        initDashboardIcon();
        initGiftView(roomInfo);
        initLikeView(roomInfo);
        initGiftPlayView(roomInfo);
        initWaitingCoGuestPassView();
    }

    private void initAudienceListView(RoomInfo roomInfo) {
        mAudienceListView.init(roomInfo);
    }

    private void initNetworkView(RoomInfo roomInfo) {
        mNetworkInfoView.init(roomInfo.createTime);
    }

    private void initExitRoomView() {
        mImageStandardExit.setOnClickListener(view -> onExitButtonClick());
        mImageCompactExit.setOnClickListener(view -> onExitButtonClick());
    }

    private void initRoomInfoView(RoomInfo roomInfo) {
        mRoomInfoView.init(roomInfo);
    }

    private void initBarrageStreamView(RoomInfo roomInfo) {
        mBarrageStreamView.init(roomInfo.roomId, roomInfo.ownerId);
        mBarrageStreamView.setItemTypeDelegate(new BarrageViewTypeDelegate());
        mBarrageStreamView.setItemAdapter(GIFT_VIEW_TYPE_1, new GiftBarrageAdapter(mContext));
        mBarrageStreamView.setOnMessageClickListener(userInfo -> {
            if (TextUtils.isEmpty(userInfo.userId)) {
                return;
            }
            if (userInfo.userId.equals(mCoreState.userState.selfInfo.getValue().userId)) {
                return;
            }

            if (mUserInfoDialog == null) {
                mUserInfoDialog = new UserInfoDialog(mContext, mAudienceManager);
            }
            mUserInfoDialog.init(userInfo);
            mUserInfoDialog.show();
        });
    }

    private void initBarrageInputView(RoomInfo roomInfo) {
        mBarrageInputView.init(roomInfo.roomId);
    }

    private void initGiftView(RoomInfo roomInfo) {
        mButtonGift.init(roomInfo.roomId, roomInfo.ownerId, roomInfo.ownerName, roomInfo.ownerAvatarUrl);
    }

    private void initLikeView(RoomInfo roomInfo) {
        mButtonLike.init(roomInfo.roomId);
    }

    private void initGiftPlayView(RoomInfo roomInfo) {
        mGiftPlayView.init(roomInfo.roomId);
        GiftCacheService giftCacheService = GiftStore.getInstance().mGiftCacheService;
        mGiftPlayView.setListener(new GiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(GiftPlayView view, TUILiveGiftManager.GiftInfo gift, int giftCount,
                                      UserInfo sender) {
                if (mBarrageStreamView == null) {
                    return;
                }
                Barrage barrage = new Barrage();
                barrage.content = "gift";
                barrage.user.userId = sender.userId;
                barrage.user.userName = TextUtils.isEmpty(sender.userName) ? sender.userId : sender.userName;
                barrage.user.avatarUrl = sender.avatarUrl;
                barrage.extInfo.put(GIFT_VIEW_TYPE, GIFT_VIEW_TYPE_1);
                barrage.extInfo.put(GIFT_NAME, gift.name);
                barrage.extInfo.put(GIFT_COUNT, giftCount);
                barrage.extInfo.put(GIFT_ICON_URL, gift.iconUrl);
                barrage.extInfo.put(GIFT_RECEIVER_USERNAME, TextUtils.isEmpty(mRoomState.roomInfo.ownerName)
                        ? mRoomState.roomInfo.ownerId : mRoomState.roomInfo.ownerName);
                mBarrageStreamView.insertBarrages(barrage);
            }

            @Override
            public void onPlayGiftAnimation(GiftPlayView view, TUILiveGiftManager.GiftInfo gift) {
                giftCacheService.request(gift.resourceUrl, (error, result) -> {
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
            TypeSelectDialog typeSelectDialog = new TypeSelectDialog(mContext, mAudienceManager, mLiveCoreView);
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

    private void cancelCoGuestRequest() {
        mImageCoGuest.setImageResource(R.drawable.livekit_function_link_request);
        mImageCoGuest.setOnClickListener(view -> showCancelCoGuestRequestDialog());
    }

    private void stopCoGuest() {
        mImageCoGuest.setImageResource(R.drawable.livekit_function_linked);
        mImageCoGuest.setOnClickListener(view -> {
            showStopCoGuestDialog();
        });
    }

    private void showStopCoGuestDialog() {
        StopCoGuestDialog stopCoGuestDialog = new StopCoGuestDialog(mContext, mLiveCoreView, mAudienceManager);
        stopCoGuestDialog.show();
    }

    private void showCancelCoGuestRequestDialog() {
        CancelRequestDialog linkMicDialog = new CancelRequestDialog(mContext, mLiveCoreView, mAudienceManager);
        linkMicDialog.show();
    }

    @Override
    protected void addObserver() {
        mRoomState.videoStreamIsLandscape.observeForever(mVideoOrientationObserver);
        mCoGuestState.coGuestStatus.observeForever(mLinkStatusObserver);
        mCoreState.coHostState.connectedUserList.observeForever(mCoHostConnectedUsersObserver);
        AudienceContainerConfig.disableHeaderFloatWin.observeForever(mDisableHeaderFloatWinObserver);
        AudienceContainerConfig.disableHeaderLiveData.observeForever(mDisableHeaderLiveDataObserver);
        AudienceContainerConfig.disableHeaderVisitorCnt.observeForever(mDisableHeaderVisitorCntObserver);
        AudienceContainerConfig.disableFooterCoGuest.observeForever(mDisableFooterCoGuestObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.videoStreamIsLandscape.removeObserver(mVideoOrientationObserver);
        mCoGuestState.coGuestStatus.removeObserver(mLinkStatusObserver);
        mCoreState.coHostState.connectedUserList.removeObserver(mCoHostConnectedUsersObserver);
        AudienceContainerConfig.disableHeaderFloatWin.removeObserver(mDisableHeaderFloatWinObserver);
        AudienceContainerConfig.disableHeaderLiveData.removeObserver(mDisableHeaderLiveDataObserver);
        AudienceContainerConfig.disableHeaderVisitorCnt.removeObserver(mDisableHeaderVisitorCntObserver);
        AudienceContainerConfig.disableFooterCoGuest.removeObserver(mDisableFooterCoGuestObserver);
    }

    public void onExitButtonClick() {
        LOGGER.info("onExitButtonClick, mIsLoading:" + mIsLoading);
        if (mIsLoading) {
            return;
        }
        if (mCoGuestState.coGuestStatus.getValue() == LINKING) {
            showLiveStreamEndDialog();
        } else {
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER, null);
        }
    }

    private void onLinkStatusChange(CoGuestStatus linkStatus) {
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
        Map<String, Object> params = new HashMap<>();
        if (LINKING == linkStatus || APPLYING == linkStatus) {
            params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, false);
        } else {
            params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, true);
        }
        TUICore.notifyEvent("EVENT_KEY_LIVE_KIT", EVENT_SUB_KEY_LINK_STATUS_CHANGE, params);
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onCoHostConnectedUsersChanged(List<ConnectionUser> connectedList) {
        post(() -> enableView(mImageCoGuest, connectedList.isEmpty()));
    }

    private void onHeaderFloatWinDisable(Boolean disable) {
        mImageFloatWindow.setVisibility(disable ? GONE : VISIBLE);
    }

    private void onHeaderLiveDataDisable(Boolean disable) {
        mRoomInfoView.setVisibility(disable ? GONE : VISIBLE);
        if (disable) {
            mAudienceListView.setVisibility(GONE);
        } else {
            mAudienceListView.setVisibility(
                    Boolean.TRUE.equals(AudienceContainerConfig.disableHeaderVisitorCnt.getValue()) ? GONE : VISIBLE);
        }
    }

    private void onHeaderVisitorCntDisable(Boolean disable) {
        if (Boolean.TRUE.equals(AudienceContainerConfig.disableHeaderLiveData.getValue())) {
            mAudienceListView.setVisibility(GONE);
        } else {
            mAudienceListView.setVisibility(disable ? GONE : VISIBLE);
        }
    }

    private void onFooterCoGuestDisable(Boolean disable) {
        mImageCoGuest.setVisibility(disable ? GONE : VISIBLE);
    }

    private void onVideoOrientationChanged(Boolean videoStreamIsLandscape) {
        mLayoutSwitchOrientationButton.setVisibility(videoStreamIsLandscape ? VISIBLE : GONE);
        mImageCoGuest.setVisibility(videoStreamIsLandscape ? GONE : VISIBLE);
        if (videoStreamIsLandscape) {
            initSwitchOrientationButtonView();
        } else {
            if (((Activity) mContext).getRequestedOrientation() == ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE) {
                setScreenOrientation(true);
            }
        }
    }

    private void enableView(View view, boolean enable) {
        view.setEnabled(enable);
        view.setAlpha(enable ? 1.0f : 0.5f);
    }

    private void showLiveStreamEndDialog() {
        EndLiveStreamDialog dialog = new EndLiveStreamDialog(mContext, mLiveCoreView, mAudienceManager,
                view -> TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER, null)
        );
        dialog.show();
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
            coGuestWidgetsView.init(mAudienceManager, userInfo);
            coGuestWidgetsView.setOnClickListener(v -> showCoGuestManageDialog(userInfo));
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
            coHostWidgetsView.init(mAudienceManager, coHostUser);
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
            battleMemberInfoView.init(mAudienceManager, battleUser.userId);
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
            battleInfoView.init(mAudienceManager);
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
