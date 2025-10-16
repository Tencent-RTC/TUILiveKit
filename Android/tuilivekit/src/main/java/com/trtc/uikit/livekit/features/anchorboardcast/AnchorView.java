package com.trtc.uikit.livekit.features.anchorboardcast;

import static android.app.Activity.RESULT_OK;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine.RoomBehavior;
import static com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine.RoomBehavior.CREATE_ROOM;
import static com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine.RoomBehavior.ENTER_ROOM;
import static com.trtc.uikit.livekit.features.anchorboardcast.state.BattleState.BATTLE_DURATION;
import static com.trtc.uikit.livekit.features.anchorboardcast.state.BattleState.BATTLE_REQUEST_TIMEOUT;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.SeatFullInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUIThemeManager;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;
import com.trtc.tuikit.common.permission.PermissionCallback;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.tuikit.common.util.ScreenUtil;
import com.trtc.tuikit.common.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.PermissionRequest;
import com.trtc.uikit.livekit.common.ui.RoundFrameLayout;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.barrage.BarrageInputView;
import com.trtc.uikit.livekit.component.barrage.BarrageStreamView;
import com.trtc.uikit.livekit.component.beauty.BeautyUtils;
import com.trtc.uikit.livekit.component.beauty.tebeauty.store.TEBeautyStore;
import com.trtc.uikit.livekit.component.gift.GiftPlayView;
import com.trtc.uikit.livekit.component.giftaccess.service.GiftCacheService;
import com.trtc.uikit.livekit.component.giftaccess.store.GiftStore;
import com.trtc.uikit.livekit.component.giftaccess.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.giftaccess.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.component.networkInfo.NetworkInfoView;
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore;
import com.trtc.uikit.livekit.component.roominfo.LiveInfoView;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.observer.AnchorBattleObserver;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.observer.AnchorConnectionObserver;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorConfig;
import com.trtc.uikit.livekit.features.anchorboardcast.state.BattleState.BattleUser;
import com.trtc.uikit.livekit.features.anchorboardcast.view.BasicView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.EndLiveStreamDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.battle.panel.AnchorEndBattleDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.battle.panel.BattleCountdownDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.battle.widgets.BattleInfoView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.battle.widgets.BattleMemberInfoView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.panel.AnchorCoGuestManageDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.panel.AnchorManagerDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.panel.ApplyCoGuestFloatView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.panel.CoGuestIconView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.widgets.AnchorEmptySeatView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.widgets.CoGuestBackgroundWidgetsView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.widgets.CoGuestForegroundWidgetsView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.panel.AnchorCoHostManageDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.panel.StandardDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.widgets.CoHostBackgroundWidgetsView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.widgets.CoHostForegroundWidgetsView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.settings.SettingsPanelDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.usermanage.UserManagerDialog;

import org.jetbrains.annotations.NotNull;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

import io.trtc.tuikit.atomicxcore.api.Barrage;
import io.trtc.tuikit.atomicxcore.api.CoGuestStore;
import io.trtc.tuikit.atomicxcore.api.CoHostListener;
import io.trtc.tuikit.atomicxcore.api.CoHostStore;
import io.trtc.tuikit.atomicxcore.api.CoreViewType;
import io.trtc.tuikit.atomicxcore.api.Gift;
import io.trtc.tuikit.atomicxcore.api.HostListener;
import io.trtc.tuikit.atomicxcore.api.LiveCoreView;
import io.trtc.tuikit.atomicxcore.api.LiveUserInfo;
import io.trtc.tuikit.atomicxcore.api.SeatUserInfo;
import io.trtc.tuikit.atomicxcore.api.VideoViewAdapter;
import io.trtc.tuikit.atomicxcore.api.ViewLayer;
import io.trtc.tuikit.atomicxcore.api.deprecated.LiveCoreViewDefine;

@SuppressLint("ViewConstructor")
public class AnchorView extends BasicView implements EndLiveStreamDialog.EndLiveStreamDialogListener,
        AnchorManager.LiveStateListener {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorView");

    private RoomBehavior                mBehavior  = CREATE_ROOM;
    private RoundFrameLayout            mLayoutCoreViewContainer;
    private LiveCoreView                mLiveCoreView;
    private FrameLayout                 mLayoutComponentsContainer;
    private FrameLayout                 mLayoutHeaderContainer;
    private ImageView                   mImageEndLive;
    private ImageView                   mImageFloatWindow;
    private View                        mViewCoGuest;
    private View                        mViewCoHost;
    private View                        mViewBattle;
    private AudienceListView            mAudienceListView;
    private LiveInfoView                mRoomInfoView;
    private NetworkInfoView             mNetworkInfoView;
    private BarrageInputView            mBarrageInputView;
    private BarrageStreamView           mBarrageStreamView;
    private GiftPlayView                mGiftPlayView;
    private ApplyCoGuestFloatView       mApplyCoGuestFloatView;
    private StandardDialog              mProcessConnectionDialog;
    private StandardDialog              mProcessBattleDialog;
    private BattleCountdownDialog       mBattleCountdownDialog;
    private AnchorManagerDialog         mAnchorManagerDialog;
    private UserManagerDialog           mUserManagerDialog;
    private AnchorConnectionObserver    mLiveStreamObserver;
    private AnchorBattleObserver        mLiveBattleManagerObserver;
    private AnchorEndBattleDialog       mAnchorEndBattleDialog;
    private TUILiveListManager.LiveInfo mLiveInfo;
    private boolean                     mIsDestroy = false;

    private final Observer<ConnectionUser>       mReceivedConnectRequestObserver = this::onReceivedCoHostRequest;
    private final Observer<List<ConnectionUser>> mConnectedObserver              = this::onConnectedUserChange;
    private final Observer<List<BattleUser>>     mBattleUserObserver             = this::onBattleUserChange;
    private final Observer<List<UserInfo>>       mCoGuestUserObserver            = this::onCoGuestUserChange;
    private final Observer<Boolean>              mBattleStartObserver            = this::onBattleStartChange;
    private final Observer<Boolean>              mBattleResultDisplayObserver    = this::onBattleResultDisplay;
    private final Observer<BattleUser>           mReceivedBattleRequestObserver  = this::onReceivedBattleRequestChange;
    private final Observer<Boolean>              mInWaitingObserver              = this::onInWaitingChange;
    private final Observer<Boolean>              mDisableLiveDataObserver        = this::onDisableLiveDataChange;
    private final Observer<Boolean>              mDisableVisitorCntObserver      = this::onDisableVisitorCntChange;
    private final Observer<Boolean>              mDisableCoGuestObserver         = this::ondDisableCoGuestChange;
    private final Observer<Boolean>              mDisableCoHostObserver          = this::onDisableCoHostChange;
    private final Observer<Boolean>              mDisableBattleObserver          = this::onDisableBattleChange;
    private final Observer<Boolean>              mPipModeObserver                = this::onPipModeObserver;
    private final CoHostListener                 mCoHostListener                 = new CoHostListener() {
        @Override
        public void onCoHostRequestReceived(@NotNull SeatUserInfo inviter,
                                            @org.jetbrains.annotations.Nullable String extensionInfo) {
            CoGuestStore coGuestStore = CoGuestStore.create(mLiveInfo.roomId);
            CoHostStore coHostStore = CoHostStore.create(mLiveInfo.roomId);
            List<SeatUserInfo> list = new ArrayList<>();
            for (SeatUserInfo userInfo : coGuestStore.getCoGuestState().getConnected().getValue()) {
                if (!userInfo.getUserID().equals(TUIRoomEngine.getSelfInfo().userId) && userInfo.getLiveID().equals(mLiveInfo.roomId)) {
                    list.add(userInfo);
                }
            }

            if (!list.isEmpty()
                    || !coGuestStore.getCoGuestState().getApplicants().getValue().isEmpty()
                    || !coGuestStore.getCoGuestState().getInvitees().getValue().isEmpty()) {
                coHostStore.rejectHostConnection(inviter.getLiveID(), null);
            }
        }
    };

    private final HostListener mCoGuestListener = new HostListener() {
        @Override
        public void onGuestApplicationReceived(@NotNull LiveUserInfo guestUser) {
            CoGuestStore coGuestStore = CoGuestStore.create(mLiveInfo.roomId);
            CoHostStore coHostStore = CoHostStore.create(mLiveInfo.roomId);
            if (!coHostStore.getCoHostState().getInvitees().getValue().isEmpty()
                    || !coHostStore.getCoHostState().getConnected().getValue().isEmpty()
                    || coHostStore.getCoHostState().getApplicant().getValue() != null) {
                coGuestStore.rejectApplication(guestUser.getUserID(), null);
            }
        }
    };

    private static final String EVENT_KEY_TIME_LIMIT          = "RTCRoomTimeLimitService";
    private static final String EVENT_SUB_KEY_COUNTDOWN_START = "CountdownStart";
    private static final String EVENT_SUB_KEY_COUNTDOWN_END   = "CountdownEnd";

    public AnchorView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_livestream_anchor_view, this, true);
        mLayoutCoreViewContainer = findViewById(R.id.fl_video_view_container);
        mLayoutComponentsContainer = findViewById(R.id.rl_component_container);
        mLayoutHeaderContainer = findViewById(R.id.fl_header_container);
        mAudienceListView = findViewById(R.id.audience_list_view);
        mImageEndLive = findViewById(R.id.iv_end_live_stream);
        mImageFloatWindow = findViewById(R.id.iv_float_window);
        mViewCoGuest = findViewById(R.id.ll_co_guest);
        mViewCoHost = findViewById(R.id.ll_co_host);
        mViewBattle = findViewById(R.id.ll_battle);
        mBarrageInputView = findViewById(R.id.barrage_input_view);
        mBarrageStreamView = findViewById(R.id.barrage_stream_view);
        mRoomInfoView = findViewById(R.id.room_info_view);
        mNetworkInfoView = findViewById(R.id.network_info_view);
        mApplyCoGuestFloatView = findViewById(R.id.rl_apply_link_audience);
        mGiftPlayView = findViewById(R.id.gift_play_view);

        mLayoutCoreViewContainer.setRadius(ScreenUtil.dip2px(16));
    }

    public void init(TUILiveListManager.LiveInfo liveInfo, LiveCoreView coreView, RoomBehavior behavior,
                     Map<String, Object> params) {
        mBehavior = behavior;
        mLiveInfo = liveInfo;
        mAnchorManager = new AnchorManager(liveInfo);
        mAnchorManager.setLiveStateListener(this);
        initLiveCoreView(coreView);
        super.init(mAnchorManager);
        parseParams(params);
        createVideoMuteBitmap();
        createOrEnterRoom();
        startForegroundService();
    }

    public void unInit() {
        destroy();
        if (mLiveCoreView != null) {
            mLiveCoreView.stopLiveStream((TUILiveListManager.StopLiveCallback) null);
            TUICore.notifyEvent(
                    TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED,
                    TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_STOP, null
            );
            TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_END, null);
        }
    }

    public void addAnchorViewListener(AnchorViewDefine.AnchorViewListener listener) {
        mAnchorManager.addAnchorViewListener(listener);
    }

    public void removeAnchorViewListener(AnchorViewDefine.AnchorViewListener listener) {
        mAnchorManager.removeAnchorViewListener(listener);
    }

    public AnchorViewDefine.AnchorState getState() {
        if (mAnchorManager != null) {
            return mAnchorManager.getExternalState();
        }
        return null;
    }

    /**
     * This API call is called in the {@link Activity#onPictureInPictureModeChanged(boolean)}
     * The code example is as follows:
     * public void onPictureInPictureModeChanged(boolean isInPictureInPictureMode) {
     * super.onPictureInPictureModeChanged(isInPictureInPictureMode);
     * if (mAnchorView != null) {
     * mAnchorView.enablePipMode(isInPictureInPictureMode);
     * }
     * }
     *
     * @param enable true:Turn on picture-in-picture mode; false:Turn off picture-in-picture mode
     */
    public void enablePipMode(boolean enable) {
        mAnchorManager.enablePipMode(enable);

        FrameLayout.LayoutParams layoutParams = (FrameLayout.LayoutParams) mLayoutCoreViewContainer.getLayoutParams();
        if (enable) {
            layoutParams.setMargins(0, 0, 0, 0);
            mLayoutCoreViewContainer.setRadius(ScreenUtil.dip2px(0));
            mLayoutComponentsContainer.setVisibility(GONE);
        } else {
            layoutParams.setMargins(0, ScreenUtil.dip2px(44), 0, ScreenUtil.dip2px(96));
            mLayoutCoreViewContainer.setRadius(ScreenUtil.dip2px(16));
            mLayoutComponentsContainer.setVisibility(VISIBLE);
        }
        mLayoutCoreViewContainer.setLayoutParams(layoutParams);
    }

    public void disableHeaderLiveData(Boolean disable) {
        LOGGER.info("disableHeaderLiveData: disable = " + disable);
        AnchorManager.disableHeaderLiveData(disable);
    }

    public void disableHeaderVisitorCnt(Boolean disable) {
        LOGGER.info("disableHeaderVisitorCnt: disable = " + disable);
        AnchorManager.disableHeaderVisitorCnt(disable);
    }

    public void disableFooterCoGuest(Boolean disable) {
        LOGGER.info("disableFooterCoGuest: disable = " + disable);
        AnchorManager.disableFooterCoGuest(disable);
    }

    public void disableFooterCoHost(Boolean disable) {
        LOGGER.info("disableFooterCoHost: disable = " + disable);
        AnchorManager.disableFooterCoHost(disable);
    }

    public void disableFooterBattle(Boolean disable) {
        LOGGER.info("disableFooterBattle: disable = " + disable);
        AnchorManager.disableFooterBattle(disable);
    }

    public void disableFooterSoundEffect(Boolean disable) {
        LOGGER.info("disableFooterSoundEffect: disable = " + disable);
        AnchorManager.disableFooterSoundEffect(disable);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        destroy();
    }

    @Override
    protected void refreshView() {
    }

    private void showCoGuestManageDialog(SeatFullInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }

        if (mAnchorManagerDialog == null) {
            mAnchorManagerDialog = new AnchorManagerDialog(mContext, mAnchorManager, mLiveCoreView);
        }
        mAnchorManagerDialog.init(userInfo);
        mAnchorManagerDialog.show();
    }

    private void initLiveCoreView(LiveCoreView coreView) {
        if (coreView != null) {
            mLiveCoreView = coreView;
            if (mLiveCoreView.getParent() != null) {
                ((ViewGroup) mLiveCoreView.getParent()).removeView(mLiveCoreView);
            }
        } else {
            mLiveCoreView = new LiveCoreView(getContext(), null, 0, CoreViewType.PUSH_VIEW);
            mLiveCoreView.setLiveId(mLiveInfo.roomId);
        }
        mAnchorManager.setCoreState(mLiveCoreView.getCoreState());
        mLayoutCoreViewContainer.addView(mLiveCoreView);
    }

    private void createVideoMuteBitmap() {
        int bigMuteImageResId =
                Locale.ENGLISH.getLanguage().equals(TUIThemeManager.getInstance().getCurrentLanguage()) ?
                        R.drawable.livekit_local_mute_image_en : R.drawable.livekit_local_mute_image_zh;
        int smallMuteImageResId = R.drawable.livekit_local_mute_image_multi;
        mMediaManager.createVideoMuteBitmap(getContext(), bigMuteImageResId, smallMuteImageResId);
    }

    private void createOrEnterRoom() {
        setComponent();
        mLiveStreamObserver = new AnchorConnectionObserver(mAnchorManager);
        mLiveBattleManagerObserver = new AnchorBattleObserver(mAnchorManager);
        mLiveCoreView.registerConnectionObserver(mLiveStreamObserver);
        mLiveCoreView.registerBattleObserver(mLiveBattleManagerObserver);
        mLiveCoreView.setVideoViewAdapter(new VideoViewAdapter() {
            @Override
            public View createCoGuestView(SeatFullInfo seatInfo, ViewLayer viewLayer) {
                if (TextUtils.isEmpty(seatInfo.userId)) {
                    if (viewLayer == ViewLayer.BACKGROUND) {
                        AnchorEmptySeatView anchorEmptySeatView = new AnchorEmptySeatView(getContext());
                        anchorEmptySeatView.init(mAnchorManager, seatInfo);
                        return anchorEmptySeatView;
                    } else {
                        return null;
                    }
                }
                if (viewLayer == ViewLayer.BACKGROUND) {
                    CoGuestBackgroundWidgetsView backgroundWidgetsView = new CoGuestBackgroundWidgetsView(getContext());
                    backgroundWidgetsView.init(mAnchorManager, seatInfo);
                    return backgroundWidgetsView;
                } else {
                    CoGuestForegroundWidgetsView foregroundWidgetsView = new CoGuestForegroundWidgetsView(getContext());
                    foregroundWidgetsView.init(mAnchorManager, seatInfo);
                    foregroundWidgetsView.setOnClickListener(v -> {
                        showCoGuestManageDialog(seatInfo);
                    });
                    return foregroundWidgetsView;
                }
            }

            @Override
            public View createCoHostView(SeatFullInfo coHostUser,
                                         ViewLayer viewLayer) {
                if (viewLayer == ViewLayer.BACKGROUND) {
                    CoHostBackgroundWidgetsView backgroundWidgetsView = new CoHostBackgroundWidgetsView(mContext);
                    backgroundWidgetsView.init(mAnchorManager, coHostUser);
                    return backgroundWidgetsView;
                } else {
                    CoHostForegroundWidgetsView foregroundWidgetsView = new CoHostForegroundWidgetsView(mContext);
                    foregroundWidgetsView.init(mAnchorManager, coHostUser);
                    return foregroundWidgetsView;
                }
            }

            @Override
            public View createBattleView(TUILiveBattleManager.BattleUser battleUser) {
                BattleMemberInfoView battleMemberInfoView = new BattleMemberInfoView(mContext);
                battleMemberInfoView.init(mAnchorManager, battleUser.userId);
                return battleMemberInfoView;
            }


            @Override
            public View createBattleContainerView() {
                BattleInfoView battleInfoView = new BattleInfoView(mContext);
                battleInfoView.init(mAnchorManager);
                return battleInfoView;
            }
        });

        PictureInPictureStore.sharedInstance().getState().isAnchorStreaming = true;
        if (mBehavior == ENTER_ROOM) {
            if (mRoomState.liveInfo.keepOwnerOnSeat) {
                PermissionRequest.requestCameraPermissions(ContextProvider.getApplicationContext(),
                        new PermissionCallback() {
                            @Override
                            public void onGranted() {
                                LOGGER.info("requestCameraPermissions:[onGranted]");
                                mLiveCoreView.startCamera(true, new TUIRoomDefine.ActionCallback() {
                                    @Override
                                    public void onSuccess() {
                                        LOGGER.info("startCamera success, requestMicrophonePermissions");
                                        PermissionRequest.requestMicrophonePermissions(ContextProvider.getApplicationContext(), new PermissionCallback() {
                                            @Override
                                            public void onGranted() {
                                                LOGGER.info("requestMicrophonePermissions success");
                                                mLiveCoreView.startMicrophone(null);
                                            }

                                            @Override
                                            public void onDenied() {
                                                LOGGER.error("requestCameraPermissions:[onDenied]");
                                            }
                                        });
                                    }

                                    @Override
                                    public void onError(TUICommonDefine.Error error, String message) {
                                        LOGGER.error("startCamera failed:error:" + error + ",message:" + message);
                                    }
                                });
                            }

                            @Override
                            public void onDenied() {
                                LOGGER.error("requestCameraPermissions:[onDenied]");
                            }
                        });
            }

            mLiveCoreView.setLocalVideoMuteImage(mMediaState.bigMuteBitmap, mMediaState.smallMuteBitmap);
            mLiveCoreView.joinLiveStream(mRoomState.roomId, new TUILiveListManager.LiveInfoCallback() {
                @Override
                public void onSuccess(TUILiveListManager.LiveInfo liveInfo) {
                    Activity activity = (Activity) mContext;
                    if (activity.isFinishing() || activity.isDestroyed()) {
                        LOGGER.warn("activity is exit");
                        mLiveCoreView.setVideoViewAdapter(null);
                        if (liveInfo.keepOwnerOnSeat) {
                            mLiveCoreView.stopLiveStream((TUILiveListManager.StopLiveCallback) null);
                        } else {
                            mLiveCoreView.leaveLiveStream(null);
                        }
                        TUICore.notifyEvent(
                                TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED,
                                TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_STOP, null
                        );
                        TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_END, null);
                        mLiveCoreView.setLocalVideoMuteImage(null, null);
                        mLiveCoreView.unregisterConnectionObserver(mLiveStreamObserver);
                        mLiveCoreView.unregisterBattleObserver(mLiveBattleManagerObserver);
                        PictureInPictureStore.sharedInstance().getState().isAnchorStreaming = false;
                        return;
                    }
                    mAnchorManager.getRoomManager().updateRoomState(liveInfo);
                    mUserManager.getAudienceList();
                    initComponentView();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    PictureInPictureStore.sharedInstance().getState().isAnchorStreaming = false;
                    ErrorLocalized.onError(error);
                    finishActivity();
                }
            });
        } else {
            mLiveInfo.keepOwnerOnSeat = true;
            mLiveInfo.isSeatEnabled = true;
            mLiveInfo.seatMode = TUIRoomDefine.SeatMode.APPLY_TO_TAKE;
            mLiveCoreView.setLocalVideoMuteImage(mMediaState.bigMuteBitmap, mMediaState.smallMuteBitmap);
            mLiveCoreView.startLiveStream(mLiveInfo, new TUILiveListManager.LiveInfoCallback() {
                @Override
                public void onSuccess(TUILiveListManager.LiveInfo liveInfo) {
                    Activity activity = (Activity) mContext;
                    if (activity.isFinishing() || activity.isDestroyed()) {
                        LOGGER.warn("activity is exit, stopLiveStream");
                        mLiveCoreView.stopLiveStream((TUIRoomDefine.ActionCallback) null);
                        TUICore.notifyEvent(
                                TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED,
                                TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_STOP, null
                        );
                        TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_END, null);
                        mLiveCoreView.setLocalVideoMuteImage(null, null);
                        mLiveCoreView.unregisterConnectionObserver(mLiveStreamObserver);
                        mLiveCoreView.unregisterBattleObserver(mLiveBattleManagerObserver);
                        return;
                    }
                    mAnchorManager.getRoomManager().updateRoomState(liveInfo);
                    mUserManager.getAudienceList();
                    initComponentView();
                    showAlertUserLiveTips();
                    TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_START, null);
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LOGGER.error("startLiveStream failed:error:" + error + ",errorCode:" + error.getValue() +
                            ",message:" + message);
                    PictureInPictureStore.sharedInstance().getState().isAnchorStreaming = false;
                    ErrorLocalized.onError(error);
                    if (error == TUICommonDefine.Error.SDK_NOT_INITIALIZED) {
                        finishActivity();
                    }
                }
            });
        }
    }

    private void setComponent() {
        try {
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "component");
            jsonObject.put("component", 21);
            LiveCoreView.callExperimentalAPI(jsonObject.toString());
        } catch (JSONException e) {
            LOGGER.error("dataReport:" + Log.getStackTraceString(e));
        }
    }

    private void initComponentView() {
        initRoomInfoView();
        initAudienceListView();
        initNetworkView();
        initEndLiveStreamView();
        initFloatWindowView();
        initBarrageInputView();
        initBarrageStreamView();
        initCoGuestView();
        initCoHostView();
        initBattleView();
        initSettingsPanel();
        initApplyCoGuestFloatView();
        initGiftPlayView();
    }

    private void initNetworkView() {
        mNetworkInfoView.init(mRoomState.liveInfo.createTime);
    }

    private void initSettingsPanel() {
        findViewById(R.id.ll_more).setOnClickListener(view -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            SettingsPanelDialog settingsPanelDialog = new SettingsPanelDialog(mContext, mAnchorManager, mLiveCoreView);
            settingsPanelDialog.setOnDismissListener(dialog -> view.setEnabled(true));
            settingsPanelDialog.show();
        });
    }

    private void showAlertUserLiveTips() {
        try {
            Map<String, Object> map = new HashMap<>();
            map.put(TUIConstants.Privacy.PARAM_DIALOG_CONTEXT, Objects.requireNonNull(getContext()));
            TUICore.notifyEvent(TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED,
                    TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_START, map);
        } catch (Exception e) {
            LOGGER.error("showAlertUserLiveTips exception:" + e.getMessage());
        }
    }

    private void initAudienceListView() {
        mAudienceListView.init(mRoomState.liveInfo);
        mAudienceListView.setOnUserItemClickListener(userInfo -> {
            if (mUserManagerDialog == null) {
                mUserManagerDialog = new UserManagerDialog(mContext, mAnchorManager);
            }
            mUserManagerDialog.init(userInfo);
            mUserManagerDialog.show();
        });
    }

    private void initEndLiveStreamView() {
        mImageEndLive.setOnClickListener(v -> showLiveStreamEndDialog());
    }

    private void initFloatWindowView() {
        mImageFloatWindow.setOnClickListener(v -> {
            if (mAnchorManager != null) {
                mAnchorManager.notifyPictureInPictureClick();
            }
        });
    }

    private void initRoomInfoView() {
        mRoomInfoView.init(mRoomState.liveInfo);
    }

    private void initBarrageInputView() {
        mBarrageInputView.init(mRoomState.roomId);
    }

    private void initBarrageStreamView() {
        mBarrageStreamView.init(mRoomState.roomId, mCoreState.roomState.ownerInfo.getValue().userId);
        mBarrageStreamView.setItemTypeDelegate(new BarrageViewTypeDelegate());
        mBarrageStreamView.setItemAdapter(GIFT_VIEW_TYPE_1, new GiftBarrageAdapter(mContext));
        mBarrageStreamView.setOnMessageClickListener(userInfo -> {
            if (TextUtils.isEmpty(userInfo.userId)) {
                return;
            }
            if (userInfo.userId.equals(mCoreState.userState.selfInfo.getValue().userId)) {
                return;
            }

            if (mUserManagerDialog == null) {
                mUserManagerDialog = new UserManagerDialog(mContext, mAnchorManager);
            }
            mUserManagerDialog.init(userInfo);
            mUserManagerDialog.show();
        });
    }

    private void initCoGuestView() {
        mViewCoGuest.setOnClickListener((view) -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            AnchorCoGuestManageDialog dialog = new AnchorCoGuestManageDialog(mContext, mAnchorManager, mLiveCoreView);
            dialog.setOnDismissListener(dialog1 -> view.setEnabled(true));
            dialog.show();
        });
    }

    private void initCoHostView() {
        mViewCoHost.setOnClickListener((view) -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            AnchorCoHostManageDialog dialog = new AnchorCoHostManageDialog(mContext, mAnchorManager, mLiveCoreView);
            dialog.setOnDismissListener(dialog1 -> view.setEnabled(true));
            dialog.show();
        });
    }

    private void initBattleView() {
        mViewBattle.setOnClickListener(view -> {
            if (Boolean.TRUE.equals(mBattleState.mIsBattleRunning.getValue()) && mBattleManager.isSelfInBattle()) {
                if (mAnchorEndBattleDialog == null) {
                    mAnchorEndBattleDialog = new AnchorEndBattleDialog(mContext, mAnchorManager, mLiveCoreView);
                }
                mAnchorEndBattleDialog.show();
            } else {
                if (Boolean.TRUE.equals(mBattleState.mIsOnDisplayResult.getValue()) || !mCoHostManager.isSelfInCoHost()) {
                    LOGGER.warn("can not requestBattle");
                    return;
                }
                List<String> list = new ArrayList<>();
                String selfId = mCoreState.userState.selfInfo.getValue().userId;
                for (ConnectionUser user : mCoreState.coHostState.connectedUserList.getValue()) {
                    if (!user.userId.equals(selfId)) {
                        list.add(user.userId);
                    }
                }

                TUILiveBattleManager.BattleConfig battleConfig = new TUILiveBattleManager.BattleConfig();
                battleConfig.duration = BATTLE_DURATION;
                battleConfig.needResponse = true;
                battleConfig.extensionInfo = "";
                mLiveCoreView.requestBattle(battleConfig, list, BATTLE_REQUEST_TIMEOUT,
                        new LiveCoreViewDefine.BattleRequestCallback() {
                            @Override
                            public void onSuccess(String battleId, List<String> requestedUserIdList) {
                                mAnchorManager.getBattleManager().onRequestBattle(battleId, requestedUserIdList);
                            }

                            @Override
                            public void onError(TUICommonDefine.Error error, String message) {
                                LOGGER.error("requestBattle failed:error:" + error + ",errorCode:" + error.getValue() +
                                        "message:" + message);
                                ErrorLocalized.onError(error);
                            }
                        });
            }
        });
    }

    private void initApplyCoGuestFloatView() {
        mApplyCoGuestFloatView.init(mAnchorManager, mLiveCoreView);
    }

    private void initGiftPlayView() {
        GiftCacheService giftCacheService = GiftStore.getInstance().mGiftCacheService;
        mGiftPlayView.setListener(new GiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(GiftPlayView view, @NonNull Gift gift, int giftCount,
                                      @NonNull LiveUserInfo sender) {
                if (mBarrageStreamView == null) {
                    return;
                }

                Barrage barrage = new Barrage();
                barrage.setTextContent("gift");
                barrage.getSender().setUserID(sender.getUserID());
                barrage.getSender().setUserName(TextUtils.isEmpty(sender.getUserName()) ? sender.getUserID() :
                        sender.getUserName());
                barrage.getSender().setAvatarURL(sender.getAvatarURL());
                Map<String, String> extInfo = new HashMap<>();
                extInfo.put(GIFT_VIEW_TYPE, String.valueOf(GIFT_VIEW_TYPE_1));
                extInfo.put(GIFT_NAME, gift.getName());
                extInfo.put(GIFT_COUNT, String.valueOf(giftCount));
                extInfo.put(GIFT_ICON_URL, gift.getIconURL());
                extInfo.put(GIFT_RECEIVER_USERNAME, getContext().getString(R.string.common_gift_me));
                barrage.setExtensionInfo(extInfo);
                mBarrageStreamView.insertBarrages(barrage);
            }

            @Override
            public void onPlayGiftAnimation(GiftPlayView view, @NonNull Gift gift) {
                giftCacheService.request(gift.getResourceURL(), (error, result) -> {
                    if (error == 0) {
                        view.playGiftAnimation(result);
                    }
                });
            }
        });
        mGiftPlayView.init(mRoomState.roomId);
    }

    @Override
    protected void addObserver() {
        AnchorConfig.disableHeaderLiveData.observeForever(mDisableLiveDataObserver);
        AnchorConfig.disableHeaderVisitorCnt.observeForever(mDisableVisitorCntObserver);
        AnchorConfig.disableFooterCoGuest.observeForever(mDisableCoGuestObserver);
        AnchorConfig.disableFooterCoHost.observeForever(mDisableCoHostObserver);
        AnchorConfig.disableFooterBattle.observeForever(mDisableBattleObserver);

        mCoreState.coHostState.connectedUserList.observeForever(mConnectedObserver);
        mBattleState.mBattledUsers.observeForever(mBattleUserObserver);
        mCoreState.coHostState.receivedConnectionRequest.observeForever(mReceivedConnectRequestObserver);
        mBattleState.mReceivedBattleRequest.observeForever(mReceivedBattleRequestObserver);
        mBattleState.mIsBattleRunning.observeForever(mBattleStartObserver);
        mBattleState.mIsInWaiting.observeForever(mInWaitingObserver);
        mBattleState.mIsOnDisplayResult.observeForever(mBattleResultDisplayObserver);
        mCoreState.coGuestState.connectedUserList.observeForever(mCoGuestUserObserver);
        mMediaState.isPipModeEnabled.observeForever(mPipModeObserver);
        CoHostStore.create(mLiveInfo.roomId).addCoHostListener(mCoHostListener);
        CoGuestStore.create(mLiveInfo.roomId).addHostListener(mCoGuestListener);
    }

    @Override
    protected void removeObserver() {
        AnchorConfig.disableHeaderLiveData.removeObserver(mDisableLiveDataObserver);
        AnchorConfig.disableHeaderVisitorCnt.removeObserver(mDisableVisitorCntObserver);
        AnchorConfig.disableFooterCoGuest.removeObserver(mDisableCoGuestObserver);
        AnchorConfig.disableFooterCoHost.removeObserver(mDisableCoHostObserver);
        AnchorConfig.disableFooterBattle.removeObserver(mDisableBattleObserver);

        mCoreState.coHostState.connectedUserList.removeObserver(mConnectedObserver);
        mBattleState.mBattledUsers.removeObserver(mBattleUserObserver);
        mCoreState.coHostState.receivedConnectionRequest.removeObserver(mReceivedConnectRequestObserver);
        mBattleState.mReceivedBattleRequest.removeObserver(mReceivedBattleRequestObserver);
        mBattleState.mIsBattleRunning.removeObserver(mBattleStartObserver);
        mBattleState.mIsInWaiting.removeObserver(mInWaitingObserver);
        mBattleState.mIsOnDisplayResult.removeObserver(mBattleResultDisplayObserver);
        mCoreState.coGuestState.connectedUserList.removeObserver(mCoGuestUserObserver);
        mMediaState.isPipModeEnabled.removeObserver(mPipModeObserver);
        CoHostStore.create(mLiveInfo.roomId).removeCoHostListener(mCoHostListener);
        CoGuestStore.create(mLiveInfo.roomId).removeHostListener(mCoGuestListener);
    }

    private void showLiveStreamEndDialog() {
        EndLiveStreamDialog endLiveStreamDialog = new EndLiveStreamDialog(mContext, mLiveCoreView, mAnchorManager,
                this);
        endLiveStreamDialog.show();
    }

    private void onReceivedCoHostRequest(ConnectionUser receivedConnectionRequest) {
        if (mMediaState.isPipModeEnabled.getValue()) {
            return;
        }
        if (receivedConnectionRequest == null) {
            if (mProcessConnectionDialog != null) {
                mProcessConnectionDialog.dismiss();
            }
            return;
        }
        String content =
                receivedConnectionRequest.userName + getContext().getString(R.string.common_connect_inviting_append);
        showConnectionRequestDialog(content, receivedConnectionRequest.avatarUrl, receivedConnectionRequest.roomId);
    }

    private void showConnectionRequestDialog(String content, String avatarUrl, String roomId) {
        mProcessConnectionDialog = new StandardDialog(getContext());
        mProcessConnectionDialog.setContent(content);
        mProcessConnectionDialog.setAvatar(avatarUrl);

        String rejectText = getContext().getString(R.string.common_reject);
        mProcessConnectionDialog.setNegativeText(rejectText, negativeView -> {
            mLiveCoreView.respondToCrossRoomConnection(roomId, false, null);
            mProcessConnectionDialog.dismiss();
        });

        String receiveText = getContext().getString(R.string.common_receive);
        mProcessConnectionDialog.setPositiveText(receiveText, positiveView -> {
            mLiveCoreView.respondToCrossRoomConnection(roomId, true, null);
            mProcessConnectionDialog.dismiss();
        });
        mProcessConnectionDialog.show();
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onConnectedUserChange(List<ConnectionUser> connectedList) {
        post(() -> {
            updateBattleView();
            enableView(mViewCoGuest, connectedList.isEmpty());
        });
    }

    private void onBattleUserChange(List<BattleUser> userList) {
        post(this::updateBattleView);
    }

    private void onCoGuestUserChange(List<UserInfo> seatList) {
        post(() -> {
            enableView(mViewCoHost, seatList.size() <= 1);
            CoGuestIconView coGuestIconView = findViewById(R.id.co_guest_icon);
            if (coGuestIconView != null) {
                if (seatList.size() > 1) {
                    coGuestIconView.startAnimation();
                } else {
                    coGuestIconView.stopAnimation();
                }
            }
        });
    }

    private void onReceivedBattleRequestChange(BattleUser user) {
        if (mMediaState.isPipModeEnabled.getValue()) {
            return;
        }
        if (mProcessBattleDialog != null) {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
        }
        if (user == null) {
            return;
        }
        String content = user.userName + " " + getContext().getString(R.string.common_battle_inviting);
        mProcessBattleDialog = new StandardDialog(getContext());
        mProcessBattleDialog.setContent(content);
        mProcessBattleDialog.setAvatar(user.avatarUrl);

        String rejectText = getContext().getString(R.string.common_reject);
        mProcessBattleDialog.setNegativeText(rejectText, negativeView -> {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
            mLiveCoreView.respondToBattle(mBattleState.mBattleId, false, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mAnchorManager.getBattleManager().onResponseBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LOGGER.error("respondToBattle failed:error:" + error + ",errorCode:" + error.getValue() +
                            "message:" + message);
                    ErrorLocalized.onError(error);
                }
            });
        });

        String receiveText = getContext().getString(R.string.common_receive);
        mProcessBattleDialog.setPositiveText(receiveText, positiveView -> {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
            mLiveCoreView.respondToBattle(mBattleState.mBattleId, true, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mAnchorManager.getBattleManager().onResponseBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LOGGER.error("respondToBattle failed:error:" + error + ",errorCode:" + error.getValue() +
                            "message:" + message);
                    ErrorLocalized.onError(error);
                }
            });
        });
        mProcessBattleDialog.show();
    }

    private void showBattleCountdownDialog() {
        if (mBattleCountdownDialog == null) {
            mBattleCountdownDialog = new BattleCountdownDialog(mContext, mAnchorManager, mLiveCoreView);
        }
        mBattleCountdownDialog.show();
    }

    private void dismissBattleCountdownDialog() {
        if (mBattleCountdownDialog != null) {
            mBattleCountdownDialog.dismiss();
            mBattleCountdownDialog = null;
        }
    }

    private void finishActivity() {
        if (mContext instanceof Activity activity) {
            Intent intent = new Intent();
            activity.setResult(RESULT_OK, intent);
            activity.finishAndRemoveTask();
        }
    }

    private void onInWaitingChange(Boolean inWaiting) {
        if (Boolean.TRUE.equals(inWaiting)) {
            showBattleCountdownDialog();
        } else if (Boolean.FALSE.equals(inWaiting)) {
            dismissBattleCountdownDialog();
        }
    }

    private void onBattleStartChange(Boolean started) {
        if (Boolean.TRUE.equals(started)) {
            for (BattleUser user : mBattleState.mBattledUsers.getValue()) {
                if (TextUtils.equals(mCoreState.userState.selfInfo.getValue().userId, user.userId)) {
                    enableView(mViewCoHost, false);
                    break;
                }
            }
        } else if (Boolean.FALSE.equals(started)) {
            enableView(mViewCoHost, true);
            if (mAnchorEndBattleDialog != null && mAnchorEndBattleDialog.isShowing()) {
                mAnchorEndBattleDialog.dismiss();
            }
        }
    }

    private void onBattleResultDisplay(Boolean onDisplay) {
        post(this::updateBattleView);
    }

    private void updateBattleView() {
        View battleIconView = mViewBattle.findViewById(R.id.v_battle_icon);
        Boolean battleResultDisplay = mBattleState.mIsOnDisplayResult.getValue();
        if (mCoHostManager.isSelfInCoHost()) {
            if (mBattleManager.isSelfInBattle()) {
                battleIconView.setBackgroundResource(R.drawable.livekit_function_battle_exit);
            } else {
                battleIconView.setBackgroundResource(R.drawable.livekit_function_battle);
            }
            if (Boolean.TRUE.equals(battleResultDisplay)) {
                battleIconView.setBackgroundResource(R.drawable.livekit_function_battle_disable);
            } else if (Boolean.FALSE.equals(battleResultDisplay)) {
                battleIconView.setBackgroundResource(R.drawable.livekit_function_battle);
            }
        } else {
            battleIconView.setBackgroundResource(R.drawable.livekit_function_battle_disable);
        }
    }

    private void enableView(View view, boolean enable) {
        view.setEnabled(enable);
        view.setAlpha(enable ? 1.0f : 0.5f);
    }

    private void parseParams(Map<String, Object> params) {
        if (params == null) {
            return;
        }
        if (params.containsKey("coHostTemplateId") && params.get("coHostTemplateId") != null) {
            Object coHostTemplateId = params.get("coHostTemplateId");
            if (coHostTemplateId instanceof Integer) {
                mAnchorManager.getCoHostManager().setCoHostTemplateId((int) coHostTemplateId);
            }
        }
    }

    private void destroy() {
        if (mIsDestroy) {
            return;
        }
        mIsDestroy = true;
        mLiveCoreView.stopCamera();
        mLiveCoreView.stopMicrophone();
        BeautyUtils.resetBeauty();
        TEBeautyStore.unInit();
        mAnchorManager.destroy();
        stopForegroundService();
    }

    private void onDisableLiveDataChange(Boolean disable) {
        if (Boolean.TRUE.equals(disable)) {
            mLayoutHeaderContainer.setVisibility(GONE);
        } else {
            mLayoutHeaderContainer.setVisibility(VISIBLE);
        }
    }

    private void onDisableVisitorCntChange(Boolean disable) {
        if (Boolean.TRUE.equals(disable)) {
            mAudienceListView.setVisibility(GONE);
        } else {
            mAudienceListView.setVisibility(VISIBLE);
        }
    }

    private void ondDisableCoGuestChange(Boolean disable) {
        if (Boolean.TRUE.equals(disable)) {
            mViewCoGuest.setVisibility(GONE);
        } else {
            mViewCoGuest.setVisibility(VISIBLE);
        }
    }

    private void onDisableCoHostChange(Boolean disable) {
        if (Boolean.TRUE.equals(disable)) {
            mViewCoHost.setVisibility(GONE);
        } else {
            mViewCoHost.setVisibility(VISIBLE);
        }
    }

    private void onDisableBattleChange(Boolean disable) {
        if (Boolean.TRUE.equals(disable)) {
            mViewBattle.setVisibility(GONE);
        } else {
            mViewBattle.setVisibility(VISIBLE);
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
    public void onRoomExit() {
        onEndLive();
    }

    @Override
    public void onRoomExitEndStatistics() {
        mAnchorManager.setExternalState(mBarrageStreamView != null ? mBarrageStreamView.getBarrageCount() : 0);
        PictureInPictureStore.sharedInstance().getState().isAnchorStreaming = false;
    }

    private void onPipModeObserver(Boolean isPipMode) {
        if (Boolean.FALSE.equals(isPipMode)) {
            onReceivedCoHostRequest(mLiveCoreView.getCoreState().coHostState.receivedConnectionRequest.getValue());
            onReceivedBattleRequestChange(mBattleState.mReceivedBattleRequest.getValue());
            checkCameraStateAndRestore();
        }
    }

    private void checkCameraStateAndRestore() {
        if (mMediaState.isCameraOccupied && mLiveCoreView != null
                && mLiveCoreView.getCoreState().mediaState.isCameraOpened.getValue()) {
            boolean isFrontCamera = mLiveCoreView.getCoreState().mediaState.isFrontCamera.getValue();
            mLiveCoreView.stopCamera();
            postDelayed(() -> mLiveCoreView.startCamera(isFrontCamera, null), 500);
        }
        mAnchorManager.getMediaManager().resetCameraOccupied();
    }

    @Override
    public void onRoomDismissed() {
        ToastUtil.toastShortMessage(mContext.getString(R.string.common_live_has_stop));
        onEndLive();
    }

    @Override
    public void onKickedOffLine(String message) {
        ToastUtil.toastShortMessage(mContext.getString(R.string.common_kicked_out_of_room_by_owner));
        onEndLive();
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        ToastUtil.toastShortMessage(mContext.getString(R.string.common_kicked_out_of_room_by_owner));
        onEndLive();
    }

    private void onEndLive() {
        PictureInPictureStore.sharedInstance().getState().isAnchorStreaming = false;
        if (mLiveCoreView != null) {
            mLiveCoreView.setLocalVideoMuteImage(null, null);
            mLiveCoreView.stopLiveStream((TUILiveListManager.StopLiveCallback) null);
            TUICore.notifyEvent(
                    TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED,
                    TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_STOP, null
            );
            TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_END, null);
        }
        finishActivity();
    }
}
