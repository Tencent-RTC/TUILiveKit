package com.trtc.uikit.livekit.livestream.view.anchor;

import static com.trtc.uikit.livekit.component.gift.access.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.gift.access.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.gift.access.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.gift.access.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.gift.access.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.gift.access.service.GiftConstants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_USER_INFO;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomInfoCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.component.barrage.BarrageInputView;
import com.trtc.uikit.component.barrage.BarrageStreamView;
import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.Constants;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.utils.LiveCoreLogger;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.component.gift.GiftPlayView;
import com.trtc.uikit.livekit.component.gift.access.service.GiftCacheService;
import com.trtc.uikit.livekit.component.gift.access.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.access.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.gift.access.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.manager.module.DashboardManager;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveBattleManagerObserver;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveStreamObserver;
import com.trtc.uikit.livekit.livestream.state.BattleState;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionUser;
import com.trtc.uikit.livekit.livestream.state.MediaState;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus;
import com.trtc.uikit.livekit.livestream.state.UserState;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestream.view.VideoLiveKitImpl;
import com.trtc.uikit.livekit.livestream.view.anchor.dashboard.AnchorDashboardView;
import com.trtc.uikit.livekit.livestream.view.anchor.preview.LiveInfoEditView;
import com.trtc.uikit.livekit.livestream.view.anchor.preview.PreviewFunctionView;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.EndLiveStreamDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle.AnchorEndBattleDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle.BattleCountdownDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest.AnchorCoGuestManageDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest.AnchorManagerDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest.ApplyCoGuestFloatView;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost.AnchorCoHostManageDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost.StandardDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.settings.SettingsPanelDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.usermanage.UserManagerDialog;
import com.trtc.uikit.livekit.livestream.view.widgets.battle.BattleInfoView;
import com.trtc.uikit.livekit.livestream.view.widgets.battle.BattleMemberInfoView;
import com.trtc.uikit.livekit.livestream.view.widgets.coguest.CoGuestWidgetsView;
import com.trtc.uikit.livekit.livestream.view.widgets.cohost.CoHostWidgetsView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class AnchorView extends BasicView implements ITUINotification {
    private static final String TAG = "AnchorView";

    private       LiveCoreView                           mLiveCoreView;
    private       FrameLayout                            mLayoutPreview;
    private       FrameLayout                            mLayoutPushing;
    private       AnchorDashboardView                    mAnchorDashboardView;
    private       View                                   mBackView;
    private       LiveInfoEditView                       mLiveInfoEditView;
    private       PreviewFunctionView                    mPreviewFunctionView;
    private       RelativeLayout                         mLayoutAnchorPreviewMask;
    private       RelativeLayout                         mLayoutAnchorLivingTopMask;
    private       RelativeLayout                         mLayoutAnchorLivingBottomMask;
    private       Button                                 mButtonStartLive;
    private       ImageView                              mImageEndLive;
    private       ImageView                              mImageFloatWindow;
    private       View                                   mViewCoGuest;
    private       View                                   mViewCoHost;
    private       View                                   mViewBattle;
    private       AudienceListView                       mAudienceListView;
    private       RoomInfoView                           mRoomInfoView;
    private       BarrageInputView                       mBarrageInputView;
    private       BarrageStreamView                      mBarrageStreamView;
    private       GiftPlayView                           mGiftPlayView;
    private       ApplyCoGuestFloatView                  mApplyCoGuestFloatView;
    private       StandardDialog                         mProcessConnectionDialog;
    private       StandardDialog                         mProcessBattleDialog;
    private       BattleCountdownDialog                  mBattleCountdownDialog;
    private       AnchorManagerDialog                    mAnchorManagerDialog;
    private       UserManagerDialog                      mUserManagerDialog;
    private       TUILiveRoomAnchorFragment.RoomBehavior mRoomBehavior;
    private       boolean                                mIsExit                         = false;
    private       boolean                                mUseCachedCoreView;
    private final Observer<LiveStatus>                   mLiveStatusObserver             =
            this::onLiveStatusChange;
    private final Observer<ConnectionUser>               mReceivedConnectRequestObserver =
            this::onReceivedCoHostRequest;
    private final Observer<List<ConnectionUser>>         mConnectedObserver              =
            this::onConnectedUserChange;
    private final Observer<List<CoGuestState.SeatInfo>>  mCoGuestUserObserver            =
            this::onCoGuestUserChange;
    private final Observer<Boolean>                      mBattleStartObserver            =
            this::onBattleStartChange;
    private final Observer<Boolean>                      mBattleResultDisplayObserver    =
            this::onBattleResultDisplay;
    private final Observer<UserState.UserInfo>           mEnterUserObserver              =
            this::onEnterUserChange;
    private final Observer<BattleState.BattleUser>       mReceivedBattleRequestObserver  =
            this::onReceivedBattleRequestChange;
    private final Observer<Boolean>                      mInWaitingObserver              =
            this::onInWaitingChange;

    public AnchorView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
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
        showLiveStreamEndDialog();
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
        LayoutInflater.from(mContext).inflate(R.layout.livekit_livestream_anchor_view, this, true);
        mLayoutPreview = findViewById(R.id.fl_preview);
        mLayoutPushing = findViewById(R.id.fl_pushing);
        mAnchorDashboardView = findViewById(R.id.anchor_dashboard_view);
        mBackView = findViewById(R.id.iv_back);
        mLiveInfoEditView = findViewById(R.id.rl_live_info_edit_view);
        mPreviewFunctionView = findViewById(R.id.fl_preview_function);
        mLayoutAnchorPreviewMask = findViewById(R.id.rl_anchor_preview_mask);
        mLayoutAnchorLivingTopMask = findViewById(R.id.rl_anchor_living_top_mask);
        mLayoutAnchorLivingBottomMask = findViewById(R.id.rl_anchor_living_bottom_mask);
        mAudienceListView = findViewById(R.id.audience_list_view);
        mImageEndLive = findViewById(R.id.iv_end_live_stream);
        mImageFloatWindow = findViewById(R.id.iv_float_window);
        mButtonStartLive = findViewById(R.id.btn_start_live);
        mViewCoGuest = findViewById(R.id.v_co_guest);
        mViewCoHost = findViewById(R.id.v_co_host);
        mViewBattle = findViewById(R.id.v_battle);
        mBarrageInputView = findViewById(R.id.barrage_input_view);
        mBarrageStreamView = findViewById(R.id.barrage_stream_view);
        mRoomInfoView = findViewById(R.id.room_info_view);
        mApplyCoGuestFloatView = findViewById(R.id.rl_apply_link_audience);
        mGiftPlayView = findViewById(R.id.gift_play_view);

        mLiveCoreView = FloatWindowManager.getInstance().getCoreView();
        if (mLiveCoreView == null) {
            mLiveCoreView = new LiveCoreView(ContextProvider.getApplicationContext());
        } else {
            mUseCachedCoreView = true;
            FloatWindowManager.getInstance().setCoreView(null);
        }
        FrameLayout frameLayout = findViewById(R.id.lsv_video_view_container);
        frameLayout.addView(mLiveCoreView);
    }

    @Override
    protected void refreshView() {
        initBackView();
        initLiveCoreView();
        initComponentView();
    }

    private void initBackView() {
        mBackView.setOnClickListener(v -> {
            mIsExit = true;
            ((Activity) getContext()).finish();
        });
    }

    private void showCoGuestManageDialog(UserInfo userInfo) {
        if (mRoomState.liveStatus.getValue() == RoomState.LiveStatus.PREVIEWING) {
            return;
        }

        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }

        if (mAnchorManagerDialog == null) {
            mAnchorManagerDialog = new AnchorManagerDialog(mContext, mLiveManager, mLiveCoreView);
        }
        mAnchorManagerDialog.init(userInfo);
        mAnchorManagerDialog.show();
    }

    private void initLiveCoreView() {
        setComponent();
        if (!mUseCachedCoreView) {
            mLiveCoreView.registerConnectionObserver(new LiveStreamObserver(mLiveManager));
            mLiveCoreView.registerBattleObserver(new LiveBattleManagerObserver(mLiveManager));
        }
        mLiveCoreView.setVideoViewAdapter(new LiveCoreViewDefine.VideoViewAdapter() {
            @Override
            public View createCoGuestView(UserInfo userInfo) {
                CoGuestWidgetsView coGuestWidgetsView = new CoGuestWidgetsView(getContext());
                coGuestWidgetsView.init(mLiveManager, userInfo);
                coGuestWidgetsView.setOnClickListener(v -> {
                    Map<String, Object> params = new HashMap<>();
                    params.put(EVENT_PARAMS_KEY_USER_INFO, userInfo);
                    TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW, params);
                });
                return coGuestWidgetsView;
            }

            @Override
            public void updateCoGuestView(View coGuestView, UserInfo userInfo,
                                          List<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlag) {
                LiveCoreLogger.info("Anchor initLiveCoreView updateCoGuestView: userInfo = " + new Gson().toJson(userInfo)
                        + ",modifyFlag = " + new Gson().toJson(modifyFlag) + ",coGuestView = " + coGuestView);
            }

            @Override
            public View createCoHostView(LiveCoreViewDefine.CoHostUser coHostUser) {
                CoHostWidgetsView coHostWidgetsView = new CoHostWidgetsView(mContext);
                coHostWidgetsView.init(mLiveManager, coHostUser);
                return coHostWidgetsView;
            }

            @Override
            public void updateCoHostView(View coHostView, LiveCoreViewDefine.CoHostUser coHostUser,
                                         List<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlag) {
                LiveCoreLogger.info("Anchor initLiveCoreView updateCoHostView: coHostUser = " + new Gson().toJson(coHostUser)
                        + ",modifyFlag = " + new Gson().toJson(modifyFlag) + ",coHostView = " + coHostView);
            }

            @Override
            public View createBattleView(TUILiveBattleManager.BattleUser battleUser) {
                BattleMemberInfoView battleMemberInfoView = new BattleMemberInfoView(mContext);
                battleMemberInfoView.init(mLiveManager, battleUser.userId);
                return battleMemberInfoView;
            }

            @Override
            public void updateBattleView(View battleView, TUILiveBattleManager.BattleUser battleUser) {
            }

            @Override
            public View createBattleContainerView() {
                BattleInfoView battleInfoView = new BattleInfoView(mContext);
                battleInfoView.init(mLiveManager);
                return battleInfoView;
            }

            @Override
            public void updateBattleContainerView(View battleContainnerView,
                                                  List<LiveCoreViewDefine.BattleUserViewModel> userInfos) {
                BattleInfoView battleInfoView = (BattleInfoView) battleContainnerView;
                battleInfoView.updateView(userInfos);
            }
        });

        boolean isFrontCamera = Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isFrontCamera.getValue());
        mLiveCoreView.startCamera(isFrontCamera, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mLiveCoreView.startMicrophone(null);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                mLiveCoreView.startMicrophone(null);
            }
        });
        if (mRoomBehavior == TUILiveRoomAnchorFragment.RoomBehavior.ENTER_ROOM) {
            mLayoutPushing.setVisibility(VISIBLE);
            mUserManager.initSelfUserData();
            mLiveCoreView.joinLiveStream(mRoomState.roomId, new GetRoomInfoCallback() {
                @Override
                public void onSuccess(RoomInfo roomInfo) {
                    mLiveManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PUSHING);
                    mLiveManager.getRoomManager().updateRoomState(roomInfo);
                    mLiveManager.getRoomManager().updateLiveInfo();
                    mUserManager.getAudienceList();
                    mUserManager.updateOwnerUserInfo();
                    mCoGuestManager.getSeatList();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorLocalized.onError(error);
                    removeAllViews();
                    if (mContext instanceof Activity) {
                        ((Activity) mContext).finish();
                    }
                }
            });
        }
    }

    private void setComponent() {
        try {
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "component");
            jsonObject.put("component", Constants.DATA_REPORT_COMPONENT_LIVE_ROOM);
            LiveCoreView.callExperimentalAPI(jsonObject.toString());
        } catch (JSONException e) {
            LiveCoreLogger.error("dataReport:" + e);
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
        dashboardManager.updateLikeNumber(mGiftPlayView.getLikeCount());
        dashboardManager.updateMessageCount(mBarrageStreamView.getBarrageCount());
        mLayoutPushing.removeAllViews();
        mLayoutPreview.removeAllViews();
        mAnchorDashboardView.setVisibility(VISIBLE);
        mAnchorDashboardView.init(mLiveManager);
    }

    private void initPreviewStatus() {
        mLayoutPushing.setVisibility(GONE);
        mAnchorDashboardView.setVisibility(GONE);
        mLayoutPreview.setVisibility(VISIBLE);

        initStartLiveView();
        initLiveInfoEditView();
        initPreviewFunctionView();
    }

    private void initPushingStatus() {
        mLayoutPreview.setVisibility(GONE);
        mAnchorDashboardView.setVisibility(GONE);
        mLayoutPushing.setVisibility(VISIBLE);

        initRoomInfoView();
        initAudienceListView();
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

    private void initSettingsPanel() {
        findViewById(R.id.v_settings).setOnClickListener(view -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            SettingsPanelDialog settingsPanelDialog = new SettingsPanelDialog(mContext, mLiveManager, mLiveCoreView);
            settingsPanelDialog.setOnDismissListener(dialog -> view.setEnabled(true));
            settingsPanelDialog.show();
        });
    }

    private void initStartLiveView() {
        mButtonStartLive.setOnClickListener((View view) -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            RoomInfo roomInfo = new RoomInfo();
            roomInfo.roomId = mRoomState.roomId;
            roomInfo.name = mRoomState.roomName.getValue();
            roomInfo.maxSeatCount = 9;
            mUserManager.initSelfUserData();
            mLayoutPreview.setVisibility(GONE);
            mLayoutPushing.setVisibility(VISIBLE);
            mLiveCoreView.startLiveStream(roomInfo, new GetRoomInfoCallback() {
                @Override
                public void onSuccess(RoomInfo roomInfo) {
                    if (mIsExit) {
                        mLiveCoreView.stopLiveStream(null);
                        return;
                    }
                    mLiveManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PUSHING);
                    mLiveManager.getRoomManager().updateRoomState(roomInfo);
                    mLiveManager.getRoomManager().updateLiveInfo();
                    mUserManager.getAudienceList();
                    mUserManager.updateOwnerUserInfo();
                    mCoGuestManager.getSeatList();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LiveStreamLog.error(TAG + " startLiveStream failed:error:" + error + ",errorCode:" + error.getValue() + "message:" + message);
                    ErrorLocalized.onError(error);
                    mLiveManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.NONE);
                    view.setEnabled(true);
                }
            });
        });
    }

    private void initLiveInfoEditView() {
        mLiveInfoEditView.init(mLiveManager);
    }

    private void initPreviewFunctionView() {
        mPreviewFunctionView.init(mLiveManager, mLiveCoreView);
    }

    private void initAudienceListView() {
        mAudienceListView.init(mRoomState.roomId);
        mAudienceListView.setOnUserItemClickListener(userInfo -> {
            if (mUserManagerDialog == null) {
                mUserManagerDialog = new UserManagerDialog(mContext, mLiveManager);
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
            FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
            if (floatWindowManager.hasPermission()) {
                floatWindowManager.setWillOpenFloatWindow(true);
                floatWindowManager.setCoreView(mLiveCoreView);
                ((Activity) mContext).finish();
            } else {
                floatWindowManager.requestPermission();
            }
        });
    }

    private void initRoomInfoView() {
        boolean enableFollow = VideoLiveKitImpl.createInstance(mContext).isEnableFollowFeature();
        mRoomInfoView.init(mRoomState.roomId, enableFollow);
    }

    private void initBarrageInputView() {
        mBarrageInputView.init(mRoomState.roomId);
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

            if (mUserManagerDialog == null) {
                mUserManagerDialog = new UserManagerDialog(mContext, mLiveManager);
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
            AnchorCoGuestManageDialog dialog = new AnchorCoGuestManageDialog(mContext, mLiveManager, mLiveCoreView);
            dialog.setOnDismissListener(dialog1 -> view.setEnabled(true));
            dialog.show();
        });
    }

    private void initCoHostView() {
        mViewCoHost.setOnClickListener((view) -> {
            AnchorCoHostManageDialog dialog = new AnchorCoHostManageDialog(mContext, mLiveManager, mLiveCoreView);
            dialog.show();
        });
    }

    private void initBattleView() {
        mViewBattle.setOnClickListener(view -> {
            if (Boolean.TRUE.equals(mBattleState.mIsBattleRunning.getValue())) {
                AnchorEndBattleDialog dialog = new AnchorEndBattleDialog(mContext, mLiveManager, mLiveCoreView);
                dialog.show();
            } else {
                List<String> list = new ArrayList<>();
                String selfId = mLiveManager.getUserState().selfInfo.userId;
                for (CoHostState.ConnectionUser user : mCoHostState.connectedUsers.getValue()) {
                    if (!user.userId.equals(selfId)) {
                        list.add(user.userId);
                    }
                }
                TUILiveBattleManager.BattleConfig battleConfig = new TUILiveBattleManager.BattleConfig();
                battleConfig.duration = BattleState.BATTLE_DURATION;
                battleConfig.needResponse = true;
                battleConfig.extensionInfo = "";
                mLiveCoreView.requestBattle(battleConfig, list, BattleState.BATTLE_REQUEST_TIMEOUT,
                        new LiveCoreViewDefine.BattleRequestCallback() {
                            @Override
                            public void onSuccess(String battleId, List<String> requestedUserIdList) {
                                mLiveManager.getBattleManager().onRequestBattle(battleId, requestedUserIdList);
                            }

                            @Override
                            public void onError(TUICommonDefine.Error error, String message) {
                                LiveStreamLog.error(TAG + " requestBattle failed:error:" + error + ",errorCode:" + error.getValue() + "message:" + message);
                                ErrorLocalized.onError(error);
                            }
                        });
            }
        });
    }

    private void initApplyCoGuestFloatView() {
        mApplyCoGuestFloatView.init(mLiveManager, mLiveCoreView);
    }

    private void initGiftPlayView() {
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
        mGiftPlayView.init(mRoomState.roomId);
    }

    @Override
    protected void addObserver() {
        mRoomState.liveStatus.observeForever(mLiveStatusObserver);
        mCoHostState.connectedUsers.observeForever(mConnectedObserver);
        mCoHostState.receivedConnectionRequest.observeForever(mReceivedConnectRequestObserver);
        mBattleState.mReceivedBattleRequest.observeForever(mReceivedBattleRequestObserver);
        mBattleState.mIsBattleRunning.observeForever(mBattleStartObserver);
        mBattleState.mIsInWaiting.observeForever(mInWaitingObserver);
        mBattleState.mIsOnDisplayResult.observeForever(mBattleResultDisplayObserver);
        mUserState.enterUserInfo.observeForever(mEnterUserObserver);
        mCoGuestState.connectedUserList.observeForever(mCoGuestUserObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW, this);
    }

    @Override
    protected void removeObserver() {
        mRoomState.liveStatus.removeObserver(mLiveStatusObserver);
        mCoHostState.connectedUsers.removeObserver(mConnectedObserver);
        mCoHostState.receivedConnectionRequest.removeObserver(mReceivedConnectRequestObserver);
        mBattleState.mReceivedBattleRequest.removeObserver(mReceivedBattleRequestObserver);
        mBattleState.mIsBattleRunning.removeObserver(mBattleStartObserver);
        mBattleState.mIsInWaiting.removeObserver(mInWaitingObserver);
        mBattleState.mIsOnDisplayResult.removeObserver(mBattleResultDisplayObserver);
        mUserState.enterUserInfo.removeObserver(mEnterUserObserver);
        mCoGuestState.connectedUserList.removeObserver(mCoGuestUserObserver);
        TUICore.unRegisterEvent(this);
    }

    private void showLiveStreamEndDialog() {
        EndLiveStreamDialog endLiveStreamDialog = new EndLiveStreamDialog(mContext, mLiveCoreView, mLiveManager);
        endLiveStreamDialog.show();
    }

    private void onLiveStatusChange(RoomState.LiveStatus liveStatus) {
        initComponentView();
    }

    private void onReceivedCoHostRequest(ConnectionUser receivedConnectionRequest) {
        if (receivedConnectionRequest == null) {
            if (mProcessConnectionDialog != null) {
                mProcessConnectionDialog.dismiss();
            }
            return;
        }
        String content = receivedConnectionRequest.userName
                + getContext().getString(R.string.live_connect_inviting_append);
        showConnectionRequestDialog(content, receivedConnectionRequest.avatarUrl, receivedConnectionRequest.roomId);
    }

    private void showConnectionRequestDialog(String content, String avatarUrl, String roomId) {
        mProcessConnectionDialog = new StandardDialog(getContext());
        mProcessConnectionDialog.setContent(content);
        mProcessConnectionDialog.setAvatar(avatarUrl);

        String rejectText = getContext().getString(R.string.live_reject);
        mProcessConnectionDialog.setNegativeText(rejectText, negativeView -> {
            mLiveCoreView.respondToCrossRoomConnection(roomId, false, null);
            mCoHostManager.removeReceivedConnectionRequest();
            mProcessConnectionDialog.dismiss();
        });

        String receiveText = getContext().getString(R.string.live_receive);
        mProcessConnectionDialog.setPositiveText(receiveText, positiveView -> {
            mLiveCoreView.respondToCrossRoomConnection(roomId, true, null);
            mCoHostManager.removeReceivedConnectionRequest();
            mProcessConnectionDialog.dismiss();
        });
        mProcessConnectionDialog.show();
    }

    private void showMaskView(RoomState.LiveStatus liveStatus) {
        if (RoomState.LiveStatus.PREVIEWING == liveStatus) {
            mLayoutAnchorPreviewMask.setVisibility(VISIBLE);
            mLayoutAnchorLivingTopMask.setVisibility(GONE);
            mLayoutAnchorLivingBottomMask.setVisibility(GONE);
        } else if (RoomState.LiveStatus.PUSHING == liveStatus || RoomState.LiveStatus.PLAYING == liveStatus) {
            mLayoutAnchorPreviewMask.setVisibility(GONE);
            mLayoutAnchorLivingTopMask.setVisibility(VISIBLE);
            mLayoutAnchorLivingBottomMask.setVisibility(VISIBLE);
        } else {
            mLayoutAnchorPreviewMask.setVisibility(GONE);
            mLayoutAnchorLivingTopMask.setVisibility(GONE);
            mLayoutAnchorLivingBottomMask.setVisibility(GONE);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onConnectedUserChange(List<ConnectionUser> connectedList) {
        post(() -> {
            enableView(mViewBattle, !connectedList.isEmpty());
            enableView(mViewCoGuest, connectedList.isEmpty());
            MediaState.VideoEncParams.VideoEncType targetEncType = connectedList.size() >= 2
                    ? MediaState.VideoEncParams.VideoEncType.SMALL
                    : MediaState.VideoEncParams.VideoEncType.BIG;
            mLiveManager.getMediaManager().changeVideoEncParams(targetEncType);
        });
    }

    private void onCoGuestUserChange(List<CoGuestState.SeatInfo> seatList) {
        post(() -> {
            enableView(mViewCoHost, seatList.size() <= 1);

            MediaState.VideoEncParams.VideoEncType targetEncType = seatList.size() >= 2
                    ? MediaState.VideoEncParams.VideoEncType.SMALL
                    : MediaState.VideoEncParams.VideoEncType.BIG;
            mLiveManager.getMediaManager().changeVideoEncParams(targetEncType);
        });
    }

    private void onReceivedBattleRequestChange(BattleState.BattleUser user) {
        if (mProcessBattleDialog != null) {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
        }
        if (user == null) {
            return;
        }
        String content = user.userName + " " + getContext().getString(R.string.live_battle_inviting);
        mProcessBattleDialog = new StandardDialog(getContext());
        mProcessBattleDialog.setContent(content);
        mProcessBattleDialog.setAvatar(user.avatarUrl);

        String rejectText = getContext().getString(R.string.live_reject);
        mProcessBattleDialog.setNegativeText(rejectText, negativeView -> {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
            mLiveCoreView.respondToBattle(mBattleState.mBattleId, false, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mLiveManager.getBattleManager().onResponseBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LiveStreamLog.error(TAG + " respondToBattle failed:error:" + error + ",errorCode:" + error.getValue() + "message:" + message);
                    ErrorLocalized.onError(error);
                }
            });
        });

        String receiveText = getContext().getString(R.string.live_receive);
        mProcessBattleDialog.setPositiveText(receiveText, positiveView -> {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
            mLiveCoreView.respondToBattle(mBattleState.mBattleId, true, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mLiveManager.getBattleManager().onResponseBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LiveStreamLog.error(TAG + " respondToBattle failed:error:" + error + ",errorCode:" + error.getValue() + "message:" + message);
                    ErrorLocalized.onError(error);
                }
            });
        });
        mProcessBattleDialog.show();
    }

    private void showBattleCountdownDialog() {
        if (mBattleCountdownDialog == null) {
            mBattleCountdownDialog = new BattleCountdownDialog(mContext, mLiveManager, mLiveCoreView);
        }
        mBattleCountdownDialog.show();
    }

    private void dismissBattleCountdownDialog() {
        if (mBattleCountdownDialog != null) {
            mBattleCountdownDialog.dismiss();
            mBattleCountdownDialog = null;
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
            mViewBattle.setBackgroundResource(R.drawable.livekit_function_battle_exit);
            for (BattleState.BattleUser user : mBattleState.mBattledUsers.getValue()) {
                if (TextUtils.equals(mUserState.selfInfo.userId, user.userId)) {
                    enableView(mViewCoHost, false);
                    break;
                }
            }
        } else if (Boolean.FALSE.equals(started)) {
            mViewBattle.setBackgroundResource(R.drawable.livekit_function_battle);
            enableView(mViewCoHost, true);
        }
    }

    private void onBattleResultDisplay(Boolean onDisplay) {
        if (Boolean.TRUE.equals(onDisplay)) {
            enableView(mViewBattle, false);
        } else if (Boolean.FALSE.equals(onDisplay)) {
            enableView(mViewBattle, true);
        }
    }

    private void enableView(View view, boolean enable) {
        view.setEnabled(enable);
        view.setAlpha(enable ? 1.0f : 0.5f);
    }

    private void onEnterUserChange(UserState.UserInfo userInfo) {
        if (userInfo != null && mBarrageStreamView != null) {
            Barrage barrage = new Barrage();
            barrage.content = mContext.getString(R.string.live_entered_room);
            barrage.user.userId = userInfo.userId;
            barrage.user.userName = TextUtils.isEmpty(userInfo.name.getValue()) ? userInfo.userId :
                    userInfo.name.getValue();
            barrage.user.avatarUrl = userInfo.avatarUrl.getValue();
            mBarrageStreamView.insertBarrages(barrage);
        }
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (TextUtils.equals(subKey, EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW)) {
            UserInfo userInfo = (UserInfo)param.get(EVENT_PARAMS_KEY_USER_INFO);
            showCoGuestManageDialog(userInfo);
        }
    }
}
