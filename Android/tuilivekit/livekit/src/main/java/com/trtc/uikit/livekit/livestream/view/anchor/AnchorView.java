package com.trtc.uikit.livekit.livestream.view.anchor;

import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_VIEW_TYPE_1;

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

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomInfoCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.component.audiencelist.AudienceListView;
import com.trtc.uikit.component.barrage.BarrageInputView;
import com.trtc.uikit.component.barrage.BarrageStreamView;
import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.component.gift.GiftPlayView;
import com.trtc.uikit.component.gift.store.model.Gift;
import com.trtc.uikit.component.gift.store.model.GiftUser;
import com.trtc.uikit.component.music.MusicPanelView;
import com.trtc.uikit.component.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.component.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestream.manager.module.DashboardManager;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveBattleManagerObserver;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveStreamObserver;
import com.trtc.uikit.livekit.livestream.state.BattleState;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionUser;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus;
import com.trtc.uikit.livekit.livestream.state.UserState;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestream.view.anchor.dashboard.AnchorDashboardView;
import com.trtc.uikit.livekit.livestream.view.anchor.preview.LiveInfoEditView;
import com.trtc.uikit.livekit.livestream.view.anchor.preview.PreviewFunctionView;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.EndLiveStreamDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle.AnchorEndBattleDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle.BattleCountdownDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest.AnchorCoGuestManageDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest.ApplyCoGuestFloatView;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost.AnchorCoHostManageDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost.StandardDialog;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.settings.SettingsPanelDialog;
import com.trtc.uikit.livekit.livestream.view.widgets.battle.BattleInfoView;
import com.trtc.uikit.livekit.livestream.view.widgets.battle.BattleMemberInfoView;
import com.trtc.uikit.livekit.livestream.view.widgets.coguest.CoGuestWidgetsView;
import com.trtc.uikit.livekit.livestream.view.widgets.cohost.CoHostWidgetsView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;

import java.util.ArrayList;
import java.util.List;

@SuppressLint("ViewConstructor")
public class AnchorView extends BasicView {

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
    private       PopupDialog                            mMusicPanelDialog;
    private       ApplyCoGuestFloatView                  mApplyCoGuestFloatView;
    private       StandardDialog                         mProcessConnectionDialog;
    private       StandardDialog                         mProcessBattleDialog;
    private       BattleCountdownDialog                  mBattleCountdownDialog;
    private       TUILiveRoomAnchorFragment.RoomBehavior mRoomBehavior;
    private final Observer<LiveStatus>                   mLiveStatusObserver             = this::onLiveStatusChange;
    private final Observer<ConnectionUser>               mReceivedConnectRequestObserver =
            this::onReceivedCoHostRequest;
    private final Observer<List<ConnectionUser>>         mConnectedObserver              = this::onConnectedUserChange;
    private final Observer<Boolean>                      mBattleStartObserver            = this::onBattleStartChange;
    private final Observer<Boolean>                      mBattleResultDisplayObserver    = this::onBattleResultDisplay;
    private final Observer<UserState.UserInfo>           mEnterUserObserver              = this::onEnterUserChange;
    private final Observer<BattleState.BattleUser>       mReceivedBattleRequestObserver  =
            this::onReceivedBattleRequestChange;
    private final Observer<Boolean>                      mInWaitingObserver              = this::onInWaitingChange;

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

    public void destroy() {
        mLiveCoreView.stopLiveStream(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mLiveManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.DASHBOARD);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (!FloatWindowManager.getInstance().isWillOpenFloatWindow()) {
            mLiveCoreView.stopCamera();
            mLiveCoreView.stopMicrophone();
        }
        mLiveManager.getMediaManager().clearEBeautyView();
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
    }

    @Override
    protected void refreshView() {
        initBackView();
        initLiveCoreView();
        initComponentView();
    }

    private void initBackView() {
        mBackView.setOnClickListener(v -> ((Activity) getContext()).finish());
    }

    private void initLiveCoreView() {
        if (mLiveCoreView == null) {
            mLiveCoreView = FloatWindowManager.getInstance().getCoreView();
            if (mLiveCoreView == null) {
                mLiveCoreView = new LiveCoreView(ContextProvider.getApplicationContext());
                mLiveCoreView.registerConnectionObserver(new LiveStreamObserver(mLiveManager));
                mLiveCoreView.registerBattleObserver(new LiveBattleManagerObserver(mLiveManager));
            } else {
                FloatWindowManager.getInstance().setCoreView(null);
            }
            FrameLayout frameLayout = findViewById(R.id.lsv_video_view_container);
            frameLayout.addView(mLiveCoreView);
        }
        mLiveCoreView.setVideoViewAdapter(new LiveCoreViewDefine.VideoViewAdapter() {
            @Override
            public View createCoGuestView(UserInfo userInfo) {
                CoGuestWidgetsView coGuestWidgetsView = new CoGuestWidgetsView(getContext());
                coGuestWidgetsView.init(mLiveManager, userInfo);
                return coGuestWidgetsView;
            }

            @Override
            public void updateCoGuestView(View coGuestView, UserInfo userInfo,
                                          List<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlag) {
                Logger.info("Anchor initLiveCoreView updateCoGuestView: userInfo = " + new Gson().toJson(userInfo)
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
                Logger.info("Anchor initLiveCoreView updateCoHostView: coHostUser = " + new Gson().toJson(coHostUser)
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

        mLiveCoreView.startCamera(true, null);
        mLiveCoreView.startMicrophone(null);
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
                    if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
                        mCoGuestManager.getSeatApplicationList();
                    }
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorHandler.onError(error);
                    removeAllViews();
                    if (mContext instanceof Activity) {
                        ((Activity) mContext).finish();
                    }
                }
            });
        }
    }

    private void initComponentView() {
        if (mRoomState.liveStatus.get() == RoomState.LiveStatus.PREVIEWING) {
            initPreviewStatus();
        } else if (mRoomState.liveStatus.get() == RoomState.LiveStatus.PUSHING) {
            initPushingStatus();
        } else if (mRoomState.liveStatus.get() == RoomState.LiveStatus.DASHBOARD) {
            initDashboardStatus();
        }
    }

    private void initDashboardStatus() {
        DashboardManager dashboardManager = mLiveManager.getDashboardManager();
        dashboardManager.updateDuration(System.currentTimeMillis() - mRoomState.createTime);
        dashboardManager.updateMaxViewersCount(mRoomState.maxAudienceCount);
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
        initMusicView();
        initApplyCoGuestFloatView();
        initGiftPlayView();
    }

    private void initSettingsPanel() {
        findViewById(R.id.v_settings).setOnClickListener(view -> {
            SettingsPanelDialog settingsPanelDialog = new SettingsPanelDialog(mContext, mLiveManager, mLiveCoreView);
            settingsPanelDialog.show();
        });
    }

    private void initMusicView() {
        findViewById(R.id.v_music).setOnClickListener(view -> {
            if (mMusicPanelDialog == null) {
                mMusicPanelDialog = new PopupDialog(mContext);
                MusicPanelView musicListPanelView = new MusicPanelView(mContext);
                musicListPanelView.init(mRoomState.roomId);
                mMusicPanelDialog.setView(musicListPanelView);
            }
            mMusicPanelDialog.show();
        });
    }

    private void initStartLiveView() {
        mButtonStartLive.setOnClickListener((View view) -> {
            RoomInfo roomInfo = new RoomInfo();
            roomInfo.roomId = mRoomState.roomId;
            roomInfo.name = mRoomState.roomName.get();
            mUserManager.initSelfUserData();
            mLiveCoreView.startLiveStream(roomInfo, new GetRoomInfoCallback() {
                @Override
                public void onSuccess(RoomInfo roomInfo) {
                    mLiveManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PUSHING);
                    mLiveManager.getRoomManager().updateRoomState(roomInfo);
                    mLiveManager.getRoomManager().updateLiveInfo();
                    mUserManager.getAudienceList();
                    mUserManager.updateOwnerUserInfo();
                    mCoGuestManager.getSeatList();
                    if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
                        mCoGuestManager.getSeatApplicationList();
                    }
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorHandler.onError(error);
                    mLiveManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.NONE);
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
        mRoomInfoView.init(mRoomState.roomId);
    }

    private void initBarrageInputView() {
        mBarrageInputView.init(mRoomState.roomId);
    }

    private void initBarrageStreamView() {
        mBarrageStreamView.init(mRoomState.roomId, mRoomState.ownerInfo.userId);
        mBarrageStreamView.setItemTypeDelegate(new BarrageViewTypeDelegate());
        mBarrageStreamView.setItemAdapter(GIFT_VIEW_TYPE_1, new GiftBarrageAdapter(mContext));
    }

    private void initCoGuestView() {
        mViewCoGuest.setOnClickListener((view) -> {
            AnchorCoGuestManageDialog dialog = new AnchorCoGuestManageDialog(mContext, mLiveManager, mLiveCoreView);
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
            if (Boolean.TRUE.equals(mBattleState.mIsBattleRunning.get())) {
                AnchorEndBattleDialog dialog = new AnchorEndBattleDialog(mContext, mLiveManager, mLiveCoreView);
                dialog.show();
            } else {
                List<String> list = new ArrayList<>();
                String selfId = mLiveManager.getUserState().selfInfo.userId;
                for (CoHostState.ConnectionUser user : mCoHostState.connectedUsers.get()) {
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
                            public void onError(TUICommonDefine.Error error, String s) {
                                ErrorHandler.onError(error);
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
                barrage.user.userName = sender.userName;
                barrage.user.avatarUrl = sender.avatarUrl;
                barrage.user.level = sender.level;
                barrage.extInfo.put(GIFT_VIEW_TYPE, GIFT_VIEW_TYPE_1);
                barrage.extInfo.put(GIFT_NAME, gift.giftName);
                barrage.extInfo.put(GIFT_COUNT, giftCount);
                barrage.extInfo.put(GIFT_ICON_URL, gift.imageUrl);
                barrage.extInfo.put(GIFT_RECEIVER_USERNAME, receiver.userName);
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
        mRoomState.liveStatus.observe(mLiveStatusObserver);
        mCoHostState.connectedUsers.observe(mConnectedObserver);
        mCoHostState.receivedConnectionRequest.observe(mReceivedConnectRequestObserver);
        mBattleState.mReceivedBattleRequest.observe(mReceivedBattleRequestObserver);
        mBattleState.mIsBattleRunning.observe(mBattleStartObserver);
        mBattleState.mIsInWaiting.observe(mInWaitingObserver);
        mBattleState.mIsOnDisplayResult.observe(mBattleResultDisplayObserver);
        mUserState.enterUserInfo.observe(mEnterUserObserver);
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
    }

    private void showLiveStreamEndDialog() {
        mLiveManager.getDashboardManager().updateDuration(System.currentTimeMillis() - mRoomState.createTime);
        mLiveManager.getDashboardManager().updateMaxViewersCount(mRoomState.maxAudienceCount);
        mLiveManager.getDashboardManager().updateLikeNumber(mGiftPlayView.getLikeCount());
        mLiveManager.getDashboardManager().updateMessageCount(mBarrageStreamView.getBarrageCount());
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
                + getContext().getString(R.string.livekit_connect_inviting_append);
        showConnectionRequestDialog(content, receivedConnectionRequest.avatarUrl, receivedConnectionRequest.roomId);
    }

    private void showConnectionRequestDialog(String content, String avatarUrl, String roomId) {
        mProcessConnectionDialog = new StandardDialog(getContext());
        mProcessConnectionDialog.setContent(content);
        mProcessConnectionDialog.setAvatar(avatarUrl);

        String rejectText = getContext().getString(R.string.livekit_reject);
        mProcessConnectionDialog.setNegativeText(rejectText, negativeView -> {
            mLiveCoreView.respondToCrossRoomConnection(roomId, false, null);
            mCoHostManager.removeReceivedConnectionRequest();
            mProcessConnectionDialog.dismiss();
        });

        String receiveText = getContext().getString(R.string.livekit_receive);
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
        post(() -> enableView(mViewBattle, !connectedList.isEmpty()));
    }

    private void onReceivedBattleRequestChange(BattleState.BattleUser user) {
        if (mProcessBattleDialog != null) {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
        }
        if (user == null) {
            return;
        }
        String content = user.userName + " " + getContext().getString(R.string.livekit_battle_inviting);
        mProcessBattleDialog = new StandardDialog(getContext());
        mProcessBattleDialog.setContent(content);
        mProcessBattleDialog.setAvatar(user.avatarUrl);

        String rejectText = getContext().getString(R.string.livekit_reject);
        mProcessBattleDialog.setNegativeText(rejectText, negativeView -> {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
            mLiveCoreView.respondToBattle(mBattleState.mBattleId, false, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mLiveManager.getBattleManager().onResponseBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String s) {

                }
            });
        });

        String receiveText = getContext().getString(R.string.livekit_receive);
        mProcessBattleDialog.setPositiveText(receiveText, positiveView -> {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
            mLiveCoreView.respondToBattle(mBattleState.mBattleId, true, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mLiveManager.getBattleManager().onResponseBattle();
                }

                @Override
                public void onError(TUICommonDefine.Error error, String s) {

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
            for (BattleState.BattleUser user : mBattleState.mBattledUsers.get()) {
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
            barrage.content = mContext.getString(R.string.livekit_entered_room);
            barrage.user.userId = userInfo.userId;
            barrage.user.userName = userInfo.name.get();
            barrage.user.avatarUrl = userInfo.avatarUrl.get();
            barrage.user.level = "32";
            mBarrageStreamView.insertBarrages(barrage);
        }
    }
}
