package com.trtc.uikit.livekit.livestream.view.audience;

import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_USER_INFO;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW;
import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.LINKING;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.DASHBOARD;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.PLAYING;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomInfoCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.uikit.component.barrage.BarrageInputView;
import com.trtc.uikit.component.barrage.BarrageStreamView;
import com.trtc.uikit.component.barrage.store.BarrageStore;
import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.Constants;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.utils.LiveCoreLogger;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.dashboard.StreamDashboardDialog;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.component.gift.GiftPlayView;
import com.trtc.uikit.livekit.component.gift.LikeButton;
import com.trtc.uikit.livekit.component.giftaccess.GiftButton;
import com.trtc.uikit.livekit.component.giftaccess.service.GiftCacheService;
import com.trtc.uikit.livekit.component.giftaccess.store.GiftStore;
import com.trtc.uikit.livekit.component.giftaccess.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.giftaccess.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionUser;
import com.trtc.uikit.livekit.livestream.state.MediaState;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestream.view.VideoLiveKitImpl;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest.AnchorManagerDialog;
import com.trtc.uikit.livekit.livestream.view.audience.dashboard.AudienceDashboardView;
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class AudienceView extends BasicView implements ITUINotification {
    private       LiveCoreView                         mLiveCoreView;
    private       FrameLayout                          mLayoutPlaying;
    private       AudienceDashboardView                mAudienceDashboardView;
    private       ImageView                            mImageDashboard;
    private       GiftButton                           mButtonGift;
    private       LikeButton                           mButtonLike;
    private       ImageView                            mImageCoGuest;
    private       BarrageInputView                     mBarrageInputView;
    private       RoomInfoView                         mRoomInfoView;
    private       GiftPlayView                         mGiftPlayView;
    private       AudienceListView                     mAudienceListView;
    private       BarrageStreamView                    mBarrageStreamView;
    private       ImageView                            mImageFloatWindow;
    private       ImageView                            mImageExitRoom;
    private       CoGuestRequestFloatView              mWaitingCoGuestPassView;
    private       UserInfoDialog                       mUserInfoDialog;
    private       AnchorManagerDialog                  mAnchorManagerDialog;
    private       ViewObserver                         mViewObserver;
    private       boolean                              mIsLoading;
    private final Observer<RoomState.LiveStatus>       mLiveStatusChangeObserver =
            this::onLiveStatusChange;
    private final Observer<CoGuestState.CoGuestStatus> mLinkStatusObserver       =
            this::onLinkStatusChange;
    private final Observer<UserState.UserInfo>         mEnterUserObserver        =
            this::onEnterUserChange;
    private final Observer<List<ConnectionUser>>       mConnectedObserver        =
            this::onConnectedUserChange;

    public AudienceView(@NonNull Context context) {
        this(context, null);
    }

    public AudienceView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
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
        mImageExitRoom = findViewById(R.id.iv_exit_room);
    }

    @Override
    protected void refreshView() {
        initComponentView();
    }

    public void onViewDidSlideIn() {
        if (mRoomState.liveStatus.getValue() == PLAYING) {
            return;
        }
        mLiveManager.addObserver();
        mLayoutPlaying.setVisibility(GONE);
        onViewLoading();
        mLiveCoreView.joinLiveStream(mRoomState.roomId, new GetRoomInfoCallback() {
            @Override
            public void onSuccess(RoomInfo roomInfo) {
                mRoomManager.updateRoomState(roomInfo);
                mRoomManager.getLiveInfo(roomInfo.roomId);
                mRoomState.liveStatus.setValue(PLAYING);
                onViewFinished();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                onViewFinished();
                ErrorLocalized.onError(error);
                removeAllViews();
                if (mContext instanceof Activity) {
                    ((Activity) mContext).finish();
                }
            }
        });
    }

    public void setViewObserver(ViewObserver observer) {
        mViewObserver = observer;
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

    public void onViewDidSlideOut() {
        FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
        if (floatWindowManager.isWillOpenFloatWindow() && FloatWindowManager.getInstance().getCoreView() == mLiveCoreView) {
            floatWindowManager.setLiveStreamManager(mLiveManager);
            floatWindowManager.showFloatWindow();
            floatWindowManager.setWillOpenFloatWindow(false);
            mLiveCoreView.removeView(this);
        } else {
            mLiveManager.removeObserver();
            mLiveCoreView.leaveLiveStream(null);
            mLiveManager.getState().reset();
            BarrageStore.sharedInstance().unInit(mLiveManager.getRoomState().roomId);
            com.trtc.uikit.livekit.component.gift.store.GiftStore.sharedInstance().unInit(mLiveManager.getRoomState().roomId);
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
        mLiveCoreView.setVideoViewAdapter(new LiveCoreViewDefine.VideoViewAdapter() {
            @Override
            public View createCoGuestView(UserInfo userInfo) {
                LiveCoreLogger.info("initLiveCoreView createCoGuestView:" + userInfo.userId);
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
                LiveCoreLogger.info("initLiveCoreView updateCoGuestView: userInfo = " + new Gson().toJson(userInfo)
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
                LiveCoreLogger.info("initLiveCoreView updateCoHostView: coHostUser = " + new Gson().toJson(coHostUser)
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
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mLiveManager.getUserManager().clearEnterUserInfo();
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
        mLayoutPlaying.removeAllViews();
        mAudienceDashboardView.setVisibility(VISIBLE);

        initAudienceDashboardView();
    }

    private void initAudienceListView() {
        mAudienceListView.init(mRoomState.roomId);
    }

    private void initExitRoomView() {
        mImageExitRoom.setOnClickListener(view -> destroy());
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

    @Override
    public void init(@NonNull LiveStreamManager liveStreamManager) {
        mLiveCoreView = (LiveCoreView) getParent();
        super.init(liveStreamManager);
        setComponent();
        setVideoViewAdapter();
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
    }

    @Override
    protected void removeObserver() {
        mCoGuestState.coGuestStatus.removeObserver(mLinkStatusObserver);
        mRoomState.liveStatus.removeObserver(mLiveStatusChangeObserver);
        mUserState.enterUserInfo.removeObserver(mEnterUserObserver);
        mCoHostState.connectedUsers.removeObserver(mConnectedObserver);
        TUICore.unRegisterEvent(this);
    }

    public void destroy() {
        if (mIsLoading) {
            return;
        }
        if (mCoGuestState.coGuestStatus.getValue() == LINKING) {
            showLiveStreamEndDialog();
        } else {
            mLiveCoreView.leaveLiveStream(null);
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        }
    }

    private void onLinkStatusChange(CoGuestState.CoGuestStatus linkStatus) {
        Map<String, Object> params = new HashMap<>();
        if (LINKING == linkStatus) {
            params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, false);
        } else {
            params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, true);
        }
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, params);

        switch (linkStatus) {
            case NONE:
                mWaitingCoGuestPassView.setVisibility(GONE);
                mLiveCoreView.stopCamera();
                mLiveCoreView.stopMicrophone();
                initCoGuestIcon();
                break;
            case APPLYING:
                mWaitingCoGuestPassView.setVisibility(VISIBLE);
                cancelCoGuestRequest();
                break;
            case LINKING:
                mWaitingCoGuestPassView.setVisibility(GONE);
                mLiveManager.getMediaManager().changeVideoEncParams(MediaState.VideoEncParams.VideoEncType.SMALL);
                stopCoGuest();
                break;
            default:
                break;
        }
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
        EndLiveStreamDialog dialog = new EndLiveStreamDialog(mContext, mLiveCoreView, mLiveManager);
        dialog.show();
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (TextUtils.equals(subKey, EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW)) {
            UserInfo userInfo = (UserInfo) param.get(EVENT_PARAMS_KEY_USER_INFO);
            showCoGuestManageDialog(userInfo);
        }
    }

    public interface ViewObserver {
        void onLoading();

        void onFinished();
    }
}
