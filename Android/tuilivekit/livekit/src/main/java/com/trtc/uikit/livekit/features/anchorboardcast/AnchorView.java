package com.trtc.uikit.livekit.features.anchorboardcast;

import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_USER_INFO;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.lifecycle.Observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.component.barrage.BarrageInputView;
import com.trtc.uikit.component.barrage.BarrageStreamView;
import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.ui.RoundFrameLayout;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.gift.GiftPlayView;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.giftaccess.service.GiftCacheService;
import com.trtc.uikit.livekit.component.giftaccess.store.GiftStore;
import com.trtc.uikit.livekit.component.giftaccess.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.giftaccess.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.component.roominfo.LiveInfoView;
import com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine.Behavior;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.coreviewwidget.CoGuestWidgetsView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.managerpanel.ApplyCoGuestFloatView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.managerpanel.CoGuestManageDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.coreviewwidget.CoHostWidgetsView;
import com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.managerpanel.CoHostManageDialog;
import com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.managerpanel.ConnectionRequestDialog;
import com.trtc.uikit.livekit.livestream.view.VideoLiveKitImpl;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AnchorView extends FrameLayout {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorView");

    private       AnchorManager                    mManager;
    private       AnchorState                      mState;
    private       Behavior                         mBehavior                       = Behavior.CREATE_AND_ENTER_ROOM;
    private       LiveCoreView                     mLiveCoreView;
    private       ConnectionRequestDialog          mConnectionRequestDialog;
    private final FrameLayout                      mLayoutRoot;
    private final AudienceListView                 mAudienceListView;
    private final LiveInfoView                     mRoomInfoView;
    private final BarrageInputView                 mBarrageInputView;
    private final BarrageStreamView                mBarrageStreamView;
    private final GiftPlayView                     mGiftPlayView;
    private final View                             mViewCoGuest;
    private final View                             mViewCoHost;
    private final View                             mViewBattle;
    private final ApplyCoGuestFloatView            mApplyCoGuestFloatView;
    private final Observer<TUIRoomDefine.UserInfo> mEnterUserObserver              = this::onEnterUserChange;
    private final Observer<ConnectionUser>         mReceivedConnectRequestObserver = this::onReceivedCoHostRequest;

    private void onReceivedCoHostRequest(ConnectionUser user) {
        if (user == null) {
            if (mConnectionRequestDialog != null && mConnectionRequestDialog.isShowing()) {
                mConnectionRequestDialog.dismiss();
            }
            return;
        }
        mConnectionRequestDialog = new ConnectionRequestDialog(getContext(), mManager);
        mConnectionRequestDialog.show();
    }

    public AnchorView(Context context) {
        this(context, null);
    }

    public AnchorView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        LayoutInflater.from(getContext()).inflate(R.layout.anchor_streaming_layout_main_view, this, true);
        mLayoutRoot = findViewById(R.id.fl_root);
        mAudienceListView = findViewById(R.id.audience_list_view);
        mRoomInfoView = findViewById(R.id.room_info_view);
        mBarrageInputView = findViewById(R.id.barrage_input_view);
        mBarrageStreamView = findViewById(R.id.barrage_stream_view);
        mGiftPlayView = findViewById(R.id.gift_play_view);
        mViewCoGuest = findViewById(R.id.ll_co_guest);
        mViewCoHost = findViewById(R.id.ll_co_host);
        mViewBattle = findViewById(R.id.ll_battle);
        mApplyCoGuestFloatView = findViewById(R.id.rl_apply_link_audience);
    }

    public void init(String roomId, LiveCoreView liveCoreView, AnchorViewDefine.Behavior behavior) {
        if (liveCoreView == null) {
            mLiveCoreView = new LiveCoreView(getContext());
        } else {
            mLiveCoreView = liveCoreView;
        }
        mBehavior = behavior;
        initManager(roomId);
        initView();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mState.enterUserInfo.observeForever(mEnterUserObserver);
        mLiveCoreView.getCoreState().coHostState.receivedConnectionRequest.observeForever(mReceivedConnectRequestObserver);
    }

    private void removeObserver() {
        mState.enterUserInfo.removeObserver(mEnterUserObserver);
        mLiveCoreView.getCoreState().coHostState.receivedConnectionRequest.removeObserver(mReceivedConnectRequestObserver);
    }

    private void initManager(String roomId) {
        mManager = new AnchorManager(mLiveCoreView);
        mState = mManager.getState();
        mState.roomId = roomId;
    }

    private void initView() {
        initCoreView();
        initRoomInfoView();
        initAudienceListView();
        initBarrageInputView();
        initBarrageStreamView();
        initGiftPlayView();

        initCoGuestView();
        initCoHostView();
    }

    private void initCoHostView() {
        mViewCoHost.setOnClickListener((view) -> {
            CoHostManageDialog dialog = new CoHostManageDialog(getContext(), mManager);
            dialog.show();
        });
    }

    private void initCoGuestView() {
        mApplyCoGuestFloatView.init(mManager);
        mViewCoGuest.setOnClickListener((view) -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            CoGuestManageDialog dialog = new CoGuestManageDialog(getContext(), mManager);
            dialog.setOnDismissListener(dialog1 -> view.setEnabled(true));
            dialog.show();
        });
    }

    private void initGiftPlayView() {
        GiftCacheService giftCacheService = GiftStore.getInstance().mGiftCacheService;
        mGiftPlayView.setListener(new GiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(Gift gift, int giftCount, GiftUser sender, GiftUser receiver) {
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
        mGiftPlayView.init(mState.roomId);
    }

    private void initBarrageStreamView() {
        mBarrageStreamView.init(mState.roomId, mState.ownerInfo.userId);
        mBarrageStreamView.setItemTypeDelegate(new BarrageViewTypeDelegate());
        mBarrageStreamView.setItemAdapter(GIFT_VIEW_TYPE_1, new GiftBarrageAdapter(getContext()));
    }

    private void initBarrageInputView() {
        mBarrageInputView.init(mState.roomId);
    }

    private void initAudienceListView() {
          //todo xander
//        mAudienceListView.init(mState.roomId);
    }

    private void initRoomInfoView() {
        boolean enableFollow = VideoLiveKitImpl.createInstance(getContext()).isEnableFollowFeature();
          //todo xander
//        mRoomInfoView.init(mState.roomId, enableFollow);
    }

    private void initCoreView() {
        if (mLiveCoreView == null) {
            LOGGER.error("Please call the AnchorPrepareView.init() method first.");
            return;
        }
        RoundFrameLayout frameLayout = findViewById(R.id.lsv_video_view_container);
        if (mLiveCoreView.getParent() == null) {
            frameLayout.setRadius(com.trtc.tuikit.common.util.ScreenUtil.dip2px(16));
            FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                    ViewGroup.LayoutParams.MATCH_PARENT);

            frameLayout.addView(mLiveCoreView, layoutParams);
            mLayoutRoot.setBackgroundColor(getResources().getColor(R.color.common_black));
        } else {
            frameLayout.setVisibility(GONE);
            mLayoutRoot.setBackgroundColor(getResources().getColor(R.color.common_design_standard_transparent));
        }

        LiveCoreViewWidgetAdapter mLiveCoreViewWidgetAdapter = new LiveCoreViewWidgetAdapter();
        mLiveCoreView.setVideoViewAdapter(mLiveCoreViewWidgetAdapter);
        if (mBehavior == Behavior.ONLY_ENTER_ROOM) {
            mManager.joinLiveStream();
        } else {
            mManager.startLiveStream();
        }
    }

    private void onEnterUserChange(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo != null && mBarrageStreamView != null) {
            Barrage barrage = new Barrage();
            barrage.content = getContext().getString(R.string.common_entered_room);
            barrage.user.userId = userInfo.userId;
            barrage.user.userName = TextUtils.isEmpty(userInfo.userName) ? userInfo.userId :
                    userInfo.userName;
            barrage.user.avatarUrl = userInfo.avatarUrl;
            mBarrageStreamView.insertBarrages(barrage);
        }
    }

    class LiveCoreViewWidgetAdapter implements LiveCoreViewDefine.VideoViewAdapter {
        @Override
        public View createCoGuestView(TUIRoomDefine.UserInfo userInfo) {
            LOGGER.info("Anchor initLiveCoreView createCoGuestView: userInfo = " + new Gson().toJson(userInfo));
            CoGuestWidgetsView coGuestWidgetsView = new CoGuestWidgetsView(getContext());
            coGuestWidgetsView.init(mManager, userInfo, mLiveCoreView);
            coGuestWidgetsView.setOnClickListener(v -> {
                Map<String, Object> params = new HashMap<>();
                params.put(EVENT_PARAMS_KEY_USER_INFO, userInfo);
                TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_SHOW_CO_GUEST_MANAGE_VIEW, params);
            });
            return coGuestWidgetsView;
        }

        @Override
        public void updateCoGuestView(View coGuestView, TUIRoomDefine.UserInfo userInfo,
                                      List<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlag) {
            LOGGER.info("Anchor initLiveCoreView updateCoGuestView: userInfo = " + new Gson().toJson(userInfo)
                    + ",modifyFlag = " + new Gson().toJson(modifyFlag) + ",coGuestView = " + coGuestView);
        }

        @Override
        public View createCoHostView(LiveCoreViewDefine.CoHostUser coHostUser) {
            LOGGER.info("Anchor initLiveCoreView createCoHostView: coHostUser = " + new Gson().toJson(coHostUser));
            CoHostWidgetsView coHostWidgetsView = new CoHostWidgetsView(getContext());
            coHostWidgetsView.init(mManager, coHostUser, mLiveCoreView);
            return coHostWidgetsView;
        }

        @Override
        public void updateCoHostView(View coHostView, LiveCoreViewDefine.CoHostUser coHostUser,
                                     List<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlag) {
            LOGGER.info("Anchor initLiveCoreView updateCoHostView: coHostUser = " + new Gson().toJson(coHostUser)
                    + ",modifyFlag = " + new Gson().toJson(modifyFlag) + ",coHostView = " + coHostView);
        }

        @Override
        public View createBattleView(TUILiveBattleManager.BattleUser battleUser) {
            LOGGER.info("Anchor initLiveCoreView createBattleView: battleUser = " + new Gson().toJson(battleUser));
            return null;
        }

        @Override
        public void updateBattleView(View battleView, TUILiveBattleManager.BattleUser battleUser) {
            LOGGER.info("Anchor initLiveCoreView updateBattleView: updateBattleView = " + battleView
                    + "battleUser =" + new Gson().toJson(battleUser));
        }

        @Override
        public View createBattleContainerView() {
            LOGGER.info("Anchor initLiveCoreView createBattleContainerView");
            return null;
        }

        @Override
        public void updateBattleContainerView(View battleContainView,
                                              List<LiveCoreViewDefine.BattleUserViewModel> userInfo) {
            LOGGER.info("Anchor initLiveCoreView battleContainView: updateBattleContainerView = " + battleContainView
                    + "userInfo =" + new Gson().toJson(userInfo));
        }
    }
}
