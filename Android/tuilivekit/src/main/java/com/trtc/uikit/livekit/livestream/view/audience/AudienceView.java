package com.trtc.uikit.livekit.livestream.view.audience;

import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.gift.service.GiftConstants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.DASHBOARD;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.PLAYING;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomInfoCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.component.barrage.BarrageInputView;
import com.trtc.uikit.livekit.component.barrage.BarrageStreamView;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.component.gift.GiftButton;
import com.trtc.uikit.livekit.component.gift.GiftPlayView;
import com.trtc.uikit.livekit.component.gift.LikeButton;
import com.trtc.uikit.livekit.component.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.gift.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.component.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestream.manager.observer.LiveStreamObserver;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestream.view.audience.dashboard.AudienceDashboardView;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.CoGuestPlaceHolderView;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.CoGuestRequestFloatView;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.dialog.CancelRequestDialog;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.dialog.StopCoGuestDialog;
import com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.dialog.TypeSelectDialog;
import com.trtc.uikit.livekit.livestream.view.widgets.battle.BattleInfoView;
import com.trtc.uikit.livekit.livestream.view.widgets.battle.BattleMemberInfoView;
import com.trtc.uikit.livekit.livestream.view.widgets.coguest.CoGuestWidgetsView;
import com.trtc.uikit.livekit.livestream.view.widgets.cohost.CoHostWidgetsView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;

@SuppressLint("ViewConstructor")
public class AudienceView extends BasicView {
    private       LiveCoreView                         mLiveCoreView;
    private       FrameLayout                          mLayoutPlaying;
    private       AudienceDashboardView                mAudienceDashboardView;
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
    private       CoGuestPlaceHolderView               mCoGuestPlaceHolderView;
    private       BattleInfoView                       mBattleInfoView;
    private final Observer<RoomState.LiveStatus>       mLiveStatusChangeObserver = this::onLiveStatusChange;
    private final Observer<CoGuestState.CoGuestStatus> mLinkStatusObserver       = this::onLinkStatusChange;
    private final Observer<UserState.UserInfo>         mEnterUserObserver        = this::onEnterUserChange;

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
        mRoomInfoView = findViewById(R.id.room_info_view);
        mAudienceListView = findViewById(R.id.audience_list_view);
        mBarrageStreamView = findViewById(R.id.barrage_stream_view);
        mWaitingCoGuestPassView = findViewById(R.id.btn_waiting_pass);
        mBattleInfoView = findViewById(R.id.battle_info_view);
        mGiftPlayView = findViewById(R.id.gift_play_view);
        mImageFloatWindow = findViewById(R.id.iv_float_window);
        mImageExitRoom = findViewById(R.id.iv_exit_room);
    }

    @Override
    protected void refreshView() {
        initLiveCoreView();
        initComponentView();
    }

    private void initLiveCoreView() {
        if (mLiveCoreView == null) {
            mLiveCoreView = FloatWindowManager.getInstance().getCoreView();
            if (mLiveCoreView == null) {
                mLiveCoreView = new LiveCoreView(ContextProvider.getApplicationContext());
                mLiveCoreView.registerConnectionObserver(new LiveStreamObserver(mLiveManager));
            } else {
                FloatWindowManager.getInstance().setCoreView(null);
            }
            FrameLayout frameLayout = findViewById(R.id.lsv_video_view_container);
            frameLayout.addView(mLiveCoreView);
        }
        mLiveCoreView.setVideoViewAdapter(new LiveCoreViewDefine.VideoViewAdapter() {
            @Override
            public View createCoGuestView(UserInfo userInfo) {
                Logger.info("initLiveStreamListView createCoGuestView:" + userInfo.userId);
                CoGuestWidgetsView coGuestWidgetsView = new CoGuestWidgetsView(getContext());
                coGuestWidgetsView.init(mLiveManager, userInfo);
                return coGuestWidgetsView;
            }

            @Override
            public void updateCoGuestView(UserInfo userInfo, View coGuestView) {

            }

            @Override
            public View createCoHostView(TUILiveConnectionManager.ConnectionUser connectionUser) {
                FrameLayout composeCoHostLayout = new FrameLayout(mContext);
                FrameLayout.LayoutParams layoutParams =
                        new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                                ViewGroup.LayoutParams.MATCH_PARENT);

                CoHostWidgetsView coHostWidgetsView = new CoHostWidgetsView(mContext);
                coHostWidgetsView.init(mLiveManager, connectionUser);
                composeCoHostLayout.addView(coHostWidgetsView, layoutParams);

                BattleMemberInfoView battleMemberInfoView = new BattleMemberInfoView(mContext);
                battleMemberInfoView.init(mLiveManager, connectionUser.userId);
                composeCoHostLayout.addView(battleMemberInfoView, layoutParams);
                return composeCoHostLayout;
            }

            @Override
            public void updateCoHostView(TUILiveConnectionManager.ConnectionUser connectionUser, View coHostView) {

            }
        });
        mLiveCoreView.joinLiveStream(mRoomState.roomId, new GetRoomInfoCallback() {
            @Override
            public void onSuccess(RoomInfo roomInfo) {
                mRoomManager.updateRoomState(roomInfo);
                mRoomState.liveStatus.set(PLAYING);
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

    private void initComponentView() {
        if (mRoomState.liveStatus.get() == PLAYING) {
            updatePlayingStatus();
        } else if (mRoomState.liveStatus.get() == DASHBOARD) {
            updateDashboardStatus();
        }
    }

    private void updatePlayingStatus() {
        mAudienceDashboardView.setVisibility(GONE);
        mLayoutPlaying.setVisibility(VISIBLE);
        if (mCoGuestState.coGuestStatus.get() == CoGuestState.CoGuestStatus.LINKING) {
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
        initGiftView();
        initLikeView();
        initGiftPlayView();
        initWaitingCoGuestPassView();
        initBattleInfoView();
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
        mImageExitRoom.setOnClickListener(view -> {
            mLiveCoreView.leaveLiveStream(null);
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        });
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

    private void initBarrageStreamView() {
        mBarrageStreamView.init(mRoomState.roomId, mRoomState.ownerInfo.userId);
        mBarrageStreamView.setItemTypeDelegate(new BarrageViewTypeDelegate());
        mBarrageStreamView.setItemAdapter(GIFT_VIEW_TYPE_1, new GiftBarrageAdapter(mContext));
    }

    private void initBarrageInputView() {
        mBarrageInputView.init(mRoomState.roomId);
    }

    private void initGiftView() {
        mButtonGift.init(mRoomState.roomId, mRoomState.ownerInfo.userId, mRoomState.ownerInfo.name.get(),
                mRoomState.ownerInfo.avatarUrl.get());
    }

    private void initLikeView() {
        mButtonLike.init(mRoomState.roomId);
    }

    private void initBattleInfoView() {
        mBattleInfoView.init(mLiveManager);
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
    }

    private void initCoGuestIcon() {
        mImageCoGuest.setImageResource(R.drawable.livekit_function_link_default);
        mImageCoGuest.setOnClickListener(view -> {
            TypeSelectDialog typeSelectDialog = new TypeSelectDialog(mContext, mLiveManager,
                    mLiveCoreView);
            typeSelectDialog.show();
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
        mCoGuestState.coGuestStatus.observe(mLinkStatusObserver);
        mRoomState.liveStatus.observe(mLiveStatusChangeObserver);
        mUserState.enterUserInfo.observe(mEnterUserObserver);
    }

    @Override
    protected void removeObserver() {
        mCoGuestState.coGuestStatus.removeObserver(mLinkStatusObserver);
        mRoomState.liveStatus.removeObserver(mLiveStatusChangeObserver);
        mUserState.enterUserInfo.removeObserver(mEnterUserObserver);
    }

    public void updateStatus(AudienceViewStatus status) {
        switch (status) {
            case CREATE:
                create();
                break;
            case START_DISPLAY:
                startDisPlay();
                break;
            case DISPLAY_COMPLETE:
                displayComplete();
                break;
            case END_DISPLAY:
                endDisplay();
                break;
            case DESTROY:
                destroy();
                break;
            default:
                break;
        }
    }

    private void create() {
    }

    private void startDisPlay() {
        mUserManager.muteAllRemoteAudio(true);
    }

    private void displayComplete() {
        mUserManager.muteAllRemoteAudio(false);
    }

    private void endDisplay() {
        mUserManager.muteAllRemoteAudio(true);
    }

    private void destroy() {
        mLiveCoreView.leaveLiveStream(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                removeAllViews();
                if (mContext instanceof Activity) {
                    ((Activity) mContext).finish();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    private void removeCoGuestPlaceHolderView() {
        if (mCoGuestPlaceHolderView != null
                && (mLiveCoreView.getVideoLayout().indexOfChild(mCoGuestPlaceHolderView) != -1)) {
            mLiveCoreView.getVideoLayout().removeView(mCoGuestPlaceHolderView);
            mCoGuestPlaceHolderView = null;
        }
    }

    private void addCoGuestPlaceHolderView() {
        if (mCoGuestPlaceHolderView == null) {
            mCoGuestPlaceHolderView = new CoGuestPlaceHolderView(mContext);
        }
        mLiveCoreView.getVideoLayout().addView(mCoGuestPlaceHolderView);
    }

    private void onLinkStatusChange(CoGuestState.CoGuestStatus linkStatus) {
        switch (linkStatus) {
            case NONE:
                mWaitingCoGuestPassView.setVisibility(GONE);
                removeCoGuestPlaceHolderView();
                initCoGuestIcon();
                break;
            case APPLYING:
                mWaitingCoGuestPassView.setVisibility(VISIBLE);
                addCoGuestPlaceHolderView();
                cancelCoGuestRequest();
                break;
            case LINKING:
                mWaitingCoGuestPassView.setVisibility(GONE);
                removeCoGuestPlaceHolderView();
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
            barrage.content = mContext.getString(R.string.livekit_entered_room);
            barrage.user.userId = userInfo.userId;
            barrage.user.userName = userInfo.name.get();
            barrage.user.avatarUrl = userInfo.avatarUrl.get();
            barrage.user.level = "0";
            mBarrageStreamView.insertBarrages(barrage);
        }
    }

    public enum AudienceViewStatus {
        CREATE,
        START_DISPLAY,
        DISPLAY_COMPLETE,
        END_DISPLAY,
        DESTROY,
    }
}
