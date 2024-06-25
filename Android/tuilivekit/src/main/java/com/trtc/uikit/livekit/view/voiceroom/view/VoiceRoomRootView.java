package com.trtc.uikit.livekit.view.voiceroom.view;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;
import static com.trtc.uikit.livekit.common.utils.Constants.DATA_REPORT_COMPONENT_VOICE_ROOM;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_COUNT;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_NAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.view.voiceroom.TUIVoiceRoomFragment.RoomBehavior.JOIN;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageDisplayView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.dashboard.AnchorDashboardView;
import com.trtc.uikit.livekit.common.uicomponent.dashboard.AudienceDashboardView;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftPlayView;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.RoomController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.view.voiceroom.TUIVoiceRoomFragment;
import com.trtc.uikit.livekit.state.operation.UserState;
import com.trtc.uikit.livekit.view.voiceroom.model.MenuDataGenerate;
import com.trtc.uikit.livekit.view.voiceroom.view.bottommenu.BottomMenuView;
import com.trtc.uikit.livekit.view.voiceroom.view.preview.AnchorPreviewView;
import com.trtc.uikit.livekit.view.voiceroom.view.seatview.SeatListView;
import com.trtc.uikit.livekit.view.voiceroom.view.topview.TopView;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

@SuppressLint("ViewConstructor")
public class VoiceRoomRootView extends BasicView {
    private       RelativeLayout                              mLayoutTopViewContainer;
    private       RelativeLayout                              mLayoutSeatListContainer;
    private       RelativeLayout                              mLayoutBottomMenuContainer;
    private       RelativeLayout                              mLayoutBarrageContainer;
    private       RelativeLayout                              mLayoutAnchorPreviewViewContainer;
    private       RelativeLayout                              mLayoutGiftContainer;
    private       RelativeLayout                              mLayoutEndViewContainer;
    private       ImageView                                   mRootBg;
    private       TUIBarrageDisplayView                       mBarrageDisplayView;
    private       TUIGiftPlayView                             mGiftPlayView;
    private final GiftCacheService                            mGiftCacheService;
    private final TUIVoiceRoomFragment.RoomBehavior           mRoomBehavior;
    private final TUIVoiceRoomFragment.RoomParams             mRoomParams;
    private final Observer<String>                            mCoverURLObserver  = this::updateRoomCover;
    private final Observer<LiveDefine.LiveStatus>             mLiveStateObserver = this::onLiveStateChanged;
    private final Observer<LiveDefine.NavigationStatus>       mRouteObserver     = this::onNavigationStatusChange;
    private final Set<String>                                 mUserIdCache       = new HashSet<>();
    private final Observer<LinkedHashSet<UserState.UserInfo>> mUserListObserver  = this::onUserListChange;

    public VoiceRoomRootView(@NonNull Context context, LiveController liveController,
                             TUIVoiceRoomFragment.RoomBehavior behavior, TUIVoiceRoomFragment.RoomParams params) {
        super(context, liveController);
        mRoomBehavior = behavior;
        mRoomParams = params;
        mGiftCacheService = new GiftCacheService(context);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_root_view, this, true);
        bindViewId();

        initSeatListView();
        initBarrageView();
        initGiftView();
        initAnchorPreviewView(mViewState.liveStatus.get());
        updateRoomCover(mRoomState.coverURL.get());
    }

    private void bindViewId() {
        mRootBg = findViewById(R.id.root_bg);
        mLayoutAnchorPreviewViewContainer = findViewById(R.id.rl_anchor_preview_view);
        mLayoutTopViewContainer = findViewById(R.id.rl_top_view);
        mLayoutSeatListContainer = findViewById(R.id.rl_seat_list);
        mLayoutBottomMenuContainer = findViewById(R.id.rl_bottom_menu);
        mLayoutBarrageContainer = findViewById(R.id.rl_barrage);
        mLayoutGiftContainer = findViewById(R.id.rl_gift);
        mLayoutEndViewContainer = findViewById(R.id.rl_end_view);
    }

    @Override
    protected void addObserver() {
        mRoomState.coverURL.observe(mCoverURLObserver);
        mViewState.currentNavigationState.observe(mRouteObserver);
        mViewState.liveStatus.observe(mLiveStateObserver);
        mUserState.userList.observe(mUserListObserver);
    }

    @Override
    protected void onAttachedToWindow() {
        enterRoom();
        super.onAttachedToWindow();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mGiftCacheService.release();
        mLiveController.getState().reset();
    }

    @Override
    protected void removeObserver() {
        mRoomState.coverURL.removeObserver(mCoverURLObserver);
        mViewState.currentNavigationState.removeObserver(mRouteObserver);
        mViewState.liveStatus.removeObserver(mLiveStateObserver);
        mUserState.userList.removeObserver(mUserListObserver);
    }

    public void updateStatus(VoiceRoomViewStatus status) {
        if (JOIN != mRoomBehavior) {
            return;
        }
        switch (status) {
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
                exit();
                break;
            default:
                break;
        }
    }

    private void initTopView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutTopViewContainer.removeAllViews();
        TopView topView = new TopView(mContext, mLiveController);
        mLayoutTopViewContainer.addView(topView, layoutParams);
    }

    private void initSeatListView() {
        ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(MATCH_PARENT, WRAP_CONTENT);
        mLayoutSeatListContainer.removeAllViews();
        SeatListView seatListView = new SeatListView(mContext, mLiveController);
        mLayoutSeatListContainer.addView(seatListView, layoutParams);
    }

    private void initBottomMenuView() {
        mLayoutBottomMenuContainer.removeAllViews();
        MenuDataGenerate generate = new MenuDataGenerate(mContext, mLiveController);
        ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        BottomMenuView bottomMenuView = new BottomMenuView(mContext, mLiveController,
                generate.generateBottomMenuData());
        mLayoutBottomMenuContainer.addView(bottomMenuView, layoutParams);
    }

    private void updateRoomCover(String url) {
        if (mContext == null) {
            return;
        }
        if (mContext instanceof Activity && ((Activity) mContext).isDestroyed()) {
            return;
        }
        ImageLoader.load(mContext, mRootBg, url, R.drawable.livekit_voiceroom_cover);
    }

    private void initAnchorPreviewView(LiveDefine.LiveStatus status) {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        AnchorPreviewView anchorPreviewView;
        if (status == LiveDefine.LiveStatus.PREVIEWING) {
            mLayoutAnchorPreviewViewContainer.removeAllViews();
            anchorPreviewView = new AnchorPreviewView(mContext, mLiveController);
            mLayoutAnchorPreviewViewContainer.addView(anchorPreviewView, layoutParams);
        } else {
            mLayoutAnchorPreviewViewContainer.removeAllViews();
        }
    }

    private void initGiftView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutGiftContainer.removeAllViews();
        mGiftPlayView = new TUIGiftPlayView(mContext, mLiveController.getRoomSate().roomId);
        mLayoutGiftContainer.addView(mGiftPlayView, layoutParams);
        mGiftPlayView.setListener(new TUIGiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver) {
                RoomController roomController = mLiveController.getRoomController();
                roomController.updateGiftIncome(gift.price * giftCount + mRoomState.liveExtraInfo.giftIncome);
                roomController.insertGiftPeople(sender.userId);
                if (mBarrageDisplayView == null) {
                    return;
                }
                TUIBarrage barrage = new TUIBarrage();
                barrage.content = "gift";
                barrage.user.userId = sender.userId;
                barrage.user.userName = sender.userName;
                barrage.user.avatarUrl = sender.avatarUrl;
                barrage.user.level = sender.level;
                barrage.extInfo.put(Constants.GIFT_VIEW_TYPE, GIFT_VIEW_TYPE_1);
                barrage.extInfo.put(GIFT_NAME, gift.giftName);
                barrage.extInfo.put(GIFT_COUNT, giftCount);
                barrage.extInfo.put(GIFT_ICON_URL, gift.imageUrl);
                barrage.extInfo.put(GIFT_RECEIVER_USERNAME, receiver.userName);
                mBarrageDisplayView.insertBarrages(barrage);
            }

            @Override
            public void onPlayGiftAnimation(TUIGiftPlayView view, TUIGift gift) {
                mGiftCacheService.request(gift.animationUrl, (error, result) -> {
                    if (error == 0) {
                        view.playGiftAnimation(result);
                    }
                });
            }
        });
    }

    private void initBarrageView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutBarrageContainer.removeAllViews();
        mBarrageDisplayView = new TUIBarrageDisplayView(mContext, mLiveController.getRoomSate().roomId,
                mLiveController.getRoomSate().ownerInfo.userId);
        mLayoutBarrageContainer.addView(mBarrageDisplayView, layoutParams);
        mBarrageDisplayView.setAdapter(new GiftBarrageAdapter(mContext));
    }

    private void initAnchorEndView() {
        mLayoutEndViewContainer.removeAllViews();
        AnchorDashboardView anchorEndView = new AnchorDashboardView(mContext, mLiveController);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutEndViewContainer.addView(anchorEndView, layoutParams);
    }

    private void initAudienceEndView() {
        mLayoutEndViewContainer.removeAllViews();
        AudienceDashboardView audienceEndView = new AudienceDashboardView(mContext, mLiveController);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutEndViewContainer.addView(audienceEndView, layoutParams);
    }

    private void enterRoom() {
        mLiveController.getViewController().enableCamera(false);
        Constants.DATA_REPORT_COMPONENT = DATA_REPORT_COMPONENT_VOICE_ROOM;
        switch (mRoomBehavior) {
            case AUTO_CREATE:
                mLiveController.getRoomController().initCreateRoomState(mRoomState.roomId, mRoomParams.roomName,
                        mRoomParams.seatMode, mRoomParams.maxSeatCount);
                mLiveController.getRoomController().start();
                break;
            case JOIN:
                mLiveController.getRoomController().join(mRoomState.roomId);
                break;
            case PREPARE_CREATE:
                mLiveController.getRoomController().initCreateRoomState(mRoomState.roomId, mRoomParams.roomName,
                        mRoomParams.seatMode, mRoomParams.maxSeatCount);
                mLiveController.getRoomController().startPreview();
                break;
            default:
                break;
        }
    }

    private void showEndView() {
        mLayoutEndViewContainer.removeAllViews();
        RoomController roomController = mLiveController.getRoomController();
        roomController.updateLikeNumber(mGiftPlayView.getLikeCount());
        roomController.updateMessageCount(mBarrageDisplayView.getBarrageCount());
        if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            initAnchorEndView();
        } else {
            initAudienceEndView();
        }
    }

    private void exit() {
        RoomController roomController = mLiveController.getRoomController();
        roomController.updateLikeNumber(mGiftPlayView.getLikeCount());
        roomController.updateMessageCount(mBarrageDisplayView.getBarrageCount());
        roomController.exit();
    }

    private void onLiveStateChanged(LiveDefine.LiveStatus status) {
        if (status == LiveDefine.LiveStatus.PUSHING || status == LiveDefine.LiveStatus.PLAYING) {
            initBottomMenuView();
            initTopView();
            initAnchorPreviewView(status);
        } else if (status == LiveDefine.LiveStatus.DASHBOARD) {
            showEndView();
        }
    }

    private void onNavigationStatusChange(LiveDefine.NavigationStatus navigationStatus) {
        if (navigationStatus == LiveDefine.NavigationStatus.EXIT) {
            exit();
        }
    }

    private void onUserListChange(LinkedHashSet<UserState.UserInfo> list) {
        Set<String> userIds = new HashSet<>();
        for (UserState.UserInfo userInfo : list) {
            String userId = userInfo.userId;
            userIds.add(userId);
            if (mBarrageDisplayView != null && !mUserIdCache.contains(userId)) {
                TUIBarrage barrage = new TUIBarrage();
                barrage.content = mContext.getString(R.string.livekit_entered_room);
                barrage.user.userId = userInfo.userId;
                barrage.user.userName = userInfo.name.get();
                barrage.user.avatarUrl = userInfo.avatarUrl.get();
                barrage.user.level = "0";
                mBarrageDisplayView.insertBarrages(barrage);
            }
        }
        mUserIdCache.clear();
        mUserIdCache.addAll(userIds);
    }

    private void startDisPlay() {
        mUserController.muteAllRemoteAudio(true);
    }

    private void displayComplete() {
        mUserController.muteAllRemoteAudio(false);
        mLayoutBarrageContainer.setVisibility(VISIBLE);
        mLayoutGiftContainer.setVisibility(VISIBLE);
        mLayoutTopViewContainer.setVisibility(VISIBLE);
        mLayoutBottomMenuContainer.setVisibility(VISIBLE);
    }

    private void endDisplay() {
        mUserController.muteAllRemoteAudio(true);
        mLayoutBarrageContainer.setVisibility(GONE);
        mLayoutGiftContainer.setVisibility(GONE);
        mLayoutTopViewContainer.setVisibility(GONE);
        mLayoutBottomMenuContainer.setVisibility(GONE);
    }

    public enum VoiceRoomViewStatus {
        START_DISPLAY,
        DISPLAY_COMPLETE,
        END_DISPLAY,
        DESTROY,
    }
}
