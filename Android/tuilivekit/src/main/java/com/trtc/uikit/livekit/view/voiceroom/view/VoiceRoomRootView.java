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
import static com.trtc.uikit.livekit.view.voiceroom.view.VoiceRoomRootView.VoiceRoomViewStatus.DESTROY;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
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
import com.trtc.uikit.livekit.common.uicomponent.gift.store.GiftStore;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.RoomController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;
import com.trtc.uikit.livekit.view.voiceroom.TUIVoiceRoomFragment;
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
    private final Observer<String>                            mBackgroundURLObserver  = this::updateRoomBackground;
    private final Observer<LiveDefine.LiveStatus>             mLiveStateObserver      = this::onLiveStateChanged;
    private final Set<String>                                 mUserIdCache            = new HashSet<>();
    private final Observer<LinkedHashSet<UserState.UserInfo>> mUserListObserver       = this::onUserListChange;
    private final Observer<SeatState.SeatInvitation>          mSeatInvitationObserver = this::onSeatInvitationChanged;

    private ConfirmDialog mInvitationDialog;

    public VoiceRoomRootView(@NonNull Context context, LiveController liveController,
                             TUIVoiceRoomFragment.RoomBehavior behavior, TUIVoiceRoomFragment.RoomParams params) {
        super(context, liveController);
        mRoomBehavior = behavior;
        mRoomParams = params;
        mGiftCacheService = GiftStore.getInstance().mGiftCacheService;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_root_view, this, true);
        bindViewId();

        initBarrageView();
        initGiftView();
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
        mRoomState.backgroundURL.observe(mBackgroundURLObserver);
        mViewState.liveStatus.observe(mLiveStateObserver);
        mUserState.userList.observe(mUserListObserver);
        mSeatState.receivedSeatInvitation.observe(mSeatInvitationObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.backgroundURL.removeObserver(mBackgroundURLObserver);
        mViewState.liveStatus.removeObserver(mLiveStateObserver);
        mUserState.userList.removeObserver(mUserListObserver);
        mSeatState.receivedSeatInvitation.removeObserver(mSeatInvitationObserver);
    }

    @Override
    protected void onAttachedToWindow() {
        LiveKitLog.info("VoiceRoomRootView attached to window");
        enterRoom();
        super.onAttachedToWindow();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        LiveKitLog.info("VoiceRoomRootView detached to window");
        mLiveController.getState().reset();
    }

    public void updateStatus(VoiceRoomViewStatus status) {
        if (DESTROY == status) {
            exit();
            return;
        }
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
        SeatListView.Config config = new SeatListView.Config();
        config.isPreview = false;
        SeatListView seatListView = new SeatListView(mContext, mLiveController, config);
        mLayoutSeatListContainer.addView(seatListView, layoutParams);
    }

    private void initBottomMenuView() {
        mLayoutBottomMenuContainer.removeAllViews();
        BottomMenuView bottomMenuView = new BottomMenuView(mContext, mLiveController);
        ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(MATCH_PARENT, WRAP_CONTENT);
        mLayoutBottomMenuContainer.addView(bottomMenuView, layoutParams);
    }

    private void updateRoomBackground(String url) {
        if (mContext == null) {
            return;
        }
        if (mContext instanceof Activity && ((Activity) mContext).isDestroyed()) {
            return;
        }
        ImageLoader.load(mContext, mRootBg, url, R.drawable.livekit_voiceroom_bg);
    }

    private void showPreviewView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        AnchorPreviewView anchorPreviewView;
        mLayoutAnchorPreviewViewContainer.removeAllViews();
        anchorPreviewView = new AnchorPreviewView(mContext, mLiveController);
        mLayoutAnchorPreviewViewContainer.addView(anchorPreviewView, layoutParams);
    }

    private void removePreviewView() {
        mLayoutAnchorPreviewViewContainer.removeAllViews();
    }

    private void initGiftView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutGiftContainer.removeAllViews();
        mGiftPlayView = new TUIGiftPlayView(mContext, mLiveController.getRoomState().roomId);
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
        mBarrageDisplayView = new TUIBarrageDisplayView(mContext, mLiveController.getRoomState().roomId,
                mLiveController.getRoomState().ownerInfo.userId);
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
        mLiveController.getViewController().enableAutoOpenCameraOnSeated(false);
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
        if (mInvitationDialog != null && mInvitationDialog.isShowing()) {
            mInvitationDialog.dismiss();
        }
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
            initSeatListView();
            removePreviewView();
        } else if (status == LiveDefine.LiveStatus.DASHBOARD) {
            showEndView();
        } else if (status == LiveDefine.LiveStatus.PREVIEWING) {
            showPreviewView();
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

    private void onSeatInvitationChanged(SeatState.SeatInvitation seatInvitation) {
        if (TextUtils.isEmpty(seatInvitation.id)) {
            if (mInvitationDialog != null && mInvitationDialog.isShowing()) {
                mInvitationDialog.dismiss();
            }
        } else {
            showInvitationDialog(seatInvitation);
        }
    }

    private void showInvitationDialog(SeatState.SeatInvitation seatInvitation) {
        if (mInvitationDialog == null) {
            mInvitationDialog = new ConfirmDialog(mContext);
        }
        mInvitationDialog.setPositiveText(mContext.getString(R.string.livekit_accept),
                v -> mSeatController.responseSeatInvitation(true, seatInvitation.id));
        mInvitationDialog.setNegativeText(mContext.getString(R.string.livekit_reject),
                v -> mSeatController.responseSeatInvitation(false, seatInvitation.id));
        mInvitationDialog.setHeadIconUrl(seatInvitation.avatarUrl);
        mInvitationDialog.setContent(mContext.getString(R.string.livekit_voiceroom_receive_seat_invitation,
                seatInvitation.userName));
        mInvitationDialog.show();
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
