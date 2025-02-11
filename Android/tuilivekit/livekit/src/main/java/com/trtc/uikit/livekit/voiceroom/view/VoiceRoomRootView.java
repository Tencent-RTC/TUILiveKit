package com.trtc.uikit.livekit.voiceroom.view;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_SUB_KEY_CLOSE_VOICE_ROOM;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.GIFT_COUNT;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.GIFT_NAME;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior.JOIN;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.component.barrage.BarrageStreamView;
import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.component.gift.GiftPlayView;
import com.trtc.uikit.component.gift.store.model.Gift;
import com.trtc.uikit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.error.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.manager.module.RoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.observer.SeatGridViewCoreObserver;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroom.view.basic.ConfirmDialog;
import com.trtc.uikit.livekit.voiceroom.view.basic.ExitConfirmDialog;
import com.trtc.uikit.livekit.voiceroom.view.bottommenu.BottomMenuView;
import com.trtc.uikit.livekit.voiceroom.view.dashboard.AnchorDashboardView;
import com.trtc.uikit.livekit.voiceroom.view.dashboard.AudienceDashboardView;
import com.trtc.uikit.livekit.voiceroom.view.preview.AnchorPreviewView;
import com.trtc.uikit.livekit.voiceroom.view.topview.TopView;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import java.util.Map;

public class VoiceRoomRootView extends FrameLayout implements ITUINotification {
    private static final String FILE = "VoiceRoomRootView";

    private final Context                           mContext;
    private       VoiceRoomManager                  mVoiceRoomManager;
    private       TUIVoiceRoomFragment.RoomBehavior mRoomBehavior;
    private       TUIVoiceRoomFragment.RoomParams   mRoomParams;
    private       boolean                           mIsAddObserver;

    private RelativeLayout    mLayoutEndViewContainer;
    private AnchorPreviewView mAnchorPreviewView;
    private TopView           mTopView;
    private SeatGridView      mSeatGridView;
    private BottomMenuView    mBottomMenuView;
    private ImageView         mRootBg;
    private BarrageStreamView mBarrageStreamView;
    private GiftPlayView      mGiftPlayView;
    private GiftCacheService  mGiftCacheService;
    private ConfirmDialog     mInvitationDialog;
    private ExitConfirmDialog mExitConfirmDialog;

    private SeatGridViewCoreObserver mSeatGridViewCoreObserver;

    private final Observer<String>                   mBackgroundURLObserver  = this::updateRoomBackground;
    private final Observer<RoomState.LiveStatus>     mLiveStateObserver      = this::onLiveStateChanged;
    private final Observer<UserState.UserInfo>       mEnterUserObserver      = this::onEnterUserChange;
    private final Observer<SeatState.SeatInvitation> mSeatInvitationObserver = this::onSeatInvitationChanged;
    private final Observer<SeatState.LinkStatus>     mLinkStateObserver      = this::onLinkStateChanged;

    public VoiceRoomRootView(@NonNull Context context) {
        this(context, null);
    }

    public VoiceRoomRootView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public VoiceRoomRootView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
    }

    public void init(VoiceRoomManager voiceRoomManager,
                     TUIVoiceRoomFragment.RoomBehavior behavior, TUIVoiceRoomFragment.RoomParams params) {
        mVoiceRoomManager = voiceRoomManager;
        mRoomBehavior = behavior;
        mRoomParams = params;
        mGiftCacheService = GiftStore.getInstance().mGiftCacheService;
        initView();
        if (!mIsAddObserver) {
            addObserver();
            mIsAddObserver = true;
        }
        enterRoom();
    }

    public void updateStatus(VoiceRoomViewStatus status) {
        if (JOIN != mRoomBehavior) {
            return;
        }
        switch (status) {
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

    private void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_root_view, this, true);
        bindViewId();
        initGiftView();
    }

    @Override
    protected void onAttachedToWindow() {
        Logger.info(FILE, "VoiceRoomRootView attached to window");
        super.onAttachedToWindow();
        if (mVoiceRoomManager == null) {
            return;
        }
        if (!mIsAddObserver) {
            addObserver();
            mIsAddObserver = true;
        }
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        Logger.info(FILE, "VoiceRoomRootView detached to window");
        if (mIsAddObserver) {
            removeObserver();
            mIsAddObserver = false;
        }
        removeObserver();
        mVoiceRoomManager.getState().reset();
    }

    private void bindViewId() {
        mRootBg = findViewById(R.id.root_bg);
        mTopView = findViewById(R.id.top_view);
        mSeatGridView = findViewById(R.id.seat_grid_view);
        mBottomMenuView = findViewById(R.id.bottom_menu);
        mBarrageStreamView = findViewById(R.id.barrage_stream_view);
        mGiftPlayView = findViewById(R.id.gift_play_view);
        mLayoutEndViewContainer = findViewById(R.id.rl_end_view);
        mAnchorPreviewView = findViewById(R.id.anchor_preview_view);
    }

    private void addObserver() {
        mVoiceRoomManager.getRoomState().backgroundURL.observe(mBackgroundURLObserver);
        mVoiceRoomManager.getRoomState().liveStatus.observe(mLiveStateObserver);
        mVoiceRoomManager.getUserState().enterUserInfo.observe(mEnterUserObserver);
        mVoiceRoomManager.getSeatState().receivedSeatInvitation.observe(mSeatInvitationObserver);
        mVoiceRoomManager.getSeatState().linkStatus.observe(mLinkStateObserver);
        mSeatGridViewCoreObserver = new SeatGridViewCoreObserver(mContext, mVoiceRoomManager, mSeatGridView);
        mSeatGridView.addObserver(mSeatGridViewCoreObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_VOICE_ROOM, this);
    }

    private void removeObserver() {
        mVoiceRoomManager.getRoomState().backgroundURL.removeObserver(mBackgroundURLObserver);
        mVoiceRoomManager.getRoomState().liveStatus.removeObserver(mLiveStateObserver);
        mVoiceRoomManager.getUserState().enterUserInfo.removeObserver(mEnterUserObserver);
        mVoiceRoomManager.getSeatState().receivedSeatInvitation.removeObserver(mSeatInvitationObserver);
        mVoiceRoomManager.getSeatState().linkStatus.removeObserver(mLinkStateObserver);
        mSeatGridView.removeObserver(mSeatGridViewCoreObserver);
        TUICore.unRegisterEvent(this);
    }

    private void showMainView() {
        mSeatGridView.setLayoutMode(VoiceRoomDefine.LayoutMode.GRID, null);
        mTopView.init(mVoiceRoomManager, mSeatGridView);
        mTopView.setVisibility(VISIBLE);
        mBottomMenuView.init(mVoiceRoomManager, mSeatGridView);
        mBottomMenuView.setVisibility(VISIBLE);
    }

    private void onLinkStateChanged(SeatState.LinkStatus status) {
        if (status == SeatState.LinkStatus.LINKING) {
            unmuteMicrophone();
            startMicrophone();
        }
    }

    private void unmuteMicrophone() {
        mSeatGridView.unmuteMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVoiceRoomManager.getMediaManager().updateMicrophoneMuteState(false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "unmuteMicrophone failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void startMicrophone() {
        mSeatGridView.startMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVoiceRoomManager.getMediaManager().updateMicrophoneOpenState(true);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "startMicrophone failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
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
        mAnchorPreviewView.init(mVoiceRoomManager, mSeatGridView);
        mAnchorPreviewView.setVisibility(VISIBLE);
    }

    private void removePreviewView() {
        mAnchorPreviewView.setVisibility(GONE);
    }

    private void initGiftView() {
        mGiftPlayView.init(mVoiceRoomManager.getRoomState().roomId);
        mGiftPlayView.setListener(new GiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(Gift gift, int giftCount, GiftUser sender, GiftUser receiver) {
                RoomManager roomManager = mVoiceRoomManager.getRoomManager();
                roomManager.updateGiftIncome(gift.price * giftCount
                        + mVoiceRoomManager.getRoomState().liveExtraInfo.giftIncome);
                roomManager.insertGiftPeople(sender.userId);
                if (mBarrageStreamView == null) {
                    return;
                }
                Barrage barrage = new Barrage();
                barrage.content = "gift";
                barrage.user.userId = sender.userId;
                barrage.user.userName = TextUtils.isEmpty(sender.userName) ? sender.userId : sender.userName;
                barrage.user.avatarUrl = sender.avatarUrl;
                barrage.user.level = sender.level;
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
                mGiftCacheService.request(gift.animationUrl, (error, result) -> {
                    if (error == 0) {
                        view.playGiftAnimation(result);
                    }
                });
            }
        });
    }

    private void initBarrageView() {
        mBarrageStreamView.init(mVoiceRoomManager.getRoomState().roomId,
                mVoiceRoomManager.getRoomState().ownerInfo.userId);
        mBarrageStreamView.setItemTypeDelegate(new BarrageViewTypeDelegate());
        mBarrageStreamView.setItemAdapter(GIFT_VIEW_TYPE_1, new GiftBarrageAdapter(mContext));
    }

    @SuppressLint("ClickableViewAccessibility")
    private void initAnchorEndView() {
        mLayoutEndViewContainer.removeAllViews();
        AnchorDashboardView anchorEndView = new AnchorDashboardView(mContext);
        anchorEndView.init(mVoiceRoomManager);
        anchorEndView.setOnTouchListener((v, event) -> true);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutEndViewContainer.addView(anchorEndView, layoutParams);
    }

    @SuppressLint("ClickableViewAccessibility")
    private void initAudienceEndView() {
        mLayoutEndViewContainer.removeAllViews();
        AudienceDashboardView audienceEndView = new AudienceDashboardView(mContext);
        audienceEndView.init(mVoiceRoomManager);
        audienceEndView.setOnTouchListener((v, event) -> true);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutEndViewContainer.addView(audienceEndView, layoutParams);
    }

    private void enterRoom() {
        switch (mRoomBehavior) {
            case AUTO_CREATE:
                mVoiceRoomManager.getRoomManager().initCreateRoomState(mVoiceRoomManager.getRoomState().roomId,
                        mRoomParams.roomName, mRoomParams.seatMode, mRoomParams.maxSeatCount);
                start();
                break;
            case JOIN:
                enter();
                break;
            case PREPARE_CREATE:
                mVoiceRoomManager.getRoomManager().initCreateRoomState(mVoiceRoomManager.getRoomState().roomId,
                        mRoomParams.roomName, mRoomParams.seatMode, mRoomParams.maxSeatCount);
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PREVIEWING);
                break;
            default:
                break;
        }
    }

    private void start() {
        TUIRoomDefine.RoomInfo roomInfo = new TUIRoomDefine.RoomInfo();
        roomInfo.roomType = TUIRoomDefine.RoomType.LIVE;
        roomInfo.isSeatEnabled = true;
        roomInfo.roomId = mVoiceRoomManager.getRoomState().roomId;
        roomInfo.name = mVoiceRoomManager.getRoomState().roomName.get();
        roomInfo.maxSeatCount = mVoiceRoomManager.getRoomState().maxSeatCount.get();
        roomInfo.seatMode = mVoiceRoomManager.getRoomState().seatMode.get();
        mSeatGridView.startVoiceRoom(roomInfo, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                Logger.info(FILE, "create room success");
                mVoiceRoomManager.getRoomManager().updateRoomState(roomInfo);
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PUSHING);
                mVoiceRoomManager.getRoomManager().updateLiveInfo();
                mVoiceRoomManager.getUserManager().getAudienceList();
                mVoiceRoomManager.getUserManager().updateOwnerUserInfo();
                mVoiceRoomManager.getSeatManager().getSeatList();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "create room failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void enter() {
        mSeatGridView.joinVoiceRoom(mVoiceRoomManager.getRoomState().roomId, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                Logger.info(FILE, "enter room success");
                mVoiceRoomManager.getRoomManager().updateRoomState(roomInfo);
                mVoiceRoomManager.getRoomManager().getLiveInfo(roomInfo.roomId);
                mVoiceRoomManager.getUserManager().getAudienceList();
                mVoiceRoomManager.getUserManager().updateOwnerUserInfo();
                mVoiceRoomManager.getSeatManager().getSeatList();
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PLAYING);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, " enter room failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.NONE);
                if (mContext instanceof Activity) {
                    ((Activity) mContext).finish();
                }
            }
        });
    }

    private void showEndView() {
        mLayoutEndViewContainer.removeAllViews();
        if (mInvitationDialog != null && mInvitationDialog.isShowing()) {
            mInvitationDialog.dismiss();
        }
        RoomManager roomManager = mVoiceRoomManager.getRoomManager();
        roomManager.updateLikeNumber(mGiftPlayView.getLikeCount());
        roomManager.updateMessageCount(mBarrageStreamView.getBarrageCount());
        if (mVoiceRoomManager.getUserState().selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            initAnchorEndView();
        } else {
            initAudienceEndView();
        }
    }

    private void exit() {
        RoomManager roomManager = mVoiceRoomManager.getRoomManager();
        roomManager.updateLikeNumber(mGiftPlayView.getLikeCount());
        roomManager.updateMessageCount(mBarrageStreamView.getBarrageCount());
        if (roomManager.isOwner()) {
            mSeatGridView.stopVoiceRoom(null);
            mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.DASHBOARD);
        } else {
            mSeatGridView.leaveVoiceRoom(null);
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        }
    }

    private void onLiveStateChanged(RoomState.LiveStatus status) {
        if (status == RoomState.LiveStatus.PUSHING || status == RoomState.LiveStatus.PLAYING) {
            initBarrageView();
            showMainView();
            removePreviewView();
        } else if (status == RoomState.LiveStatus.DASHBOARD) {
            showEndView();
        } else if (status == RoomState.LiveStatus.PREVIEWING) {
            showPreviewView();
        }
    }

    private void onEnterUserChange(UserState.UserInfo userInfo) {
        if (userInfo != null && mBarrageStreamView != null) {
            Barrage barrage = new Barrage();
            barrage.content = mContext.getString(R.string.livekit_entered_room);
            barrage.user.userId = userInfo.userId;
            barrage.user.userName = TextUtils.isEmpty(userInfo.name.get()) ? userInfo.userId : userInfo.name.get();
            barrage.user.avatarUrl = userInfo.avatarUrl.get();
            barrage.user.level = "0";
            mBarrageStreamView.insertBarrages(barrage);
        }
    }

    private void onSeatInvitationChanged(SeatState.SeatInvitation seatInvitation) {
        if (TextUtils.isEmpty(seatInvitation.userId)) {
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
                v -> responseSeatInvitation(seatInvitation.userId, true));

        mInvitationDialog.setNegativeText(mContext.getString(R.string.livekit_reject),
                v -> responseSeatInvitation(seatInvitation.userId, false));
        mInvitationDialog.setHeadIconUrl(seatInvitation.avatarUrl);
        mInvitationDialog.setContent(mContext.getString(R.string.livekit_voiceroom_receive_seat_invitation,
                seatInvitation.userName));
        mInvitationDialog.show();
    }

    private void responseSeatInvitation(String userId, boolean isAgree) {
        mSeatGridView.responseRemoteRequest(userId, isAgree, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVoiceRoomManager.getSeatManager().removeSeatApplication(userId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "responseSeatInvitation failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void displayComplete() {
        mBarrageStreamView.setVisibility(VISIBLE);
        mGiftPlayView.setVisibility(VISIBLE);
        mTopView.setVisibility(VISIBLE);
        mBottomMenuView.setVisibility(VISIBLE);
    }

    private void endDisplay() {
        mBarrageStreamView.setVisibility(GONE);
        mGiftPlayView.setVisibility(GONE);
        mTopView.setVisibility(GONE);
        mBottomMenuView.setVisibility(GONE);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_CLOSE_VOICE_ROOM.equals(subKey)) {
            if (mVoiceRoomManager.getUserState().selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
                showExitConfirmDialog();
            } else {
                exit();
            }
        }
    }

    private void showExitConfirmDialog() {
        if (mExitConfirmDialog == null) {
            mExitConfirmDialog = new ExitConfirmDialog(mContext, this::exit);
        }
        mExitConfirmDialog.show();
    }

    public enum VoiceRoomViewStatus {
        START_DISPLAY,
        DISPLAY_COMPLETE,
        END_DISPLAY,
    }
}
