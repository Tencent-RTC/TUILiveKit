package com.trtc.uikit.livekit.voiceroom.view;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static com.trtc.uikit.livekit.common.ConstantsKt.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.ConstantsKt.EVENT_SUB_KEY_CLOSE_VOICE_ROOM;
import static com.trtc.uikit.livekit.common.ConstantsKt.TEMPLATE_ID_VOICE_ROOM;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_COUNT;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_NAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.voiceroom.state.RoomState.LayoutType.KTVRoom;
import static com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior.JOIN;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.permission.PermissionCallback;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.PermissionRequest;
import com.trtc.uikit.livekit.component.barrage.BarrageStreamView;
import com.trtc.uikit.livekit.component.gift.GiftPlayView;
import com.trtc.uikit.livekit.component.giftaccess.service.GiftCacheService;
import com.trtc.uikit.livekit.component.giftaccess.store.GiftStore;
import com.trtc.uikit.livekit.component.giftaccess.view.BarrageViewTypeDelegate;
import com.trtc.uikit.livekit.component.giftaccess.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.RoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.observer.SeatGridViewCoreObserver;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.view.basic.ConfirmDialog;
import com.trtc.uikit.livekit.voiceroom.view.basic.ExitConfirmDialog;
import com.trtc.uikit.livekit.voiceroom.view.basic.ExitSeatDialog;
import com.trtc.uikit.livekit.voiceroom.view.bottommenu.BottomMenuView;
import com.trtc.uikit.livekit.voiceroom.view.dashboard.AnchorDashboardView;
import com.trtc.uikit.livekit.voiceroom.view.dashboard.AudienceDashboardView;
import com.trtc.uikit.livekit.voiceroom.view.preview.AnchorPreviewView;
import com.trtc.uikit.livekit.voiceroom.view.topview.TopView;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import java.util.HashMap;
import java.util.Map;

import io.trtc.tuikit.atomicx.karaoke.KaraokeControlView;
import io.trtc.tuikit.atomicx.karaoke.KaraokeFloatingView;
import io.trtc.tuikit.atomicxcore.api.Barrage;
import io.trtc.tuikit.atomicxcore.api.Gift;
import io.trtc.tuikit.atomicxcore.api.LiveUserInfo;

public class VoiceRoomRootView extends FrameLayout implements ITUINotification {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("VoiceRoomRootView");

    private final Context                           mContext;
    private       VoiceRoomManager                  mVoiceRoomManager;
    private       TUIVoiceRoomFragment.RoomBehavior mRoomBehavior;
    private       TUIVoiceRoomFragment.RoomParams   mRoomParams;
    private       boolean                           mIsAddObserver;

    private RelativeLayout    mLayoutEndViewContainer;
    private AnchorPreviewView mAnchorPreviewView;
    private TopView           mTopView;
    private FrameLayout       mSeatGridContainer;
    private SeatGridView      mSeatGridView;
    private BottomMenuView    mBottomMenuView;
    private ImageView         mRootBg;
    private BarrageStreamView mBarrageStreamView;
    private GiftPlayView      mGiftPlayView;
    private GiftCacheService  mGiftCacheService;
    private ConfirmDialog     mInvitationDialog;
    private ExitConfirmDialog mExitConfirmDialog;
    private ExitSeatDialog    mExitSeatDialog;

    private SeatGridViewCoreObserver mSeatGridViewCoreObserver;

    private final Observer<String>                   mBackgroundURLObserver   = this::updateRoomBackground;
    private final Observer<RoomState.LiveStatus>     mLiveStateObserver       = this::onLiveStateChanged;
    private final Observer<RoomState.LayoutType>     mVoiceRoomLayoutObserver = this::onVoiceRoomLayoutChanged;
    private final Observer<SeatState.SeatInvitation> mSeatInvitationObserver  = this::onSeatInvitationChanged;
    private final Observer<SeatState.LinkStatus>     mLinkStateObserver       = this::onLinkStateChanged;
    private       KaraokeFloatingView                mKaraokeFloatingView;
    private       KaraokeControlView                 mKaraokeControlView;
    private       ViewGroup                          mLayoutRoot;

    private static final String EVENT_KEY_TIME_LIMIT          = "RTCRoomTimeLimitService";
    private static final String EVENT_SUB_KEY_COUNTDOWN_START = "CountdownStart";
    private static final String EVENT_SUB_KEY_COUNTDOWN_END   = "CountdownEnd";

    public VoiceRoomRootView(@NonNull Context context) {
        this(context, null);
    }

    public VoiceRoomRootView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public VoiceRoomRootView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_root_view, this, true);
        bindViewId();
    }

    public void init(VoiceRoomManager voiceRoomManager, TUIVoiceRoomFragment.RoomBehavior behavior,
                     TUIVoiceRoomFragment.RoomParams params) {
        mVoiceRoomManager = voiceRoomManager;
        mRoomBehavior = behavior;
        mRoomParams = params;
        mGiftCacheService = GiftStore.getInstance().mGiftCacheService;
        mSeatGridView = new SeatGridView(mContext);
        mSeatGridContainer.removeAllViews();
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mSeatGridContainer.addView(mSeatGridView, layoutParams);
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
        initGiftView();
    }

    @Override
    protected void onAttachedToWindow() {
        LOGGER.info("VoiceRoomRootView attached to window");
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
        LOGGER.info("VoiceRoomRootView detached to window");
        if (mIsAddObserver) {
            removeObserver();
            mIsAddObserver = false;
        }
        removeObserver();
        mVoiceRoomManager.getState().reset();
    }

    private void bindViewId() {
        mLayoutRoot = findViewById(R.id.cl_root);
        mRootBg = findViewById(R.id.root_bg);
        mTopView = findViewById(R.id.top_view);
        mSeatGridContainer = findViewById(R.id.seat_grid_container);
        mBottomMenuView = findViewById(R.id.bottom_menu);
        mBarrageStreamView = findViewById(R.id.barrage_stream_view);
        mGiftPlayView = findViewById(R.id.gift_play_view);
        mLayoutEndViewContainer = findViewById(R.id.rl_end_view);
        mAnchorPreviewView = findViewById(R.id.anchor_preview_view);
        mKaraokeControlView = findViewById(R.id.ktv_view);
        mKaraokeFloatingView = new KaraokeFloatingView(mContext);
    }

    private void addObserver() {
        mVoiceRoomManager.getRoomState().backgroundURL.observeForever(mBackgroundURLObserver);
        mVoiceRoomManager.getRoomState().liveStatus.observeForever(mLiveStateObserver);
        mVoiceRoomManager.getRoomState().layoutType.observeForever(mVoiceRoomLayoutObserver);
        mVoiceRoomManager.getSeatState().receivedSeatInvitation.observeForever(mSeatInvitationObserver);
        mVoiceRoomManager.getSeatState().linkStatus.observeForever(mLinkStateObserver);
        mSeatGridViewCoreObserver = new SeatGridViewCoreObserver(mContext, mVoiceRoomManager, mSeatGridView);
        mSeatGridView.addObserver(mSeatGridViewCoreObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_VOICE_ROOM, this);
    }

    private void removeObserver() {
        mVoiceRoomManager.getRoomState().backgroundURL.removeObserver(mBackgroundURLObserver);
        mVoiceRoomManager.getRoomState().liveStatus.removeObserver(mLiveStateObserver);
        mVoiceRoomManager.getRoomState().layoutType.observeForever(mVoiceRoomLayoutObserver);
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
        mKaraokeFloatingView.init(mVoiceRoomManager.getRoomState().roomId,
                mVoiceRoomManager.getRoomManager().isOwner());
        mKaraokeControlView.init(mVoiceRoomManager.getRoomState().roomId, mVoiceRoomManager.getRoomManager().isOwner());
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
                LOGGER.error("unmuteMicrophone failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void startMicrophone() {
        if (Boolean.TRUE.equals(mVoiceRoomManager.getMediaState().hasMicrophonePermission.getValue())) {
            startMicrophoneInternal();
            return;
        }
        PermissionRequest.requestMicrophonePermissions(getContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                LOGGER.info("requestMicrophonePermissions");
            }

            @Override
            public void onGranted() {
                LOGGER.info("requestMicrophonePermissions:[onGranted]");
                startMicrophoneInternal();
            }

            @Override
            public void onDenied() {
                LOGGER.info("onDenied");
            }
        });

    }

    private void startMicrophoneInternal() {
        mSeatGridView.startMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVoiceRoomManager.getMediaManager().updateMicrophoneOpenState(true);
                mSeatGridView.unmuteMicrophone(null);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("startMicrophone failed, error: " + error + ", message: " + message);
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
                TUIRoomDefine.UserInfo ownerInfo = mVoiceRoomManager.getRoomState().ownerInfo;
                String receiverName = TextUtils.isEmpty(ownerInfo.userName) ? ownerInfo.userId : ownerInfo.userName;
                if (TextUtils.equals(ownerInfo.userId, TUILogin.getUserId())) {
                    receiverName = getContext().getString(R.string.common_gift_me);
                }
                extInfo.put(GIFT_RECEIVER_USERNAME, receiverName);
                barrage.setExtensionInfo(extInfo);
                mBarrageStreamView.insertBarrages(barrage);
            }

            @Override
            public void onPlayGiftAnimation(GiftPlayView view, @NonNull Gift gift) {
                mGiftCacheService.request(gift.getResourceURL(), (error, result) -> {
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
        RoomState roomState = mVoiceRoomManager.getRoomState();
        TUILiveListManager.LiveInfo liveInfo = new TUILiveListManager.LiveInfo();
        liveInfo.isSeatEnabled = true;
        liveInfo.keepOwnerOnSeat = true;
        liveInfo.seatLayoutTemplateId = TEMPLATE_ID_VOICE_ROOM;
        liveInfo.roomId = roomState.roomId;
        liveInfo.name = roomState.roomName.getValue();
        liveInfo.maxSeatCount = roomState.maxSeatCount.getValue() == null ? 9 : roomState.maxSeatCount.getValue();
        liveInfo.seatMode = roomState.seatMode.getValue();
        liveInfo.backgroundUrl = roomState.backgroundURL.getValue();
        liveInfo.coverUrl = roomState.coverURL.getValue();
        liveInfo.isPublicVisible =
                roomState.liveExtraInfo.liveMode.getValue() == RoomState.LiveStreamPrivacyStatus.PUBLIC;
        mSeatGridView.startVoiceRoom(liveInfo, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfo liveInfo) {
                LOGGER.info("create room success");
                mVoiceRoomManager.getRoomManager().updateRoomState(liveInfo);
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PUSHING);
                mVoiceRoomManager.getUserManager().getAudienceList();
                mVoiceRoomManager.getUserManager().updateOwnerUserInfo();
                mVoiceRoomManager.getSeatManager().getSeatList();
                TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_START, null);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("create room failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void enter() {
        mSeatGridView.joinVoiceRoom(mVoiceRoomManager.getRoomState().roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfo liveInfo) {
                LOGGER.info("enter room success");
                mVoiceRoomManager.getRoomManager().updateRoomState(liveInfo);
                mVoiceRoomManager.getRoomManager().getLiveInfo(liveInfo.roomId);
                mVoiceRoomManager.getUserManager().getAudienceList();
                mVoiceRoomManager.getUserManager().updateOwnerUserInfo();
                mVoiceRoomManager.getSeatManager().getSeatList();
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PLAYING);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("enter room failed, error: " + error + ", message: " + message);
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
        roomManager.updateMessageCount(mBarrageStreamView.getBarrageCount());
        if (mVoiceRoomManager.getUserState().selfInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER) {
            initAnchorEndView();
        } else {
            initAudienceEndView();
        }
    }

    private void hideKTVView() {
        mKaraokeControlView.setVisibility(GONE);
        mKaraokeFloatingView.detachFromFloating();
    }

    private void exit() {
        RoomManager roomManager = mVoiceRoomManager.getRoomManager();
        roomManager.updateMessageCount(mBarrageStreamView.getBarrageCount());
        TUICore.notifyEvent(
                TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED, TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_STOP, null
        );
        TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_END, null);
        if (roomManager.isOwner()) {
            mSeatGridView.stopVoiceRoom(new TUILiveListManager.StopLiveCallback() {
                @Override
                public void onSuccess(TUILiveListManager.LiveStatisticsData data) {
                    mVoiceRoomManager.getRoomManager().updateLiveStatisticsData(data);
                    mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.DASHBOARD);
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LOGGER.error("stopVoiceRoom onError:error:" + error + ", errorCode:" + error.getValue() + ", " +
                            "message:" + message);
                    if (mContext instanceof Activity) {
                        ((Activity) mContext).finish();
                    }
                }
            });
        } else {
            mSeatGridView.leaveVoiceRoom(null);
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        }
        mKaraokeFloatingView.release();
        mKaraokeControlView.release();
    }

    private boolean checkActivityStatus() {
        Activity activity = (Activity) mContext;
        return !activity.isFinishing() && !activity.isDestroyed();
    }

    private void onLiveStateChanged(RoomState.LiveStatus status) {
        if (status == RoomState.LiveStatus.PUSHING || status == RoomState.LiveStatus.PLAYING) {
            initBarrageView();
            showMainView();
            removePreviewView();
            mVoiceRoomManager.getRoomManager().setVoiceRoomLayout(mVoiceRoomManager.getRoomState().layoutType.getValue());
            mVoiceRoomManager.getRoomManager().updateVoiceRoomLayout();
        } else if (status == RoomState.LiveStatus.DASHBOARD) {
            showEndView();
            hideKTVView();
        } else if (status == RoomState.LiveStatus.PREVIEWING) {
            showPreviewView();
            hideKTVView();
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

    private void onVoiceRoomLayoutChanged(RoomState.LayoutType layoutType) {
        if (mVoiceRoomManager.getRoomState().liveStatus.getValue() == RoomState.LiveStatus.PUSHING ||
                mVoiceRoomManager.getRoomState().liveStatus.getValue() == RoomState.LiveStatus.PLAYING) {
            if (KTVRoom == layoutType) {
                mKaraokeControlView.setVisibility(VISIBLE);
                mKaraokeFloatingView.detachFromFloating();
            } else {
                mKaraokeFloatingView.attachAsFloating(mLayoutRoot, KaraokeFloatingView.FloatingMode.RIGHT_HALF_MOVE);
                mKaraokeControlView.setVisibility(GONE);
            }
        }
    }

    private void showInvitationDialog(SeatState.SeatInvitation seatInvitation) {
        if (mInvitationDialog == null) {
            mInvitationDialog = new ConfirmDialog(mContext);
        }
        mInvitationDialog.setPositiveText(mContext.getString(R.string.common_receive),
                v -> responseSeatInvitation(seatInvitation.userId, true));

        mInvitationDialog.setNegativeText(mContext.getString(R.string.common_reject),
                v -> responseSeatInvitation(seatInvitation.userId, false));
        mInvitationDialog.setHeadIconUrl(seatInvitation.avatarUrl);
        mInvitationDialog.setContent(mContext.getString(R.string.common_voiceroom_receive_seat_invitation,
                TextUtils.isEmpty(mVoiceRoomManager.getRoomState().ownerInfo.userName) ?
                        mVoiceRoomManager.getRoomState().ownerInfo.userId :
                        mVoiceRoomManager.getRoomState().ownerInfo.userName));
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
                LOGGER.error("responseSeatInvitation failed, error: " + error + ", message: " + message);
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
            if (mVoiceRoomManager.getUserState().selfInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER) {
                showExitConfirmDialog();
            } else if (mVoiceRoomManager.getSeatState().linkStatus.getValue() == SeatState.LinkStatus.LINKING) {
                showExitSeatDialog();
            } else {
                exit();
            }
        }
    }

    private void showExitSeatDialog() {
        if (mExitSeatDialog == null) {
            mExitSeatDialog = new ExitSeatDialog(mContext, new ExitSeatDialog.OnConfirmListener() {
                @Override
                public void onExitRoom() {
                    exit();
                }

                @Override
                public void onExitSeat() {
                    mSeatGridView.leaveSeat(null);
                }
            });
        }
        mExitSeatDialog.show();
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
