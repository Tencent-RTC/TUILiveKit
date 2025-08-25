package com.trtc.uikit.livekit.voiceroom.view;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.EVENT_SUB_KEY_CLOSE_VOICE_ROOM;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.GIFT_COUNT;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.GIFT_NAME;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.GIFT_VIEW_TYPE_1;
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
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.Constants;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.barrage.BarrageStreamView;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
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

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Map;

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

    private final Observer<String>                   mBackgroundURLObserver  = this::updateRoomBackground;
    private final Observer<RoomState.LiveStatus>     mLiveStateObserver      = this::onLiveStateChanged;
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
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_root_view, this, true);
        bindViewId();
    }

    public void init(VoiceRoomManager voiceRoomManager, TUIVoiceRoomFragment.RoomBehavior behavior,
                     TUIVoiceRoomFragment.RoomParams params) {
        setComponent();
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

    private void setComponent() {
        try {
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "component");
            jsonObject.put("component", Constants.DATA_REPORT_COMPONENT_VOICE_ROOM);
            SeatGridView.callExperimentalAPI(jsonObject.toString());
        } catch (JSONException e) {
            LOGGER.error("setComponent:" + e);
        }
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

    public VoiceRoomDefine.CoreState getCoreState() {
        return mSeatGridView.getCoreState();
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
        mVoiceRoomManager.getRoomState().backgroundURL.observeForever(mBackgroundURLObserver);
        mVoiceRoomManager.getRoomState().liveStatus.observeForever(mLiveStateObserver);
        mVoiceRoomManager.getSeatState().receivedSeatInvitation.observeForever(mSeatInvitationObserver);
        mVoiceRoomManager.getSeatState().linkStatus.observeForever(mLinkStateObserver);
        mSeatGridViewCoreObserver = new SeatGridViewCoreObserver(mContext, mVoiceRoomManager, mSeatGridView);
        mSeatGridView.addObserver(mSeatGridViewCoreObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_VOICE_ROOM, this);
    }

    private void removeObserver() {
        mVoiceRoomManager.getRoomState().backgroundURL.removeObserver(mBackgroundURLObserver);
        mVoiceRoomManager.getRoomState().liveStatus.removeObserver(mLiveStateObserver);
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

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("unmuteMicrophone failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void startMicrophone() {
        mSeatGridView.startMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

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
            public void onReceiveGift(GiftPlayView view, TUILiveGiftManager.GiftInfo gift, int giftCount, TUIRoomDefine.UserInfo sender) {
                if (mBarrageStreamView == null) {
                    return;
                }
                Barrage barrage = new Barrage();
                barrage.content = "gift";
                barrage.user.userId = sender.userId;
                barrage.user.userName = TextUtils.isEmpty(sender.userName) ? sender.userId : sender.userName;
                barrage.user.avatarUrl = sender.avatarUrl;
                barrage.extInfo.put(GIFT_VIEW_TYPE, GIFT_VIEW_TYPE_1);
                barrage.extInfo.put(GIFT_NAME, gift.name);
                barrage.extInfo.put(GIFT_COUNT, giftCount);
                barrage.extInfo.put(GIFT_ICON_URL, gift.iconUrl);
                TUIRoomDefine.UserInfo ownerInfo = mVoiceRoomManager.getCoreState().roomState.ownerInfo.getValue();
                String receiverName = TextUtils.isEmpty(ownerInfo.userName) ? ownerInfo.userId : ownerInfo.userName;
                if (TextUtils.equals(ownerInfo.userId, TUILogin.getUserId())) {
                    receiverName = getContext().getString(R.string.common_gift_me);
                }
                barrage.extInfo.put(GIFT_RECEIVER_USERNAME, receiverName);
                mBarrageStreamView.insertBarrages(barrage);
            }

            @Override
            public void onPlayGiftAnimation(GiftPlayView view, TUILiveGiftManager.GiftInfo gift) {
                mGiftCacheService.request(gift.resourceUrl, (error, result) -> {
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
        liveInfo.roomId = roomState.roomId;
        liveInfo.name = roomState.roomName.getValue();
        liveInfo.maxSeatCount = roomState.maxSeatCount.getValue() == null ? 9 : roomState.maxSeatCount.getValue();
        liveInfo.seatMode = roomState.seatMode.getValue();
        liveInfo.backgroundUrl = roomState.backgroundURL.getValue();
        liveInfo.coverUrl = roomState.coverURL.getValue();
        liveInfo.isPublicVisible = roomState.liveExtraInfo.liveMode.getValue() == RoomState.LiveStreamPrivacyStatus.PUBLIC;
        mSeatGridView.startVoiceRoom(liveInfo, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfo liveInfo) {
                LOGGER.info("create room success");
                mVoiceRoomManager.getRoomManager().updateRoomState(liveInfo);
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PUSHING);
                mVoiceRoomManager.getUserManager().getAudienceList();
                mVoiceRoomManager.getUserManager().updateOwnerUserInfo();
                mVoiceRoomManager.getSeatManager().getSeatList();
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

    private void exit() {
        RoomManager roomManager = mVoiceRoomManager.getRoomManager();
        roomManager.updateMessageCount(mBarrageStreamView.getBarrageCount());
        if (roomManager.isOwner()) {
            mSeatGridView.stopVoiceRoom(new TUILiveListManager.StopLiveCallback() {
                @Override
                public void onSuccess(TUILiveListManager.LiveStatisticsData data) {
                    mVoiceRoomManager.getRoomManager().updateLiveStatisticsData(data);
                    mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.DASHBOARD);
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LOGGER.error("stopVoiceRoom onError:error:" + error + ", errorCode:" + error.getValue() + ", message:" + message);
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
        } else if (status == RoomState.LiveStatus.DASHBOARD) {
            showEndView();
        } else if (status == RoomState.LiveStatus.PREVIEWING) {
            showPreviewView();
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
        mInvitationDialog.setPositiveText(mContext.getString(R.string.common_accept),
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
