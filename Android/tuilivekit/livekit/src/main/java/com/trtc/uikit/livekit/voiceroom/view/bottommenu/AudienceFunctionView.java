package com.trtc.uikit.livekit.voiceroom.view.bottommenu;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.LinearInterpolator;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.component.gift.LikeButton;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.GiftButton;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.error.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

public class AudienceFunctionView extends BasicView {
    private static final String FILE = "AudienceFunctionView";

    private ImageView mTakeSeatButton;

    private final Observer<SeatState.LinkStatus> mLinkStateObserver = this::onLinkStateChanged;

    public AudienceFunctionView(@NonNull Context context) {
        this(context, null);
    }

    public AudienceFunctionView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceFunctionView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_audience_function, this, true);
        mTakeSeatButton = findViewById(R.id.iv_take_seat);
    }

    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        super.init(voiceRoomManager);
        initTakeButton();
        initGiftButton();
        initLikeButton();
    }

    @Override
    protected void addObserver() {
        mSeatState.linkStatus.observe(mLinkStateObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.linkStatus.removeObserver(mLinkStateObserver);
    }

    private void initGiftButton() {
        GiftButton giftButton = new GiftButton(mContext);
        giftButton.init(mRoomState.roomId, mRoomState.ownerInfo.userId, mRoomState.ownerInfo.name.get(),
                mRoomState.ownerInfo.avatarUrl.get());
        giftButton.setLayoutParams(new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT,
                RelativeLayout.LayoutParams.MATCH_PARENT));
        RelativeLayout container = findViewById(R.id.rl_gift);
        container.addView(giftButton);
    }

    private void initLikeButton() {
        LikeButton likeButton = new LikeButton(mContext);
        likeButton.init(mVoiceRoomManager.getRoomState().roomId);
        likeButton.setLayoutParams(new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT,
                RelativeLayout.LayoutParams.MATCH_PARENT));
        RelativeLayout container = findViewById(R.id.rl_like);
        container.addView(likeButton);
    }

    private void initTakeButton() {
        mTakeSeatButton.setOnClickListener(v -> {
            switch (mSeatState.linkStatus.get()) {
                case LINKING:
                    leaveSeat();
                    break;
                case APPLYING:
                    cancelSeatApplication();
                    break;
                default:
                    takeSeat();
                    break;
            }
        });
    }

    private void takeSeat() {
        mSeatGridView.takeSeat(-1, 60, new VoiceRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.LINKING);
            }

            @Override
            public void onRejected(TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
                ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(
                        R.string.livekit_voiceroom_take_seat_rejected));
            }

            @Override
            public void onCancelled(TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
            }

            @Override
            public void onTimeout(TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
                ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(
                        R.string.livekit_voiceroom_take_seat_timeout));
            }

            @Override
            public void onError(TUIRoomDefine.UserInfo userInfo, TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "takeSeat failed,error:" + error + ",message:" + message);
                if (error != TUICommonDefine.Error.REQUEST_ID_REPEAT) {
                    mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
                }
                ErrorLocalized.onError(error);
            }
        });
        mSeatManager.updateLinkState(SeatState.LinkStatus.APPLYING);
    }

    private void leaveSeat() {
        mSeatGridView.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "leaveSeat failed,error:" + error + ",message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void cancelSeatApplication() {
        mSeatGridView.cancelRequest("", new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "cancelRequest failed,error:" + error + ",message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void onLinkStateChanged(SeatState.LinkStatus linkStatus) {
        mTakeSeatButton.clearAnimation();
        switch (linkStatus) {
            case LINKING:
                mTakeSeatButton.setImageResource(R.drawable.livekit_audience_linking_mic);
                break;
            case APPLYING:
                Animation animation = AnimationUtils.loadAnimation(mContext, R.anim.rotate_animation);
                animation.setInterpolator(new LinearInterpolator());
                mTakeSeatButton.setImageResource(R.drawable.livekit_audience_applying_link_mic);
                mTakeSeatButton.startAnimation(animation);
                break;
            default:
                mTakeSeatButton.clearAnimation();
                mTakeSeatButton.setImageResource(R.drawable.livekit_ic_hand_up);
                break;
        }
    }
}
