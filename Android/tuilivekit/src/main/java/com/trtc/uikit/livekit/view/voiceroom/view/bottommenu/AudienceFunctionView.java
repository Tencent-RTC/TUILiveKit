package com.trtc.uikit.livekit.view.voiceroom.view.bottommenu;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.LinearInterpolator;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUILikeButton;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.GiftButton;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;

@SuppressLint("ViewConstructor")
public class AudienceFunctionView extends BasicView {
    private ImageView mTakeSeatButton;

    private final Observer<String> mMySeatApplicationIdObserver = this::updateTakeSeatIcon;

    private final Observer<LiveDefine.LinkStatus> mLinkStateObserver = this::onLinkStateChanged;

    public AudienceFunctionView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_audience_function, this, true);
        initTakeButton();
        initGiftButton();
        initLikeButton();
    }

    @Override
    protected void addObserver() {
        mViewState.linkStatus.observe(mLinkStateObserver);
        mSeatState.mySeatApplicationId.observe(mMySeatApplicationIdObserver);
    }

    @Override
    protected void removeObserver() {
        mViewState.linkStatus.removeObserver(mLinkStateObserver);
        mSeatState.mySeatApplicationId.removeObserver(mMySeatApplicationIdObserver);
    }

    private void initGiftButton() {
        GiftButton giftButton = new GiftButton(mContext, mLiveController);
        giftButton.setLayoutParams(new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT,
                RelativeLayout.LayoutParams.MATCH_PARENT));
        RelativeLayout container = findViewById(R.id.rl_gift);
        container.addView(giftButton);
    }

    private void initLikeButton() {
        TUILikeButton likeButton = new TUILikeButton(mContext, mLiveController.getRoomSate().roomId);
        likeButton.setLayoutParams(new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT,
                RelativeLayout.LayoutParams.MATCH_PARENT));
        RelativeLayout container = findViewById(R.id.rl_like);
        container.addView(likeButton);
    }

    private void updateTakeSeatIcon(String mySeatApplicationId) {
        if (TextUtils.isEmpty(mySeatApplicationId)) {
            mTakeSeatButton.clearAnimation();
            mTakeSeatButton.setImageResource(R.drawable.livekit_ic_hand_up);
        } else {
            Animation animation = AnimationUtils.loadAnimation(mContext, R.anim.rotate_animation);
            animation.setInterpolator(new LinearInterpolator());
            mTakeSeatButton.setImageResource(R.drawable.livekit_audience_applying_link_mic);
            mTakeSeatButton.startAnimation(animation);
        }
    }

    private void initTakeButton() {
        mTakeSeatButton = findViewById(R.id.iv_take_seat);
        mTakeSeatButton.setOnClickListener(v -> {
            if (mViewState.linkStatus.get() == LiveDefine.LinkStatus.LINKING) {
                mSeatController.leaveSeat();
                return;
            }
            String takeSeatApplicationId = mSeatState.mySeatApplicationId.get();
            if (TextUtils.isEmpty(takeSeatApplicationId)) {
                mSeatController.takeSeat(-1);
            } else {
                mSeatController.cancelTakeSeatApplication();
            }
        });
    }

    private void onLinkStateChanged(LiveDefine.LinkStatus linkStatus) {
        mTakeSeatButton.clearAnimation();
        mTakeSeatButton.setImageResource(linkStatus == LiveDefine.LinkStatus.LINKING
                ? R.drawable.livekit_audience_linking_mic : R.drawable.livekit_ic_hand_up);
    }
}
