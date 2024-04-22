package com.trtc.uikit.livekit.voiceroom.view.bottommenu;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageButton;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.model.BottomMenuInfo;
import com.trtc.uikit.livekit.common.core.store.state.operation.SeatState;
import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUILikeButton;
import com.trtc.uikit.livekit.common.view.BadgeView;
import com.trtc.uikit.livekit.common.view.BasicView;

import java.util.LinkedHashSet;
import java.util.List;

@SuppressLint("ViewConstructor")
public class BottomMenuView extends BasicView {
    private final List<BottomMenuInfo> mMenuInfoList;
    private       LinearLayout         mViewContainer;
    private       RelativeLayout       mBarrageButtonContainer;
    private       ImageView            mTakeSeatButton;
    private       BadgeView            mBadgeView;
    private final boolean              mIsAudience;

    private final Observer<Boolean> mIsInSeatObserver = this::updateTakeSeatIcon;

    private final Observer<String> mMySeatApplicationIdObserver = this::updateTakeSeatIcon;

    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mSeatApplicationListObserver =
            this::updateLickManager;

    public BottomMenuView(Context context, LiveController liveController, List<BottomMenuInfo> list) {
        super(context, liveController);
        mMenuInfoList = list;
        mIsAudience = mUserState.selfInfo.role.get() != TUIRoomDefine.Role.ROOM_OWNER;
    }

    @Override
    protected void initView() {
        View roomView = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_voiceroom_layout_bottom_menu, this, true);
        mViewContainer = roomView.findViewById(R.id.view_container);
        mBarrageButtonContainer = roomView.findViewById(R.id.rl_barrage_button);
        mBadgeView = new BadgeView(mContext);
        setMenuButton(mMenuInfoList);
        if (mIsAudience) {
            showAudienceBarrageSendButton();
        }
    }

    @Override
    protected void addObserver() {
        if (mIsAudience) {
            mUserState.selfInfo.isInSeat.observe(mIsInSeatObserver);
            mSeatState.mySeatApplicationId.observe(mMySeatApplicationIdObserver);
        } else {
            mSeatState.seatApplicationList.observe(mSeatApplicationListObserver);
        }
    }

    @Override
    protected void removeObserver() {
        mUserState.selfInfo.isInSeat.remove(mIsInSeatObserver);
        mSeatState.mySeatApplicationId.remove(mMySeatApplicationIdObserver);
        mSeatState.seatApplicationList.remove(mSeatApplicationListObserver);
    }

    private void initTakeSeatButton(ImageView imageView) {
        mTakeSeatButton = imageView;
        updateTakeSeatIcon(mUserState.selfInfo.isInSeat.get());
    }

    private void initSeatApplicationButton(View itemView) {
        mBadgeView = new BadgeView(mContext);
        ViewGroup viewGroup = (ViewGroup) itemView;
        viewGroup.addView(mBadgeView);
        updateLickManager(mSeatState.seatApplicationList.get());
    }

    private void updateTakeSeatIcon(boolean isInSeat) {
        mTakeSeatButton.setImageResource(isInSeat ? R.drawable.livekit_audience_linking_mic :
                R.drawable.livekit_ic_link_mic);
    }

    private void updateTakeSeatIcon(String mySeatApplicationId) {
        if (TextUtils.isEmpty(mySeatApplicationId)) {
            mTakeSeatButton.setImageResource(R.drawable.livekit_ic_link_mic);
        } else {
            mTakeSeatButton.setImageResource(R.drawable.livekit_audience_applying_link_mic);
        }
    }

    private void updateLickManager(LinkedHashSet<SeatState.SeatApplication> list) {
        mBadgeView.setVisibility(list.isEmpty() ? INVISIBLE : VISIBLE);
    }

    private void setMenuButton(List<BottomMenuInfo> mMenuInfoList) {
        mViewContainer.removeAllViews();
        for (BottomMenuInfo bottomMenuInfo : mMenuInfoList) {
            View itemView =
                    LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_item_bottom_menu, this,
                            false);
            ImageView iv = itemView.findViewById(R.id.iv_item);
            if (bottomMenuInfo.navigationState == ViewState.NavigationState.LIKE) {
                TUILikeButton likeButton = new TUILikeButton(mContext, mRoomState.roomId);
                mViewContainer.addView(likeButton);
                continue;
            } else if (bottomMenuInfo.navigationState == ViewState.NavigationState.LINK_MIC) {
                initTakeSeatButton(iv);
            } else if (bottomMenuInfo.navigationState == ViewState.NavigationState.LINK_MANAGEMENT) {
                initSeatApplicationButton(itemView);
            }
            iv.setImageResource(bottomMenuInfo.icon);
            itemView.setOnClickListener(view -> {
                if (bottomMenuInfo.listener != null) {
                    bottomMenuInfo.listener.onClick();
                }
            });
            mViewContainer.addView(itemView);
        }
    }

    private void showAudienceBarrageSendButton() {
        TUIBarrageButton barrageButton = new TUIBarrageButton(mContext, mRoomState.roomId);
        mBarrageButtonContainer.addView(barrageButton);
    }
}

