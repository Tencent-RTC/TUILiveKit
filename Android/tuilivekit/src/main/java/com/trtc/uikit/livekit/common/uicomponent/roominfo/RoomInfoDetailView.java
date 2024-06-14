package com.trtc.uikit.livekit.common.uicomponent.roominfo;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.android.material.imageview.ShapeableImageView;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class RoomInfoDetailView extends BottomPanelView {

    private       Button                                      mButtonFollow;
    private       TextView                                    mTextOwnerName;
    private       ShapeableImageView                          mImageAvatar;
    private final Observer<String>                            mOwnerNameObserver       = this::onOwnerNameChange;
    private final Observer<LinkedHashSet<UserState.UserInfo>> mMyFollowingUserObserver = this::onMyFollowingUserChange;
    private final Observer<Long>                              mOwnerFansCountObserver  = this::onOwnerFansCountChange;
    private final Observer<String>                            mOwnerAvatarObserver     = this::onOwnerAvatarChange;

    public RoomInfoDetailView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void addObserver() {
        mUserState.myFollowingUserList.observe(mMyFollowingUserObserver);
        mRoomState.ownerInfo.fansCount.observe(mOwnerFansCountObserver);
        mRoomState.ownerInfo.name.observe(mOwnerNameObserver);
        mRoomState.ownerInfo.avatarUrl.observe(mOwnerAvatarObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.myFollowingUserList.removeObserver(mMyFollowingUserObserver);
        mRoomState.ownerInfo.fansCount.removeObserver(mOwnerFansCountObserver);
        mRoomState.ownerInfo.name.removeObserver(mOwnerNameObserver);
        mRoomState.ownerInfo.avatarUrl.removeObserver(mOwnerAvatarObserver);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_room_info_detail_view, this, true);
        bindViewId();

        initAnchorNameView();
        initRoomIdView();
        initAnchorAvatarView();
        initFollowButtonView();
        initFansView();
    }

    private void bindViewId() {
        mButtonFollow = findViewById(R.id.btn_follow);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mTextOwnerName = findViewById(R.id.tv_anchor_name);
    }

    private void initAnchorAvatarView() {
        ImageView imageAvatar = findViewById(R.id.iv_avatar);
        ImageLoader.load(mContext, imageAvatar, mRoomState.ownerInfo.avatarUrl.get(), R.drawable.livekit_ic_avatar);
    }

    private void initFollowButtonView() {
        if (mRoomState.ownerInfo.userId.equals(mUserState.selfInfo.userId)) {
            mButtonFollow.setText("");
            mButtonFollow.setVisibility(View.GONE);
        } else {
            refreshFollowButton();
            mButtonFollow.setOnClickListener(view -> {
                if (mUserState.myFollowingUserList.get().contains(
                        new UserState.UserInfo(mRoomState.ownerInfo.userId))) {
                    mLiveController.getUserController().unfollow(mRoomState.ownerInfo.userId);
                } else {
                    mLiveController.getUserController().follow(mRoomState.ownerInfo.userId);
                }
            });
        }
    }

    private void refreshFollowButton() {
        if (mUserState.myFollowingUserList.get().contains(new UserState.UserInfo(mRoomState.ownerInfo.userId))) {
            mButtonFollow.setText(R.string.livekit_unfollow_anchor);
            mButtonFollow.setBackgroundResource(R.drawable.livekit_live_info_detail_button_unfollow);
        } else {
            mButtonFollow.setText(R.string.livekit_follow_anchor);
            mButtonFollow.setBackgroundResource(R.drawable.livekit_live_info_button_follow);
        }
    }

    private void initFansView() {
        mUserController.getFansCount();
    }

    private void initRoomIdView() {
        TextView textRoomId = findViewById(R.id.tv_liveroom_id);
        textRoomId.setText(mRoomState.roomId);
    }

    private void initAnchorNameView() {
        mTextOwnerName.setText(mRoomState.ownerInfo.name.get().isEmpty() ? mRoomState.ownerInfo.userId :
                mRoomState.ownerInfo.name.get());
    }

    private void onMyFollowingUserChange(LinkedHashSet<UserState.UserInfo> userInfos) {
        refreshFollowButton();
    }

    private void onOwnerFansCountChange(Long fansCount) {
        TextView textFans = findViewById(R.id.tv_fans);
        textFans.setText(String.valueOf(fansCount));
    }

    private void onOwnerNameChange(String name) {
        mTextOwnerName.setText(name);
    }

    private void onOwnerAvatarChange(String avatar) {
        ImageLoader.load(mContext, mImageAvatar, avatar, R.drawable.livekit_ic_avatar);
    }
}
