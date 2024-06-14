package com.trtc.uikit.livekit.common.uicomponent.roominfo;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.google.android.material.imageview.ShapeableImageView;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.BottomPanel;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class RoomInfoView extends BasicView {
    private       TextView                                    mOwnerName;
    private       ShapeableImageView                          mImageAvatar;
    private       BottomPanel                                 mStreamInfoPanel;
    private       LinearLayout                                mLayoutRoot;
    private       TextView                                    mTextUnfollow;
    private       ImageView                                   mImageFollowIcon;
    private       FrameLayout                                 mLayoutFollowPanel;
    private final Observer<String>                            mOwnerNameObserver       = this::onOwnerNameChange;
    private final Observer<String>                            mOwnerAvatarObserver     = this::onOwnerAvatarChange;
    private final Observer<LinkedHashSet<UserState.UserInfo>> mMyFollowingUserObserver = this::onMyFollowingUserChange;
    private final Observer<LiveDefine.LiveStatus>             mLiveStatusObserver      = this::onLiveStatusChange;

    public RoomInfoView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_room_info, this, true);
        bindViewId();

        initRoomInfoPanelView();
        initOwnerNameView();
        initOwnerAvatarView();
    }


    @Override
    protected void addObserver() {
        mRoomState.ownerInfo.name.observe(mOwnerNameObserver);
        mRoomState.ownerInfo.avatarUrl.observe(mOwnerAvatarObserver);
        mUserState.myFollowingUserList.observe(mMyFollowingUserObserver);
        mViewState.liveStatus.observe(mLiveStatusObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.ownerInfo.name.removeObserver(mOwnerNameObserver);
        mRoomState.ownerInfo.avatarUrl.removeObserver(mOwnerAvatarObserver);
        mUserState.myFollowingUserList.removeObserver(mMyFollowingUserObserver);
        mViewState.liveStatus.removeObserver(mLiveStatusObserver);
    }


    private void bindViewId() {
        mLayoutRoot = findViewById(R.id.ll_root);
        mOwnerName = findViewById(R.id.tv_live_name);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mTextUnfollow = findViewById(R.id.tv_unfollow);
        mImageFollowIcon = findViewById(R.id.iv_follow);
        mLayoutFollowPanel = findViewById(R.id.fl_follow_panel);
    }

    private void initOwnerAvatarView() {
        ImageLoader.load(mContext, mImageAvatar, mRoomState.ownerInfo.avatarUrl.get(), R.drawable.livekit_ic_avatar);
    }

    private void initOwnerNameView() {
        mOwnerName.setText(mRoomState.ownerInfo.name.get());
    }

    private void initRoomInfoPanelView() {
        mLayoutRoot.setOnClickListener(view -> {
            if (mStreamInfoPanel == null) {
                RoomInfoDetailView panel = new RoomInfoDetailView(mContext, mLiveController);
                mStreamInfoPanel = BottomPanel.createTransparent(panel);
            }
            mStreamInfoPanel.show();
        });
    }

    protected void initFollowButtonView() {
        if (mRoomState.ownerInfo.userId.equals(mUserState.selfInfo.userId)) {
            mTextUnfollow.setVisibility(View.GONE);
            mImageFollowIcon.setVisibility(View.GONE);
        } else {
            mLiveController.getUserController().checkFollowType(mRoomState.ownerInfo.userId);
            refreshFollowStatus();
        }
        mLayoutFollowPanel.setOnClickListener(view -> {
            if (mUserState.myFollowingUserList.get().contains(new UserState.UserInfo(mRoomState.ownerInfo.userId))) {
                mLiveController.getUserController().unfollow(mRoomState.ownerInfo.userId);
            } else {
                mLiveController.getUserController().follow(mRoomState.ownerInfo.userId);
            }
        });
    }

    private void onOwnerNameChange(String name) {
        mOwnerName.setText(name);
    }

    private void onOwnerAvatarChange(String avatar) {
        ImageLoader.load(mContext, mImageAvatar, avatar, R.drawable.livekit_ic_avatar);
    }

    private void onMyFollowingUserChange(LinkedHashSet<UserState.UserInfo> followUsers) {
        refreshFollowStatus();
    }

    private void refreshFollowStatus() {
        if (!mUserState.myFollowingUserList.get().contains(new UserState.UserInfo(mRoomState.ownerInfo.userId))) {
            mImageFollowIcon.setVisibility(GONE);
            mTextUnfollow.setVisibility(View.VISIBLE);
        } else {
            mTextUnfollow.setVisibility(View.GONE);
            mImageFollowIcon.setVisibility(VISIBLE);
        }
    }

    private void onLiveStatusChange(LiveDefine.LiveStatus liveStatus) {
        if (LiveDefine.LiveStatus.PUSHING == liveStatus || LiveDefine.LiveStatus.PLAYING == liveStatus) {
            initFollowButtonView();
        }
    }
}
