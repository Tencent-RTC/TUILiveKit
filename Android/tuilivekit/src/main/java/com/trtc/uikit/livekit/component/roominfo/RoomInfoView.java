package com.trtc.uikit.livekit.component.roominfo;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.google.android.material.imageview.ShapeableImageView;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.roominfo.service.RoomInfoService;
import com.trtc.uikit.livekit.component.roominfo.store.RoomInfoState;
import com.trtc.uikit.livekit.component.roominfo.view.RoomInfoPopupDialog;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class RoomInfoView extends FrameLayout {
    private final Context                         mContext;
    private       TextView                        mTextNickName;
    private       ShapeableImageView              mImageAvatar;
    private       LinearLayout                    mLayoutRoot;
    private       TextView                        mTextUnfollow;
    private       ImageView                       mImageFollowIcon;
    private       FrameLayout                     mLayoutFollowPanel;
    private       RoomInfoPopupDialog             mRoomInfoPopupDialog;
    private final RoomInfoService                 mRoomInfoService      = new RoomInfoService();
    private final RoomInfoState                   mRoomInfoState        = mRoomInfoService.mRoomInfoState;
    private final Observer<String>                mHostIdObserver       = this::onHostIdChange;
    private final Observer<String>                mHostNickNameObserver = this::onHostNickNameChange;
    private final Observer<String>                mHostAvatarObserver   = this::onHostAvatarChange;
    private final Observer<LinkedHashSet<String>> mFollowStatusObserver = this::onFollowStatusChange;

    public RoomInfoView(Context context) {
        this(context, null);
    }

    public RoomInfoView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public RoomInfoView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_room_info, this, true);
    }

    public void init(String roomId) {
        mRoomInfoService.initRoomInfo(roomId);
    }

    private void initView() {
        bindViewId();

        initHostNameView();
        initHostAvatarView();
        initRoomInfoPanelView();
    }

    private void addObserver() {
        mRoomInfoState.ownerId.observe(mHostIdObserver);
        mRoomInfoState.ownerName.observe(mHostNickNameObserver);
        mRoomInfoState.ownerAvatarUrl.observe(mHostAvatarObserver);
        mRoomInfoState.followingList.observe(mFollowStatusObserver);
    }

    private void removeObserver() {
        mRoomInfoState.ownerId.removeObserver(mHostIdObserver);
        mRoomInfoState.ownerName.removeObserver(mHostNickNameObserver);
        mRoomInfoState.ownerAvatarUrl.removeObserver(mHostAvatarObserver);
        mRoomInfoState.followingList.removeObserver(mFollowStatusObserver);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();

        initView();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void bindViewId() {
        mLayoutRoot = findViewById(R.id.ll_root);
        mTextNickName = findViewById(R.id.tv_name);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mTextUnfollow = findViewById(R.id.tv_unfollow);
        mImageFollowIcon = findViewById(R.id.iv_follow);
        mLayoutFollowPanel = findViewById(R.id.fl_follow_panel);
    }

    private void initHostNameView() {
        mTextNickName.setText(mRoomInfoState.ownerName.get());
    }

    private void initHostAvatarView() {
        ImageLoader.load(mContext, mImageAvatar, mRoomInfoState.ownerAvatarUrl.get(), R.drawable.livekit_ic_avatar);
    }

    private void initRoomInfoPanelView() {
        mLayoutRoot.setOnClickListener(view -> {
            if (mRoomInfoPopupDialog == null) {
                mRoomInfoPopupDialog = new RoomInfoPopupDialog(mContext, mRoomInfoService);
            }
            mRoomInfoPopupDialog.show();
        });
    }

    private void onHostIdChange(String ownerId) {
        if (!TextUtils.isEmpty(ownerId) && !TextUtils.equals(mRoomInfoState.myUserId, ownerId)) {
            mLayoutFollowPanel.setVisibility(View.VISIBLE);
            mRoomInfoService.isFollow(ownerId);
            refreshFollowButton();
            mLayoutFollowPanel.setOnClickListener(this::onFollowButtonClick);
        }
    }

    private void onHostNickNameChange(String name) {
        mTextNickName.setText(name);
    }

    private void onHostAvatarChange(String avatar) {
        ImageLoader.load(mContext, mImageAvatar, avatar, R.drawable.livekit_ic_avatar);
    }

    private void onFollowStatusChange(LinkedHashSet<String> followUsers) {
        refreshFollowButton();
    }

    private void onFollowButtonClick(View view) {
        if (mRoomInfoState.followingList.get().contains(mRoomInfoState.ownerId.get())) {
            mRoomInfoService.unfollowUser(mRoomInfoState.ownerId.get());
        } else {
            mRoomInfoService.followUser(mRoomInfoState.ownerId.get());
        }
    }

    private void refreshFollowButton() {
        if (!mRoomInfoState.followingList.get().contains(mRoomInfoState.ownerId.get())) {
            mImageFollowIcon.setVisibility(GONE);
            mTextUnfollow.setVisibility(View.VISIBLE);
        } else {
            mTextUnfollow.setVisibility(View.GONE);
            mImageFollowIcon.setVisibility(VISIBLE);
        }
    }
}
