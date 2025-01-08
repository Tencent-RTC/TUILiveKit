package com.trtc.uikit.component.roominfo.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.component.roominfo.R;
import com.trtc.uikit.component.roominfo.service.RoomInfoService;
import com.trtc.uikit.component.roominfo.store.RoomInfoState;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class RoomInfoPopupDialog extends PopupDialog {
    private final Context                         mContext;
    private       Button                          mButtonFollow;
    private       TextView                        mTextOwnerName;
    private final RoomInfoService                 mRoomInfoService;
    private final RoomInfoState                   mRoomInfoState;
    private final Observer<LinkedHashSet<String>> mFollowStatusObserver   = this::onFollowStatusChange;
    private final Observer<Long>                  mOwnerFansCountObserver = this::onFansNumberChange;
    private final Observer<String>                mOwnerIdObserver        = this::onHostChange;
    private       TextView                        mTextRoomId;
    private       ImageView                       mImageAvatar;
    private       TextView                        mTextFans;
    private       View                            mFansLayout;

    public RoomInfoPopupDialog(Context context, RoomInfoService roomInfoService) {
        super(context);
        mContext = context;
        mRoomInfoService = roomInfoService;
        mRoomInfoState = mRoomInfoService.mRoomInfoState;
        initView();
    }

    private void addObserver() {
        mRoomInfoState.followingList.observe(mFollowStatusObserver);
        mRoomInfoState.fansNumber.observe(mOwnerFansCountObserver);
        mRoomInfoState.ownerId.observe(mOwnerIdObserver);
    }

    private void removeObserver() {
        mRoomInfoState.followingList.removeObserver(mFollowStatusObserver);
        mRoomInfoState.fansNumber.removeObserver(mOwnerFansCountObserver);
        mRoomInfoState.ownerId.removeObserver(mOwnerIdObserver);
    }

    private void initView() {
        View view = LayoutInflater.from(mContext).inflate(R.layout.livekit_room_info_detail_view, null);
        bindViewId(view);

        initAnchorNameView();
        initRoomIdView();
        initAvatarView();
        initFansView();
        setView(view);
    }

    private void bindViewId(View view) {
        mButtonFollow = view.findViewById(R.id.btn_follow);
        mTextOwnerName = view.findViewById(R.id.tv_anchor_name);
        mTextRoomId = view.findViewById(R.id.tv_liveroom_id);
        mImageAvatar = view.findViewById(R.id.iv_avatar);
        mTextFans = view.findViewById(R.id.tv_fans);
        mFansLayout = view.findViewById(R.id.ll_fans);
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
        getFansNumber();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void initAnchorNameView() {
        mTextOwnerName.setText(mRoomInfoState.ownerName.get().isEmpty()
                ? mRoomInfoState.ownerId.get() : mRoomInfoState.ownerName.get());
    }

    private void initRoomIdView() {
        mTextRoomId.setText(mRoomInfoState.roomId);
    }

    private void initAvatarView() {
        ImageLoader.load(mContext, mImageAvatar, mRoomInfoState.ownerAvatarUrl.get(), R.drawable.livekit_ic_avatar);
    }

    private void initFansView() {
        if (mRoomInfoState.enableFollow) {
            mFansLayout.setVisibility(View.VISIBLE);
        } else {
            mFansLayout.setVisibility(View.GONE);
        }
    }

    private void getFansNumber() {
        mRoomInfoService.getFansNumber();
    }

    private void refreshFollowButton() {
        if (!mRoomInfoState.enableFollow) {
            mButtonFollow.setVisibility(View.GONE);
            return;
        }
        if (mRoomInfoState.followingList.get().contains(mRoomInfoState.ownerId.get())) {
            mButtonFollow.setText(R.string.livekit_roominfo_unfollow_anchor);
            mButtonFollow.setBackgroundResource(R.drawable.livekit_live_info_detail_button_unfollow);
        } else {
            mButtonFollow.setText(R.string.livekit_roominfo_follow_anchor);
            mButtonFollow.setBackgroundResource(R.drawable.livekit_live_info_button_follow);
        }
    }

    private void onFollowStatusChange(LinkedHashSet<String> userInfo) {
        refreshFollowButton();
    }

    private void onFansNumberChange(Long fansCount) {
        mTextFans.setText(String.valueOf(fansCount));
    }

    private void onHostChange(String ownerId) {
        if (!mRoomInfoState.enableFollow) {
            return;
        }
        if (TextUtils.isEmpty(ownerId)) {
            return;
        }
        if (TextUtils.equals(mRoomInfoState.myUserId, mRoomInfoState.ownerId.get())) {
            mButtonFollow.setText("");
            mButtonFollow.setVisibility(View.GONE);
        } else {
            mRoomInfoService.isFollow(mRoomInfoState.ownerId.get());
            refreshFollowButton();
        }
        mButtonFollow.setOnClickListener(this::onFollowButtonClick);
    }

    private void onFollowButtonClick(View view) {
        if (mRoomInfoState.followingList.get().contains(mRoomInfoState.ownerId.get())) {
            mRoomInfoService.unfollowUser(mRoomInfoState.ownerId.get());
        } else {
            mRoomInfoService.followUser(mRoomInfoState.ownerId.get());
        }
    }
}
