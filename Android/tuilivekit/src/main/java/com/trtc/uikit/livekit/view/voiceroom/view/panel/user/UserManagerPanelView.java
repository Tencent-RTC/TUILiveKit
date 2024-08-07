package com.trtc.uikit.livekit.view.voiceroom.view.panel.user;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class UserManagerPanelView extends BottomPanelView {
    private final SeatState.SeatInfo mSeatInfo;
    private       ImageView          mIvMute;
    private       TextView           mTvMute;
    private       TextView           mTextUnfollow;
    private       ImageView          mImageFollowIcon;
    private       View               mUserControllerView;

    private final Observer<Boolean>                           isAudioLockedObserver    = this::updateAudioLockState;
    private final Observer<LinkedHashSet<UserState.UserInfo>> mMyFollowingUserObserver = this::onMyFollowingUserChange;

    public UserManagerPanelView(Context context, LiveController liveController, SeatState.SeatInfo seatInfo) {
        super(context, liveController);
        mSeatInfo = seatInfo;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_user_manager_panel, this, true);
        bindViewId();
        updateSeatInfoView();
        initFollowButtonView();
        findViewById(R.id.hand_up).setOnClickListener(v -> hangup());
        findViewById(R.id.mute_container).setOnClickListener(v -> mSeatController.muteSeatAudio(mSeatInfo));
        mUserControllerView.setVisibility(mUserState.selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER
                ? VISIBLE : GONE);
    }

    private void bindViewId() {
        mIvMute = findViewById(R.id.iv_mute);
        mTvMute = findViewById(R.id.tv_mute);
        mTextUnfollow = findViewById(R.id.tv_unfollow);
        mImageFollowIcon = findViewById(R.id.iv_follow);
        mUserControllerView = findViewById(R.id.user_controller_container);
    }

    private void updateSeatInfoView() {
        if (mSeatInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(mSeatInfo.userId.get())) {
            return;
        }
        ImageFilterView imageHead = findViewById(R.id.iv_head);
        String avatarUrl = mSeatInfo.avatarUrl.get();
        if (TextUtils.isEmpty(mSeatInfo.avatarUrl.get())) {
            imageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, imageHead, avatarUrl, R.drawable.livekit_ic_avatar);
        }
        TextView nameText = findViewById(R.id.user_name);
        TextView idText = findViewById(R.id.user_id);
        nameText.setText(mSeatInfo.name.get());
        idText.setText(mContext.getString(R.string.livekit_user_id, mSeatInfo.userId.get()));
    }

    @Override
    protected void addObserver() {
        mSeatInfo.isAudioLocked.observe(isAudioLockedObserver);
        mUserState.myFollowingUserList.observe(mMyFollowingUserObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatInfo.isAudioLocked.removeObserver(isAudioLockedObserver);
        mUserState.myFollowingUserList.removeObserver(mMyFollowingUserObserver);
    }

    private void hangup() {
        mSeatController.kickUserOffSeatByAdmin(mSeatInfo);
        dismiss();
    }

    private void updateAudioLockState(boolean isAudioLocked) {
        if (isAudioLocked) {
            mIvMute.setImageResource(R.drawable.livekit_ic_unmute_microphone);
            mTvMute.setText(R.string.livekit_cvoiceroom_unmuted_seat);
        } else {
            mIvMute.setImageResource(R.drawable.livekit_ic_mute_microphone);
            mTvMute.setText(R.string.livekit_voiceroom_mute_seat);
        }
    }

    private void initFollowButtonView() {
        mUserController.checkFollowType(mSeatInfo.userId.get());
        findViewById(R.id.fl_follow_panel).setOnClickListener(view -> {
            if (mUserState.myFollowingUserList.get().contains(new UserState.UserInfo(mSeatInfo.userId.get()))) {
                mLiveController.getUserController().unfollow(mSeatInfo.userId.get());
            } else {
                mLiveController.getUserController().follow(mSeatInfo.userId.get());
            }
        });
    }

    private void onMyFollowingUserChange(LinkedHashSet<UserState.UserInfo> followUsers) {
        if (followUsers.contains(new UserState.UserInfo(mSeatInfo.userId.get()))) {
            mTextUnfollow.setVisibility(View.GONE);
            mImageFollowIcon.setVisibility(VISIBLE);
        } else {
            mImageFollowIcon.setVisibility(GONE);
            mTextUnfollow.setVisibility(View.VISIBLE);
        }
    }
}