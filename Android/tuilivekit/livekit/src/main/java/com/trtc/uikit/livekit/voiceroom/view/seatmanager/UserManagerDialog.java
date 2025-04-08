package com.trtc.uikit.livekit.voiceroom.view.seatmanager;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.content.Context;
import android.text.TextUtils;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.api.Logger;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;

import java.util.List;
import java.util.Set;

public class UserManagerDialog extends PopupDialog {
    private static final String FILE = "UserManagerDialog";

    private final Context            mContext;
    private final VoiceRoomManager   mVoiceRoomManager;
    private final SeatGridView       mSeatGridView;
    private       int                mSeatIndex = -1;
    private       SeatState.SeatInfo mSeatInfo;

    private ImageFilterView mImageHeadView;
    private TextView        mUserIdText;
    private TextView        mUserNameText;
    private ImageView       mIvMute;
    private TextView        mTvMute;
    private TextView        mTextUnfollow;
    private ImageView       mImageFollowIcon;
    private View            mUserControllerView;

    private final Observer<Boolean>                 isAudioLockedObserver    = this::updateAudioLockState;
    private final Observer<String>                  userIdObserver           = this::updateUserIdState;
    private final Observer<Set<UserState.UserInfo>> mMyFollowingUserObserver = this::onMyFollowingUserChanged;

    public UserManagerDialog(@NonNull Context context, VoiceRoomManager voiceRoomManager, SeatGridView seatGridView) {
        super(context);
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        mSeatGridView = seatGridView;
        initView();
    }

    public void setSeatIndex(int seatIndex) {
        if (mSeatIndex == seatIndex) {
            return;
        }
        if (seatIndex == -1) {
            return;
        }
        List<SeatState.SeatInfo> seatList = mVoiceRoomManager.getSeatState().seatList.getValue();
        if (seatIndex >= seatList.size()) {
            return;
        }
        mSeatInfo = seatList.get(seatIndex);
        if (mSeatInfo != null) {
            updateSeatInfoView();
            mVoiceRoomManager.getUserManager().checkFollowType(mSeatInfo.userId.getValue());
        }
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void initView() {
        View rootView = View.inflate(mContext, R.layout.livekit_user_manager_panel, null);
        setView(rootView);
        bindViewId(rootView);
        initFollowButtonView(rootView);
        rootView.findViewById(R.id.hand_up).setOnClickListener(v -> hangup());
        rootView.findViewById(R.id.mute_container).setOnClickListener(v -> muteSeatAudio());
        mUserControllerView.setVisibility(
                mVoiceRoomManager.getUserState().selfInfo.role.getValue() == TUIRoomDefine.Role.ROOM_OWNER ? VISIBLE
                        : GONE);
    }

    private void bindViewId(View rootView) {
        mUserControllerView = rootView.findViewById(R.id.user_controller_container);
        mUserIdText = rootView.findViewById(R.id.user_id);
        mUserNameText = rootView.findViewById(R.id.user_name);
        mImageHeadView = rootView.findViewById(R.id.iv_head);
        mIvMute = rootView.findViewById(R.id.iv_mute);
        mTvMute = rootView.findViewById(R.id.tv_mute);
        mTextUnfollow = rootView.findViewById(R.id.tv_unfollow);
        mImageFollowIcon = rootView.findViewById(R.id.iv_follow);
    }

    private void updateSeatInfoView() {
        if (mSeatInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(mSeatInfo.userId.getValue())) {
            return;
        }
        String avatarUrl = mSeatInfo.avatarUrl.getValue();
        if (TextUtils.isEmpty(avatarUrl)) {
            mImageHeadView.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImageHeadView, avatarUrl, R.drawable.livekit_ic_avatar);
        }
        mUserNameText.setText(mSeatInfo.name.getValue());
        mUserIdText.setText(mContext.getString(R.string.common_user_id, mSeatInfo.userId.getValue()));
        updateAudioLockState(mSeatInfo.isAudioLocked.getValue());
    }

    private void addObserver() {
        mVoiceRoomManager.getUserState().myFollowingUserList.observeForever(mMyFollowingUserObserver);
        mSeatInfo.isAudioLocked.observeForever(isAudioLockedObserver);
        mSeatInfo.userId.observeForever(userIdObserver);
    }

    private void removeObserver() {
        mVoiceRoomManager.getUserState().myFollowingUserList.observeForever(mMyFollowingUserObserver);
        mSeatInfo.isAudioLocked.removeObserver(isAudioLockedObserver);
        mSeatInfo.userId.removeObserver(userIdObserver);
    }

    private void hangup() {
        kickUserOffSeatByAdmin();
        dismiss();
    }

    private void initFollowButtonView(View rootView) {
        rootView.findViewById(R.id.fl_follow_panel).setOnClickListener(view -> {
            if (mSeatInfo == null) {
                return;
            }
            if (mVoiceRoomManager.getUserState().myFollowingUserList.getValue().contains(
                    new UserState.UserInfo(mSeatInfo.userId.getValue()))) {
                mVoiceRoomManager.getUserManager().unfollow(mSeatInfo.userId.getValue());
            } else {
                mVoiceRoomManager.getUserManager().follow(mSeatInfo.userId.getValue());
            }
        });
    }

    private void onMyFollowingUserChanged(Set<UserState.UserInfo> followUsers) {
        if (mSeatInfo == null) {
            return;
        }
        if (followUsers.contains(new UserState.UserInfo(mSeatInfo.userId.getValue()))) {
            mTextUnfollow.setVisibility(GONE);
            mImageFollowIcon.setVisibility(VISIBLE);
        } else {
            mImageFollowIcon.setVisibility(GONE);
            mTextUnfollow.setVisibility(VISIBLE);
        }
    }

    private void muteSeatAudio() {
        if (mSeatInfo == null) {
            return;
        }
        TUIRoomDefine.SeatLockParams params = new TUIRoomDefine.SeatLockParams();
        params.lockAudio = !mSeatInfo.isAudioLocked.getValue();
        params.lockSeat = mSeatInfo.isLocked.getValue();
        mSeatGridView.lockSeat(mSeatInfo.index, params, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorLocalized.onError(error);
            }
        });
    }

    private void updateAudioLockState(boolean isAudioLocked) {
        if (isAudioLocked) {
            mIvMute.setImageResource(R.drawable.livekit_ic_unmute_microphone);
            mTvMute.setText(R.string.common_voiceroom_unmuted_seat);
        } else {
            mIvMute.setImageResource(R.drawable.livekit_ic_mute_microphone);
            mTvMute.setText(R.string.common_voiceroom_mute_seat);
        }
    }

    private void updateUserIdState(String userId) {
        if (TextUtils.isEmpty(userId)) {
            dismiss();
        }
    }

    private void kickUserOffSeatByAdmin() {
        if (mSeatInfo == null) {
            return;
        }
        mSeatGridView.kickUserOffSeatByAdmin(mSeatInfo.userId.getValue(), new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "kickUserOffSeatByAdmin failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }
}