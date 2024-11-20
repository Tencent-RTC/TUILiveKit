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

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.error.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;

import java.util.LinkedHashSet;
import java.util.List;

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

    private final Observer<Boolean>                           isAudioLockedObserver    = this::updateAudioLockState;
    private final Observer<LinkedHashSet<UserState.UserInfo>> mMyFollowingUserObserver = this::onMyFollowingUserChanged;

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
        List<SeatState.SeatInfo> seatList = mVoiceRoomManager.getSeatState().seatList.get();
        if (seatIndex >= seatList.size()) {
            return;
        }
        mSeatInfo = seatList.get(seatIndex);
        if (mSeatInfo != null) {
            updateSeatInfoView();
            mVoiceRoomManager.getUserManager().checkFollowType(mSeatInfo.userId.get());
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
                mVoiceRoomManager.getUserState().selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER ? VISIBLE : GONE);
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
        if (TextUtils.isEmpty(mSeatInfo.userId.get())) {
            return;
        }
        String avatarUrl = mSeatInfo.avatarUrl.get();
        if (TextUtils.isEmpty(avatarUrl)) {
            mImageHeadView.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImageHeadView, avatarUrl, R.drawable.livekit_ic_avatar);
        }
        mUserNameText.setText(mSeatInfo.name.get());
        mUserIdText.setText(mContext.getString(R.string.livekit_user_id, mSeatInfo.userId.get()));
        updateAudioLockState(mSeatInfo.isAudioLocked.get());
    }

    private void addObserver() {
        mVoiceRoomManager.getUserState().myFollowingUserList.observe(mMyFollowingUserObserver);
        mSeatInfo.isAudioLocked.observe(isAudioLockedObserver);
    }

    private void removeObserver() {
        mVoiceRoomManager.getUserState().myFollowingUserList.removeObserver(mMyFollowingUserObserver);
        mSeatInfo.isAudioLocked.removeObserver(isAudioLockedObserver);
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
            if (mVoiceRoomManager.getUserState().myFollowingUserList.get().contains(
                    new UserState.UserInfo(mSeatInfo.userId.get()))) {
                mVoiceRoomManager.getUserManager().unfollow(mSeatInfo.userId.get());
            } else {
                mVoiceRoomManager.getUserManager().follow(mSeatInfo.userId.get());
            }
        });
    }

    private void onMyFollowingUserChanged(LinkedHashSet<UserState.UserInfo> followUsers) {
        if (mSeatInfo == null) {
            return;
        }
        if (followUsers.contains(new UserState.UserInfo(mSeatInfo.userId.get()))) {
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
        params.lockAudio = !mSeatInfo.isAudioLocked.get();
        params.lockSeat = mSeatInfo.isLocked.get();
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
            mTvMute.setText(R.string.livekit_cvoiceroom_unmuted_seat);
        } else {
            mIvMute.setImageResource(R.drawable.livekit_ic_mute_microphone);
            mTvMute.setText(R.string.livekit_voiceroom_mute_seat);
        }
    }

    private void kickUserOffSeatByAdmin() {
        if (mSeatInfo == null) {
            return;
        }
        mSeatGridView.kickUserOffSeatByAdmin(mSeatInfo.userId.get(), new TUIRoomDefine.ActionCallback() {
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