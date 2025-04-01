package com.trtc.uikit.livekit.livestream.view.anchor.pushing.usermanage;

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

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.UserState;

import java.util.Set;

public class UserManagerDialog extends PopupDialog {
    private final Context            mContext;
    private final LiveStreamManager  mLiveStreamManager;
    private       UserState.UserInfo mUserInfo;

    private ImageFilterView mImageHeadView;
    private TextView        mUserIdText;
    private TextView        mUserNameText;
    private ImageView       mIvDisableMessage;
    private TextView        mTvDisableMessage;
    private TextView        mTextUnfollow;
    private ImageView       mImageFollowIcon;

    private ConfirmDialog mConfirmDialog;

    private final Observer<Set<UserState.UserInfo>> mMyFollowingUserObserver =
            this::onMyFollowingUserChanged;

    private final Observer<Boolean> mDisableMessageObserver = this::updateDisableMessageButton;

    public UserManagerDialog(@NonNull Context context, LiveStreamManager liveStreamManager) {
        super(context);
        mContext = context;
        mLiveStreamManager = liveStreamManager;
        initView();
    }

    public void init(TUIRoomDefine.UserInfo userInfo) {
        mUserInfo = mLiveStreamManager.getUserManager().getUserFromUserList(userInfo.userId);
        if (mUserInfo == null) {
            mUserInfo = mLiveStreamManager.getUserManager().addUserInUserList(userInfo);
        }
        updateView();
        mLiveStreamManager.getUserManager().checkFollowType(userInfo.userId);
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
        View rootView = View.inflate(mContext, R.layout.livekit_user_manager, null);
        setView(rootView);
        bindViewId(rootView);
        initFollowButtonView(rootView);
    }

    private void bindViewId(View rootView) {
        mUserIdText = rootView.findViewById(R.id.user_id);
        mUserNameText = rootView.findViewById(R.id.user_name);
        mImageHeadView = rootView.findViewById(R.id.iv_head);
        mIvDisableMessage = rootView.findViewById(R.id.iv_disable_message);
        mTvDisableMessage = rootView.findViewById(R.id.tv_disable_message);
        mTextUnfollow = rootView.findViewById(R.id.tv_unfollow);
        mImageFollowIcon = rootView.findViewById(R.id.iv_follow);
        rootView.findViewById(R.id.disable_message_container).setOnClickListener(v -> onDisableMessageButtonClicked());
        rootView.findViewById(R.id.kick_out_room_container).setOnClickListener(v -> onKickUserButtonClicked());
    }

    private void initFollowButtonView(View rootView) {
        rootView.findViewById(R.id.fl_follow_panel).setOnClickListener(view -> {
            if (mUserInfo == null) {
                return;
            }
            if (mLiveStreamManager.getUserState().myFollowingUserList.getValue().contains(
                    new UserState.UserInfo(mUserInfo.userId))) {
                mLiveStreamManager.getUserManager().unfollowUser(mUserInfo.userId);
            } else {
                mLiveStreamManager.getUserManager().followUser(mUserInfo.userId);
            }
        });
    }

    private void addObserver() {
        mLiveStreamManager.getUserState().myFollowingUserList.observeForever(mMyFollowingUserObserver);
        mUserInfo.isMessageDisabled.observeForever(mDisableMessageObserver);
        TUIRoomEngine.sharedInstance().addObserver(mTUIRoomObserver);
    }

    private void removeObserver() {
        mLiveStreamManager.getUserState().myFollowingUserList.removeObserver(mMyFollowingUserObserver);
        mUserInfo.isMessageDisabled.removeObserver(mDisableMessageObserver);
        TUIRoomEngine.sharedInstance().removeObserver(mTUIRoomObserver);
    }

    private void updateView() {
        if (mUserInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(mUserInfo.userId)) {
            return;
        }
        mUserIdText.setText(mContext.getString(R.string.live_user_id, mUserInfo.userId));
        String name = TextUtils.isEmpty(mUserInfo.name.getValue()) ? mUserInfo.userId : mUserInfo.name.getValue();
        mUserNameText.setText(name);
        if (TextUtils.isEmpty(mUserInfo.avatarUrl.getValue())) {
            mImageHeadView.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImageHeadView, mUserInfo.avatarUrl.getValue(), R.drawable.livekit_ic_avatar);
        }
    }

    private void updateDisableMessageButton(boolean isMessageDisabled) {
        if (isMessageDisabled) {
            mIvDisableMessage.setImageResource(R.drawable.livekit_ic_disable_message);
            mTvDisableMessage.setText(R.string.live_enable_message);
        } else {
            mIvDisableMessage.setImageResource(R.drawable.livekit_ic_enable_message);
            mTvDisableMessage.setText(R.string.live_disable_message);
        }
    }

    private void onMyFollowingUserChanged(Set<UserState.UserInfo> followUsers) {
        if (mUserInfo == null) {
            return;
        }
        if (followUsers.contains(new UserState.UserInfo(mUserInfo.userId))) {
            mTextUnfollow.setVisibility(GONE);
            mImageFollowIcon.setVisibility(VISIBLE);
        } else {
            mImageFollowIcon.setVisibility(GONE);
            mTextUnfollow.setVisibility(VISIBLE);
        }
    }

    private void onDisableMessageButtonClicked() {
        if (mUserInfo == null) {
            return;
        }
        boolean isMessageDisabled = mUserInfo.isMessageDisabled.getValue();
        mLiveStreamManager.getUserManager().disableSendingMessageByAdmin(mUserInfo.userId, !isMessageDisabled);
    }

    private void onKickUserButtonClicked() {
        if (mUserInfo == null) {
            return;
        }
        if (mConfirmDialog == null) {
            mConfirmDialog = new ConfirmDialog(mContext);
        }
        String name = TextUtils.isEmpty(mUserInfo.name.getValue()) ? mUserInfo.userId : mUserInfo.name.getValue();
        mConfirmDialog.setContent(mContext.getString(R.string.live_kick_user_confirm_message, name));
        mConfirmDialog.setPositiveText(mContext.getString(R.string.live_kick_out_of_room));
        mConfirmDialog.setPositiveListener(v -> {
            mLiveStreamManager.getUserManager().kickRemoteUserOutOfRoom(mUserInfo.userId);
            dismiss();
        });
        mConfirmDialog.show();
    }

    private final TUIRoomObserver mTUIRoomObserver = new TUIRoomObserver() {
        @Override
        public void onKickedOffSeat(int seatIndex, TUIRoomDefine.UserInfo operateUser) {
            dismiss();
            if (mConfirmDialog != null) {
                mConfirmDialog.dismiss();
            }
        }

        @Override
        public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
            if (mUserInfo == null || userInfo == null) {
                return;
            }
            if (TextUtils.isEmpty(mUserInfo.userId) || TextUtils.isEmpty(userInfo.userId)) {
                return;
            }
            if (userInfo.userId.equals(mUserInfo.userId)) {
                dismiss();
                if (mConfirmDialog != null) {
                    mConfirmDialog.dismiss();
                }
            }
        }
    };
}