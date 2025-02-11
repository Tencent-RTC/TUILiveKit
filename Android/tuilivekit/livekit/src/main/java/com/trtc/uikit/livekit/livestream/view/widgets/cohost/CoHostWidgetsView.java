package com.trtc.uikit.livekit.livestream.view.widgets.cohost;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionUser;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import java.util.LinkedHashSet;
import java.util.List;

public class CoHostWidgetsView extends BasicView {

    private       ImageFilterView                 mImageAvatar;
    private       LinearLayout                    mLayoutUserInfo;
    private       TextView                        mTextName;
    private       ImageView                       mImageMuteAudio;
    private final CoHostWidgetsViewState          mState                          = new CoHostWidgetsViewState();
    private final Observer<LinkedHashSet<String>> mHasVideoStreamUserListObserver = this::onVideoStreamUserListChange;
    private final Observer<LinkedHashSet<String>> mHasAudioStreamUserListObserver = this::onAudioStreamUserListChange;
    private final Observer<List<ConnectionUser>>  mConnectedUsersObserver         = this::onConnectedUsersChange;
    private final Observer<Boolean>               mFloatWindowModeObserver        = this::onFloatWindowModeObserver;

    public CoHostWidgetsView(@NonNull Context context) {
        this(context, null);
    }

    public CoHostWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CoHostWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(LiveStreamManager manager, LiveCoreViewDefine.CoHostUser userInfo) {
        mState.roomId = userInfo.connectionUser.roomId;
        mState.userId = userInfo.connectionUser.userId;
        mState.userName = userInfo.connectionUser.userName;
        mState.userAvatar = userInfo.connectionUser.avatarUrl;
        mState.hasVideoStream = userInfo.hasVideoStream;
        super.init(manager);
        getUserInfo();
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_co_guest_widgets_view, this, true);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mLayoutUserInfo = findViewById(R.id.ll_user_info);
        mImageMuteAudio = findViewById(R.id.iv_mute_audio);
        mTextName = findViewById(R.id.tv_name);
    }

    @Override
    protected void refreshView() {
        initUserNameView();
        initUserAvatarView();
    }

    private void getUserInfo() {
        mUserManager.getUserInfo(mState.userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                mState.userName = userInfo.userName;
                mState.userAvatar = userInfo.avatarUrl;
                refreshView();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {

            }
        });
    }

    private void initMuteAudioView() {
        boolean hasAudioStream = mUserState.hasAudioStreamUserList.get().contains(mState.userId);
        mImageMuteAudio.setVisibility(hasAudioStream ? GONE : VISIBLE);
    }

    private void initUserNameView() {
        if (mState.userId.equals(mUserState.selfInfo.userId) || (mState.userId.equals(mRoomState.ownerInfo.userId)
                && !mCoHostManager.isConnected(mState.userId))) {
            mLayoutUserInfo.setVisibility(GONE);
        } else {
            mLayoutUserInfo.setVisibility(VISIBLE);
            mTextName.setText(mState.userName);
        }
    }

    private void initUserAvatarView() {
        String userId = mState.userId;
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        boolean hasVideoStream = mUserState.hasVideoStreamUserList.get().contains(userId) || mState.hasVideoStream;
        boolean isPreview = RoomState.LiveStatus.PREVIEWING == mRoomState.liveStatus.get();
        if (isPreview || hasVideoStream) {
            mImageAvatar.setVisibility(GONE);
        } else {
            mImageAvatar.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageAvatar, mState.userAvatar, R.drawable.livekit_ic_avatar);
        }
    }

    @Override
    protected void addObserver() {
        mUserState.hasVideoStreamUserList.observe(mHasVideoStreamUserListObserver);
        mUserState.hasAudioStreamUserList.observe(mHasAudioStreamUserListObserver);
        mCoHostState.connectedUsers.observe(mConnectedUsersObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.observe(mFloatWindowModeObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.hasVideoStreamUserList.removeObserver(mHasVideoStreamUserListObserver);
        mUserState.hasAudioStreamUserList.removeObserver(mHasAudioStreamUserListObserver);
        mCoHostState.connectedUsers.removeObserver(mConnectedUsersObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.removeObserver(mFloatWindowModeObserver);
    }

    private void onVideoStreamUserListChange(LinkedHashSet<String> strings) {
        initUserAvatarView();
    }

    private void onAudioStreamUserListChange(LinkedHashSet<String> strings) {
        initMuteAudioView();
    }

    private void onConnectedUsersChange(List<ConnectionUser> connectionUsers) {
        initUserNameView();
        updateVisibility();
    }

    private void onFloatWindowModeObserver(Boolean isFloating) {
        updateVisibility();
    }

    private void updateVisibility() {
        if (Boolean.TRUE.equals(FloatWindowManager.getInstance().getStore().isShowingFloatWindow.get())
                || mCoHostState.connectedUsers.get().isEmpty()) {
            setVisibility(GONE);
        } else {
            setVisibility(VISIBLE);
        }
    }
}
