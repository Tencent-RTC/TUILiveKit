package com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.widgets;

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
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.view.BasicView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import java.util.List;
import java.util.Set;

public class CoHostWidgetsView extends BasicView {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("CoHostWidgetsView");

    private       ImageFilterView                mImageAvatar;
    private       LinearLayout                   mLayoutUserInfo;
    private       TextView                       mTextName;
    private       ImageView                      mImageMuteAudio;
    private final CoHostWidgetsViewState         mState                          = new CoHostWidgetsViewState();
    private final Observer<Set<String>>          mHasVideoStreamUserListObserver = this::onVideoStreamUserListChange;
    private final Observer<Set<String>>          mHasAudioStreamUserListObserver = this::onAudioStreamUserListChange;
    private final Observer<Boolean>              mCameraOpenObserver             = this::onCameraOpenChange;
    private final Observer<List<ConnectionUser>> mConnectedUsersObserver         = this::onConnectedUsersChange;
    private final Observer<Boolean>              mPipModeObserver                = this::onPipModeObserver;

    public CoHostWidgetsView(@NonNull Context context) {
        this(context, null);
    }

    public CoHostWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CoHostWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(AnchorManager manager, LiveCoreViewDefine.CoHostUser userInfo) {
        LOGGER.info("init userId:" + userInfo.connectionUser.userId + ",roomId:" + userInfo.connectionUser.roomId);
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
        boolean hasAudioStream =
                mAnchorManager.getCoreState().userState.hasAudioStreamUserList.getValue().contains(mState.userId);
        mImageMuteAudio.setVisibility(hasAudioStream ? GONE : VISIBLE);
    }

    private void initUserNameView() {
        if (mState.userId.equals(mCoreState.userState.selfInfo.getValue().userId)
                || (mState.userId.equals(mCoreState.roomState.ownerInfo.getValue().userId)
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
        boolean hasVideoStream =
                mAnchorManager.getCoreState().userState.hasVideoStreamUserList.getValue().contains(userId) || mState.hasVideoStream;
        if (hasVideoStream || (isSelf() && mAnchorManager.getCoreState().mediaState.isCameraOpened.getValue())) {
            mImageAvatar.setVisibility(GONE);
        } else {
            mImageAvatar.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageAvatar, mState.userAvatar, R.drawable.livekit_ic_avatar);
        }
    }

    private boolean isSelf() {
        return TextUtils.equals(mState.userId, mCoreState.userState.selfInfo.getValue().userId);
    }

    @Override
    protected void addObserver() {
        mAnchorManager.getCoreState().mediaState.isCameraOpened.observeForever(mCameraOpenObserver);
        mAnchorManager.getCoreState().userState.hasVideoStreamUserList.observeForever(mHasVideoStreamUserListObserver);
        mAnchorManager.getCoreState().userState.hasAudioStreamUserList.observeForever(mHasAudioStreamUserListObserver);
        mCoreState.coHostState.connectedUserList.observeForever(mConnectedUsersObserver);
        mMediaState.isPipModeEnabled.observeForever(mPipModeObserver);
    }

    @Override
    protected void removeObserver() {
        mAnchorManager.getCoreState().mediaState.isCameraOpened.removeObserver(mCameraOpenObserver);
        mAnchorManager.getCoreState().userState.hasVideoStreamUserList.removeObserver(mHasVideoStreamUserListObserver);
        mAnchorManager.getCoreState().userState.hasAudioStreamUserList.removeObserver(mHasAudioStreamUserListObserver);
        mCoreState.coHostState.connectedUserList.removeObserver(mConnectedUsersObserver);
        mMediaState.isPipModeEnabled.removeObserver(mPipModeObserver);
    }

    private void onVideoStreamUserListChange(Set<String> strings) {
        initUserAvatarView();
    }

    private void onAudioStreamUserListChange(Set<String> strings) {
        initMuteAudioView();
    }

    private void onConnectedUsersChange(List<ConnectionUser> connectionUsers) {
        initUserNameView();
        updateVisibility();
    }

    private void onCameraOpenChange(Boolean aBoolean) {
        initUserAvatarView();
    }

    private void onPipModeObserver(Boolean isPipMode) {
        updateVisibility();
    }

    private void updateVisibility() {
        if (Boolean.TRUE.equals(mMediaState.isPipModeEnabled.getValue()) || mCoreState.coHostState.connectedUserList.getValue().isEmpty()) {
            setVisibility(GONE);
        } else {
            setVisibility(VISIBLE);
        }
    }
}
