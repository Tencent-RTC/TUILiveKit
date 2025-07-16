package com.trtc.uikit.livekit.features.audiencecontainer.view.cohost.widgets;

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
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.view.BasicView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import java.util.List;
import java.util.Set;


public class CoHostWidgetsView extends BasicView {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("CoHostWidgetsView");

    private       ImageFilterView                mImageAvatar;
    private       LinearLayout                   mLayoutUserInfo;
    private       TextView                       mTextName;
    private       ImageView                      mImageMuteAudio;
    private final CoHostWidgetsViewState         mState                          = new CoHostWidgetsViewState();
    private final Observer<Set<String>>          mHasVideoStreamUserListObserver = this::onVideoStreamUserListChange;
    private final Observer<Set<String>>          mHasAudioStreamUserListObserver = this::onAudioStreamUserListChange;
    private final Observer<List<ConnectionUser>> mConnectionListObserver         = this::onConnectedListChange;
    private final Observer<Boolean>              mPictureInPictureObserver       = this::onPictureInPictureObserver;

    public CoHostWidgetsView(@NonNull Context context) {
        this(context, null);
    }

    public CoHostWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CoHostWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(AudienceManager manager, LiveCoreViewDefine.CoHostUser userInfo) {
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
                mAudienceManager.getCoreState().userState.hasAudioStreamUserList.getValue().contains(mState.userId);
        mImageMuteAudio.setVisibility(hasAudioStream ? GONE : VISIBLE);
    }

    private void initUserNameView() {
        if (mState.userId.equals(mCoreState.userState.selfInfo.getValue().userId) || (mState.userId.equals(mRoomState.roomInfo.ownerId)
                && !isConnected(mState.userId))) {
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
                mAudienceManager.getCoreState().userState.hasVideoStreamUserList.getValue().contains(userId) || mState.hasVideoStream;
        if (hasVideoStream) {
            mImageAvatar.setVisibility(GONE);
        } else {
            mImageAvatar.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageAvatar, mState.userAvatar, R.drawable.livekit_ic_avatar);
        }
    }

    @Override
    protected void addObserver() {
        mAudienceManager.getCoreState().userState.hasVideoStreamUserList.observeForever(mHasVideoStreamUserListObserver);
        mAudienceManager.getCoreState().userState.hasAudioStreamUserList.observeForever(mHasAudioStreamUserListObserver);
        mCoreState.coHostState.connectedUserList.observeForever(mConnectionListObserver);
        mMediaState.isPictureInPictureMode.observeForever(mPictureInPictureObserver);
    }

    @Override
    protected void removeObserver() {
        mAudienceManager.getCoreState().userState.hasVideoStreamUserList.removeObserver(mHasVideoStreamUserListObserver);
        mAudienceManager.getCoreState().userState.hasAudioStreamUserList.removeObserver(mHasAudioStreamUserListObserver);
        mCoreState.coHostState.connectedUserList.removeObserver(mConnectionListObserver);
        mMediaState.isPictureInPictureMode.removeObserver(mPictureInPictureObserver);
    }

    private void onVideoStreamUserListChange(Set<String> strings) {
        initUserAvatarView();
    }

    private void onAudioStreamUserListChange(Set<String> strings) {
        initMuteAudioView();
    }

    private void onConnectedListChange(List<ConnectionUser> connectionUsers) {
        initUserNameView();
        updateVisibility();
    }

    private void updateVisibility() {
        if (Boolean.TRUE.equals(mMediaState.isPictureInPictureMode.getValue()) || mCoreState.coHostState.connectedUserList.getValue().isEmpty()) {
            setVisibility(GONE);
        } else {
            setVisibility(VISIBLE);
        }
    }

    private boolean isConnected(String roomId) {
        for (ConnectionUser connectionUser : mCoreState.coHostState.connectedUserList.getValue()) {
            if (TextUtils.equals(connectionUser.roomId, roomId)) {
                return true;
            }
        }
        return false;
    }

    private void onPictureInPictureObserver(Boolean isPipMode) {
        updateVisibility();
    }
}
