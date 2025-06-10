package com.trtc.uikit.livekit.livestream.view.widgets.coguest;

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
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.view.BasicView;

import java.util.Set;

public class CoGuestWidgetsView extends BasicView {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("CoGuestWidgetsView");

    private       ImageFilterView         mImageAvatar;
    private       LinearLayout            mLayoutUserInfo;
    private       TextView                mTextName;
    private       ImageView               mImageMuteAudio;
    private final CoGuestWidgetsViewState mState                          = new CoGuestWidgetsViewState();
    private final Observer<Set<String>>   mHasVideoStreamUserListObserver = this::onVideoStreamUserListChange;
    private final Observer<Set<String>>   mHasAudioStreamUserListObserver = this::onAudioStreamUserListChange;
    private final Observer<CoGuestStatus> mCoGuestStatusObserver          = this::onCoGuestStatusChange;
    private final Observer<Boolean>       mFloatWindowModeObserver        = this::onFloatWindowModeObserver;

    public CoGuestWidgetsView(@NonNull Context context) {
        this(context, null);
    }

    public CoGuestWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CoGuestWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(LiveStreamManager manager, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info("init userId:" + userInfo.userId);
        mState.userId = userInfo.userId;
        mState.userName = userInfo.userName;
        mState.userAvatar = userInfo.avatarUrl;
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
        boolean hasAudioStream = mLiveManager.getCoreState().userState.hasAudioStreamUserList.getValue().contains(mState.userId);
        mImageMuteAudio.setVisibility(hasAudioStream ? GONE : VISIBLE);
    }

    private void initUserNameView() {
        if (mState.userId.equals(mUserState.selfInfo.userId) || (mState.userId.equals(mRoomState.ownerInfo.userId)
                && mCoGuestState.coGuestStatus.getValue() != CoGuestStatus.LINKING)) {
            mLayoutUserInfo.setVisibility(GONE);
        } else {
            mLayoutUserInfo.setVisibility(VISIBLE);
            mTextName.setText(mState.userName);
        }
    }

    private boolean isAnchor() {
        return TextUtils.equals(mState.userId, mRoomState.ownerInfo.userId);
    }

    private void initUserAvatarView() {
        String userId = mState.userId;
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        boolean hasVideoStream = mLiveManager.getCoreState().userState.hasVideoStreamUserList.getValue().contains(userId);
        boolean isPreview = RoomState.LiveStatus.PREVIEWING == mRoomState.liveStatus.getValue();
        if (isPreview || hasVideoStream || mState.hasVideoStream) {
            mImageAvatar.setVisibility(GONE);
        } else {
            mImageAvatar.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageAvatar, mState.userAvatar, R.drawable.livekit_ic_avatar);
        }
    }

    @Override
    protected void addObserver() {
        mLiveManager.getCoreState().userState.hasVideoStreamUserList.observeForever(mHasVideoStreamUserListObserver);
        mLiveManager.getCoreState().userState.hasAudioStreamUserList.observeForever(mHasAudioStreamUserListObserver);
        mCoGuestState.coGuestStatus.observeForever(mCoGuestStatusObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.observeForever(mFloatWindowModeObserver);
    }

    @Override
    protected void removeObserver() {
        mLiveManager.getCoreState().userState.hasVideoStreamUserList.removeObserver(mHasVideoStreamUserListObserver);
        mLiveManager.getCoreState().userState.hasAudioStreamUserList.removeObserver(mHasAudioStreamUserListObserver);
        mCoGuestState.coGuestStatus.removeObserver(mCoGuestStatusObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.removeObserver(mFloatWindowModeObserver);
    }
 
    private void onVideoStreamUserListChange(Set<String> strings) {
        initUserAvatarView();
    }

    private void onAudioStreamUserListChange(Set<String> strings) {
        initMuteAudioView();
    }

    private void onCoGuestStatusChange(CoGuestStatus coGuestStatus) {
        initUserNameView();
    }

    private void onFloatWindowModeObserver(Boolean isFloating) {
        if (Boolean.TRUE.equals(isFloating)) {
            setVisibility(GONE);
        } else {
            setVisibility(VISIBLE);
        }
    }
}
