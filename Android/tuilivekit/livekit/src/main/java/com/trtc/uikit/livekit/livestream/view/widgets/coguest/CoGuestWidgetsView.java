package com.trtc.uikit.livekit.livestream.view.widgets.coguest;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
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
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.view.BasicView;

import java.util.LinkedHashSet;

public class CoGuestWidgetsView extends BasicView {

    private       ImageFilterView                 mImageAvatar;
    private       LinearLayout                    mLayoutUserInfo;
    private       TextView                        mTextName;
    private       ImageView                       mImageMuteAudio;
    private final CoGuestWidgetsViewState         mState                          = new CoGuestWidgetsViewState();
    private final Observer<LinkedHashSet<String>> mHasVideoStreamUserListObserver = this::onVideoStreamUserListChange;
    private final Observer<LinkedHashSet<String>> mHasAudioStreamUserListObserver = this::onAudioStreamUserListChange;
    private final Observer<CoGuestStatus>         mCoGuestStatusObserver          = this::onCoGuestStatusChange;
    private final Observer<Boolean>               mFloatWindowModeObserver        = this::onFloatWindowModeObserver;

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
        boolean hasAudioStream = mUserState.hasAudioStreamUserList.get().contains(mState.userId);
        mImageMuteAudio.setVisibility(hasAudioStream ? GONE : VISIBLE);
    }

    private void initUserNameView() {
        if (mState.userId.equals(mUserState.selfInfo.userId) || (mState.userId.equals(mRoomState.ownerInfo.userId)
                && mCoGuestState.coGuestStatus.get() == CoGuestState.CoGuestStatus.NONE)) {
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
        boolean hasVideoStream = mUserState.hasVideoStreamUserList.get().contains(userId);
        boolean isPreview = RoomState.LiveStatus.PREVIEWING == mRoomState.liveStatus.get();
        if (isPreview || hasVideoStream || mState.hasVideoStream) {
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
        mCoGuestState.coGuestStatus.observe(mCoGuestStatusObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.observe(mFloatWindowModeObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.hasVideoStreamUserList.removeObserver(mHasVideoStreamUserListObserver);
        mUserState.hasAudioStreamUserList.removeObserver(mHasAudioStreamUserListObserver);
        mCoGuestState.coGuestStatus.removeObserver(mCoGuestStatusObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.removeObserver(mFloatWindowModeObserver);
    }

    private void onVideoStreamUserListChange(LinkedHashSet<String> strings) {
        Log.i("CoGuestWidgetsView", "onVideoStreamUserListChange, userId=" + mState.userId + ", strings=" + strings);
        initUserAvatarView();
    }

    private void onAudioStreamUserListChange(LinkedHashSet<String> strings) {
        Log.i("CoGuestWidgetsView", "onAudioStreamUserListChange, userId=" + mState.userId + ", strings=" + strings);
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
