package com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.coreviewwidget;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.List;
import java.util.Set;

public class CoGuestWidgetsView extends FrameLayout {

    private AnchorManager mManager;
    private LiveCoreView  mCoreView;
    private final ImageFilterView          mImageAvatar;
    private final LinearLayout             mLayoutUserInfo;
    private final TextView                 mTextName;
    private final ImageView                mImageMuteAudio;
    private final CoGuestWidgetsViewState  mState                          = new CoGuestWidgetsViewState();
    private final Observer<Set<String>>    mHasVideoStreamUserListObserver = this::onVideoStreamUserListChange;
    private final Observer<Set<String>>    mHasAudioStreamUserListObserver = this::onAudioStreamUserListChange;
    private final Observer<List<UserInfo>> mCoGuestStatusObserver          = this::onCoGuestUserListChange;

    public CoGuestWidgetsView(@NonNull Context context) {
        this(context, null);
    }

    public CoGuestWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CoGuestWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        LayoutInflater.from(context).inflate(R.layout.livekit_co_guest_widgets_view, this, true);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mLayoutUserInfo = findViewById(R.id.ll_user_info);
        mImageMuteAudio = findViewById(R.id.iv_mute_audio);
        mTextName = findViewById(R.id.tv_name);
    }

    public void init(AnchorManager manager, TUIRoomDefine.UserInfo userInfo, LiveCoreView coreView) {
        mManager = manager;
        mCoreView = coreView;
        mState.userId = userInfo.userId;
        mState.userName = userInfo.userName;
        mState.userAvatar = userInfo.avatarUrl;
        mState.hasVideoStream = userInfo.hasVideoStream;
        getUserInfo();

        initView();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void initView() {
        initUserNameView();
        initUserAvatarView();
    }

    private void getUserInfo() {
        mManager.getUserInfo(mState.userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                mState.userName = userInfo.userName;
                mState.userAvatar = userInfo.avatarUrl;
                initView();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {

            }
        });
    }

    private void initMuteAudioView() {
        if (mCoreView.getCoreState().userState.hasAudioStreamUserList.getValue() == null) {
            return;
        }
        boolean hasAudioStream =
                mCoreView.getCoreState().userState.hasAudioStreamUserList.getValue().contains(mState.userId);
        mImageMuteAudio.setVisibility(hasAudioStream ? GONE : VISIBLE);
    }

    private void initUserNameView() {
        if (mCoreView.getCoreState().roomState.ownerInfo.getValue() == null) {
            return;
        }
        if (mState.userId.equals(mManager.getState().loginUserInfo.userId)
                || mState.userId.equals(mCoreView.getCoreState().roomState.ownerInfo.getValue().userId)) {
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
                mCoreView.getCoreState().userState.hasVideoStreamUserList.getValue().contains(userId);
        if (hasVideoStream || mState.hasVideoStream) {
            mImageAvatar.setVisibility(GONE);
        } else {
            mImageAvatar.setVisibility(VISIBLE);
            ImageLoader.load(getContext(), mImageAvatar, mState.userAvatar, R.drawable.livekit_ic_avatar);
        }
    }

    protected void addObserver() {
        mCoreView.getCoreState().userState.hasVideoStreamUserList.observeForever(mHasVideoStreamUserListObserver);
        mCoreView.getCoreState().userState.hasAudioStreamUserList.observeForever(mHasAudioStreamUserListObserver);
        mCoreView.getCoreState().coGuestState.connectedUserList.observeForever(mCoGuestStatusObserver);
    }

    protected void removeObserver() {
        mCoreView.getCoreState().userState.hasVideoStreamUserList.removeObserver(mHasVideoStreamUserListObserver);
        mCoreView.getCoreState().userState.hasAudioStreamUserList.removeObserver(mHasAudioStreamUserListObserver);
        mCoreView.getCoreState().coGuestState.connectedUserList.removeObserver(mCoGuestStatusObserver);
    }

    private void onVideoStreamUserListChange(Set<String> lists) {
        Log.i("CoGuestWidgetsView", "onVideoStreamUserListChange, userId=" + mState.userId + ", lists=" + lists);
        initUserAvatarView();
    }

    private void onAudioStreamUserListChange(Set<String> lists) {
        Log.i("CoGuestWidgetsView", "onAudioStreamUserListChange, userId=" + mState.userId + ", lists=" + lists);
        initMuteAudioView();
    }

    private void onCoGuestUserListChange(List<UserInfo> lists) {
        Log.i("CoGuestWidgetsView", "onCoGuestUserLiveChange, userId=" + mState.userId + ", lists=" + lists);
        initUserNameView();
    }
}
