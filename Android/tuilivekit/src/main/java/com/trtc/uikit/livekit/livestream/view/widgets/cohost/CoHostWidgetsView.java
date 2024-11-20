package com.trtc.uikit.livekit.livestream.view.widgets.cohost;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionUser;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.view.BasicView;

import java.util.LinkedHashSet;
import java.util.List;

public class CoHostWidgetsView extends BasicView {

    private       ImageFilterView                 mImageAvatar;
    private       TextView                        mTextName;
    private final CoHostWidgetsViewState          mState                          = new CoHostWidgetsViewState();
    private final Observer<LinkedHashSet<String>> mHasVideoStreamUserListObserver = this::onVideoStreamUserListChange;
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

    public void init(LiveStreamManager manager, TUILiveConnectionManager.ConnectionUser userInfo) {
        super.init(manager);
        mState.roomId = userInfo.roomId;
        mState.userId = userInfo.userId;
        mState.userName = userInfo.userName;
        mState.userAvatar = userInfo.avatarUrl;

        refreshView();
        addObserver();
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_co_guest_widgets_view, this, true);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mTextName = findViewById(R.id.tv_name);
    }

    @Override
    protected void refreshView() {
        initUserNameView();
        initUserAvatarView();
    }

    private void initUserNameView() {
        if (mState.userId.equals(mUserState.selfInfo.userId) || (mState.userId.equals(mRoomState.ownerInfo.userId)
                && !mCoHostManager.isConnected(mState.userId))) {
            mTextName.setText("");
            mTextName.setVisibility(GONE);
        } else {
            mTextName.setVisibility(VISIBLE);
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
        if (isPreview || hasVideoStream) {
            mImageAvatar.setVisibility(GONE);
            setBackgroundResource(R.color.livekit_design_standard_transparent);
        } else {
            setBackgroundResource(R.color.livekit_design_standard_g2);
            mImageAvatar.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageAvatar, mState.userAvatar, R.drawable.livekit_ic_avatar);
        }
    }

    @Override
    protected void addObserver() {
        mUserState.hasVideoStreamUserList.observe(mHasVideoStreamUserListObserver);
        mCoHostState.connectedUsers.observe(mConnectedUsersObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.observe(mFloatWindowModeObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.hasVideoStreamUserList.removeObserver(mHasVideoStreamUserListObserver);
        mCoHostState.connectedUsers.removeObserver(mConnectedUsersObserver);
        FloatWindowManager.getInstance().getStore().isShowingFloatWindow.removeObserver(mFloatWindowModeObserver);
    }

    private void onVideoStreamUserListChange(LinkedHashSet<String> strings) {
        initUserAvatarView();
    }

    private void onConnectedUsersChange(List<ConnectionUser> connectionUsers) {
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
