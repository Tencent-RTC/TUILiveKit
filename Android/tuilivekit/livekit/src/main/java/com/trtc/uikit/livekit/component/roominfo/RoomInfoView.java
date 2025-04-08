package com.trtc.uikit.livekit.component.roominfo;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.lifecycle.Observer;

import com.google.android.material.imageview.ShapeableImageView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.DataReporter;
import com.trtc.uikit.livekit.component.roominfo.service.RoomInfoService;
import com.trtc.uikit.livekit.component.roominfo.store.RoomInfoState;
import com.trtc.uikit.livekit.component.roominfo.view.RoomInfoPopupDialog;

import java.util.Set;

@SuppressLint("ViewConstructor")
public class RoomInfoView extends FrameLayout {

    private static final int LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_LIVE_INFO  = 190009;
    private static final int LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_LIVE_INFO = 191008;

    private final Context               mContext;
    private       TextView              mTextNickName;
    private       ShapeableImageView    mImageAvatar;
    private       LinearLayout          mLayoutRoot;
    private       TextView              mTextUnfollow;
    private       ImageView             mImageFollowIcon;
    private       FrameLayout           mLayoutFollowPanel;
    private       RoomInfoPopupDialog   mRoomInfoPopupDialog;
    private final RoomInfoService       mRoomInfoService      = new RoomInfoService();
    private final RoomInfoState         mRoomInfoState        = mRoomInfoService.mRoomInfoState;
    private final Observer<String>      mHostIdObserver       = this::onHostIdChange;
    private final Observer<String>      mHostNickNameObserver = this::onHostNickNameChange;
    private final Observer<String>      mHostAvatarObserver   = this::onHostAvatarChange;
    private final Observer<Set<String>> mFollowStatusObserver = this::onFollowStatusChange;

    private final TUIRoomObserver mRoomObserver = new TUIRoomObserver() {
        @Override
        public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
            if (mRoomInfoPopupDialog != null) {
                mRoomInfoPopupDialog.dismiss();
            }
        }
    };

    public RoomInfoView(Context context) {
        this(context, null);
    }

    public RoomInfoView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public RoomInfoView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.room_info_view, this, true);
        bindViewId();
    }

    public void init(String roomId) {
        init(roomId, true);
    }

    public void init(String roomId, boolean enableFollow) {
        mRoomInfoService.initRoomInfo(roomId);
        mRoomInfoState.enableFollow = enableFollow;
        reportData(roomId);
        refreshView();
    }

    private void initView() {
        initHostNameView();
        initHostAvatarView();
        initRoomInfoPanelView();
    }

    private void refreshView() {
        if (!mRoomInfoState.enableFollow) {
            mLayoutFollowPanel.setVisibility(GONE);
        }
    }

    private void addObserver() {
        TUIRoomEngine.sharedInstance().addObserver(mRoomObserver);
        mRoomInfoState.ownerId.observeForever(mHostIdObserver);
        mRoomInfoState.ownerName.observeForever(mHostNickNameObserver);
        mRoomInfoState.ownerAvatarUrl.observeForever(mHostAvatarObserver);
        mRoomInfoState.followingList.observeForever(mFollowStatusObserver);
    }

    private void removeObserver() {
        TUIRoomEngine.sharedInstance().removeObserver(mRoomObserver);
        mRoomInfoState.ownerId.removeObserver(mHostIdObserver);
        mRoomInfoState.ownerName.removeObserver(mHostNickNameObserver);
        mRoomInfoState.ownerAvatarUrl.removeObserver(mHostAvatarObserver);
        mRoomInfoState.followingList.removeObserver(mFollowStatusObserver);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();

        initView();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void bindViewId() {
        mLayoutRoot = findViewById(R.id.ll_root);
        mTextNickName = findViewById(R.id.tv_name);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mTextUnfollow = findViewById(R.id.tv_unfollow);
        mImageFollowIcon = findViewById(R.id.iv_follow);
        mLayoutFollowPanel = findViewById(R.id.fl_follow_panel);
    }

    private void initHostNameView() {
        mTextNickName.setText(mRoomInfoState.ownerName.getValue());
    }

    private void initHostAvatarView() {
        ImageLoader.load(mContext, mImageAvatar, mRoomInfoState.ownerAvatarUrl.getValue(),
                R.drawable.room_info_default_avatar);
    }

    private void initRoomInfoPanelView() {
        mLayoutRoot.setOnClickListener(view -> {
            if (mRoomInfoPopupDialog == null) {
                mRoomInfoPopupDialog = new RoomInfoPopupDialog(mContext, mRoomInfoService);
            }
            mRoomInfoPopupDialog.show();
        });
    }

    private void onHostIdChange(String ownerId) {
        if (!mRoomInfoState.enableFollow) {
            return;
        }
        if (!TextUtils.isEmpty(ownerId) && !TextUtils.equals(mRoomInfoState.myUserId, ownerId)) {
            mLayoutFollowPanel.setVisibility(View.VISIBLE);
            mRoomInfoService.isFollow(ownerId);
            refreshFollowButton();
            mLayoutFollowPanel.setOnClickListener(this::onFollowButtonClick);
        }
    }

    private void onHostNickNameChange(String name) {
        mTextNickName.setText(name);
    }

    private void onHostAvatarChange(String avatar) {
        ImageLoader.load(mContext, mImageAvatar, avatar, R.drawable.room_info_default_avatar);
    }

    private void onFollowStatusChange(Set<String> followUsers) {
        refreshFollowButton();
    }

    private void onFollowButtonClick(View view) {
        if (mRoomInfoState.followingList.getValue().contains(mRoomInfoState.ownerId.getValue())) {
            mRoomInfoService.unfollowUser(mRoomInfoState.ownerId.getValue());
        } else {
            mRoomInfoService.followUser(mRoomInfoState.ownerId.getValue());
        }
    }

    private void refreshFollowButton() {
        if (!mRoomInfoState.followingList.getValue().contains(mRoomInfoState.ownerId.getValue())) {
            mImageFollowIcon.setVisibility(GONE);
            mTextUnfollow.setVisibility(View.VISIBLE);
        } else {
            mTextUnfollow.setVisibility(View.GONE);
            mImageFollowIcon.setVisibility(VISIBLE);
        }
    }

    private void reportData(String roomId) {
        boolean isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_");
        if (isVoiceRoom) {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_LIVE_INFO);
        } else {
            DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_LIVE_INFO);
        }
    }
}
