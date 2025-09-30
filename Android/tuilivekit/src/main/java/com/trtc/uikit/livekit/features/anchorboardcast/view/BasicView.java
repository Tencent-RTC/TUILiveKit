package com.trtc.uikit.livekit.features.anchorboardcast.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.BattleManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.CoHostManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.MediaManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.RoomManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.UserManager;
import com.trtc.uikit.livekit.features.anchorboardcast.state.BattleState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoGuestState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoHostState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.MediaState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.RoomState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.UserState;
import io.trtc.tuikit.atomicxcore.api.deprecated.LiveCoreViewDefine.CoreState;

public abstract class BasicView extends FrameLayout {
    protected final Context        mContext;
    protected       CoreState      mCoreState;
    protected       RoomState      mRoomState;
    protected       CoGuestState   mCoGuestState;
    protected       CoHostState    mCoHostState;
    protected       BattleState    mBattleState;
    protected       UserState      mUserState;
    protected       MediaState     mMediaState;
    protected       AnchorManager  mAnchorManager;
    protected       RoomManager    mRoomManager;
    protected       CoGuestManager mCoGuestManager;
    protected       CoHostManager  mCoHostManager;
    protected       BattleManager  mBattleManager;
    protected       UserManager    mUserManager;
    protected       MediaManager   mMediaManager;
    private         boolean        mIsAddObserver = false;

    public BasicView(@NonNull Context context) {
        this(context, null);
    }

    public BasicView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BasicView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        initView();
    }

    public void init(@NonNull AnchorManager liveStreamManager) {
        mAnchorManager = liveStreamManager;
        mRoomManager = liveStreamManager.getRoomManager();
        mCoGuestManager = liveStreamManager.getCoGuestManager();
        mUserManager = liveStreamManager.getUserManager();
        mMediaManager = liveStreamManager.getMediaManager();
        mCoHostManager = liveStreamManager.getCoHostManager();
        mBattleManager = liveStreamManager.getBattleManager();
        mRoomState = liveStreamManager.getRoomState();
        mCoGuestState = liveStreamManager.getCoGuestState();
        mUserState = liveStreamManager.getUserState();
        mMediaState = liveStreamManager.getMediaState();
        mCoHostState = liveStreamManager.getCoHostState();
        mBattleState = liveStreamManager.getBattleState();
        mCoreState = liveStreamManager.getCoreState();


        refreshView();
        if (!mIsAddObserver) {
            addObserver();
            mIsAddObserver = true;
        }
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (mAnchorManager == null) {
            return;
        }
        if (!mIsAddObserver) {
            addObserver();
            mIsAddObserver = true;
        }
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (mIsAddObserver) {
            removeObserver();
            mIsAddObserver = false;
        }
    }

    protected abstract void initView();

    protected abstract void refreshView();

    protected abstract void addObserver();

    protected abstract void removeObserver();
}
