package com.trtc.uikit.livekit.livestream.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.module.BattleManager;
import com.trtc.uikit.livekit.livestream.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.livestream.manager.module.CoHostManager;
import com.trtc.uikit.livekit.livestream.manager.module.MediaManager;
import com.trtc.uikit.livekit.livestream.manager.module.RoomManager;
import com.trtc.uikit.livekit.livestream.manager.module.UserManager;
import com.trtc.uikit.livekit.livestream.state.BattleState;
import com.trtc.uikit.livekit.livestream.state.BeautyState;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.DashboardState;
import com.trtc.uikit.livekit.livestream.state.MediaState;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;

public abstract class BasicView extends FrameLayout {
    protected Context           mContext;
    protected RoomState         mRoomState;
    protected CoGuestState      mCoGuestState;
    protected CoHostState       mCoHostState;
    protected BattleState       mBattleState;
    protected UserState         mUserState;
    protected MediaState        mMediaState;
    protected BeautyState       mBeautyState;
    protected DashboardState    mDashboardState;
    protected LiveStreamManager mLiveManager;
    protected RoomManager       mRoomManager;
    protected CoGuestManager    mCoGuestManager;
    protected CoHostManager     mCoHostManager;
    protected BattleManager     mBattleManager;
    protected UserManager       mUserManager;
    protected MediaManager      mMediaManager;
    private   boolean           mIsAddObserver = false;

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

    public void init(@NonNull LiveStreamManager liveStreamManager) {
        mLiveManager = liveStreamManager;
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
        mBeautyState = liveStreamManager.getBeautyState();
        mCoHostState = liveStreamManager.getCoHostState();
        mBattleState = liveStreamManager.getBattleState();
        mDashboardState = liveStreamManager.getDashboardState();

        refreshView();
        if (!mIsAddObserver) {
            addObserver();
            mIsAddObserver = true;
        }
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (mLiveManager == null) {
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
