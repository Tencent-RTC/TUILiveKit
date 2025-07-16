package com.trtc.uikit.livekit.features.audiencecontainer.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.BattleManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.MediaManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.RoomManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.UserManager;
import com.trtc.uikit.livekit.features.audiencecontainer.state.BattleState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.MediaState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.RoomState;
import com.trtc.uikit.livekit.features.audiencecontainer.state.UserState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

public abstract class BasicView extends FrameLayout {
    protected final Context                      mContext;
    protected       LiveCoreViewDefine.CoreState mCoreState;
    protected       RoomState                    mRoomState;
    protected       CoGuestState                 mCoGuestState;
    protected       BattleState                  mBattleState;
    protected       UserState                    mUserState;
    protected       MediaState                   mMediaState;
    protected       AudienceManager              mAudienceManager;
    protected       RoomManager                  mRoomManager;
    protected       CoGuestManager               mCoGuestManager;
    protected       BattleManager                mBattleManager;
    protected       UserManager                  mUserManager;
    protected       MediaManager                 mMediaManager;
    private         boolean                      mIsAddObserver = false;

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

    public void init(@NonNull AudienceManager audienceManager) {
        mAudienceManager = audienceManager;
        mRoomManager = audienceManager.getRoomManager();
        mCoGuestManager = audienceManager.getCoGuestManager();
        mUserManager = audienceManager.getUserManager();
        mMediaManager = audienceManager.getMediaManager();
        mBattleManager = audienceManager.getBattleManager();
        mRoomState = audienceManager.getRoomState();
        mCoGuestState = audienceManager.getCoGuestState();
        mUserState = audienceManager.getUserState();
        mMediaState = audienceManager.getMediaState();
        mBattleState = audienceManager.getBattleState();
        mCoreState = audienceManager.getCoreState();
        refreshView();
        if (!mIsAddObserver) {
            addObserver();
            mIsAddObserver = true;
        }
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (mAudienceManager == null) {
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

    protected void refreshView() {
    }

    protected abstract void addObserver();

    protected abstract void removeObserver();
}
