package com.trtc.uikit.livekit.voiceroom.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.MediaManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.RoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.SeatManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.UserManager;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroom.state.MediaState;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;

public abstract class BasicView extends FrameLayout {
    protected Context          mContext;
    protected VoiceRoomManager mVoiceRoomManager;
    protected SeatGridView     mSeatGridView;
    protected SeatManager      mSeatManager;
    protected RoomManager      mRoomManager;
    protected UserManager      mUserManager;
    protected MediaManager     mMediaManager;

    protected RoomState  mRoomState;
    protected SeatState  mSeatState;
    protected UserState  mUserState;
    protected MediaState mMediaState;

    private boolean mIsAddObserver;

    public BasicView(@NonNull Context context) {
        this(context, null);
    }

    public BasicView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, null, 0);
    }

    public BasicView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        initView();
    }

    public void init(@NonNull VoiceRoomManager voiceRoomManager, SeatGridView seatGridView) {
        mSeatGridView = seatGridView;
        init(voiceRoomManager);
    }

    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        mVoiceRoomManager = voiceRoomManager;
        mSeatManager = voiceRoomManager.getSeatManager();
        mRoomManager = voiceRoomManager.getRoomManager();
        mUserManager = voiceRoomManager.getUserManager();
        mMediaManager = voiceRoomManager.getMediaManager();
        mRoomState = voiceRoomManager.getRoomState();
        mSeatState = voiceRoomManager.getSeatState();
        mUserState = voiceRoomManager.getUserState();
        mMediaState = voiceRoomManager.getMediaState();
        if (!mIsAddObserver) {
            addObserver();
            mIsAddObserver = true;
        }
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (mVoiceRoomManager == null) {
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

    protected abstract void addObserver();

    protected abstract void removeObserver();

}
