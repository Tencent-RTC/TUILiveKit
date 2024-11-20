package com.trtc.uikit.livekit.voiceroomcore.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.voiceroomcore.R;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridViewObserver;
import com.trtc.uikit.livekit.voiceroomcore.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class SeatInfoView extends FrameLayout {
    private static final int VOLUME_CAN_HEARD_MIN_LIMIT = 25;

    private final Context                     mContext;
    private final VoiceRoomManager            mVoiceRoomManager;
    private final SeatGridViewObserverManager mSeatGridViewObserverManager;
    private       TUIRoomDefine.SeatInfo      mSeatInfo;

    private ImageFilterView mImgHead;
    private View            mEmptyViewContainer;
    private ImageView       mIvEmptyView;
    private TextView        mTextName;
    private ImageView       mIvMute;
    private ImageView       mIvRoomOwner;
    private RippleView      mIvTalkBorder;
    private boolean         mIsShowTalkBorder;

    public SeatInfoView(@NonNull Context context, VoiceRoomManager voiceRoomManager,
                        SeatGridViewObserverManager observerManager, TUIRoomDefine.SeatInfo seatInfo) {
        super(context);
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        mSeatGridViewObserverManager = observerManager;
        mSeatInfo = seatInfo;
        initView();
        updateView(seatInfo);
    }

    public void updateSeatView(SeatGridView seatGridView, TUIRoomDefine.SeatInfo seatInfo) {
        mSeatInfo = seatInfo;
        updateView(seatInfo);
    }

    public void updateUserVolume(SeatGridView seatGridView, int volume) {
        String userId = mSeatInfo.userId;
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        if (!mVoiceRoomManager.getUserState().hasAudioStreamUserList.get().contains(userId)) {
            mIvTalkBorder.setVisibility(GONE);
            mIvMute.setVisibility(VISIBLE);
            return;
        }

        boolean isShowTalkBorder = volume > VOLUME_CAN_HEARD_MIN_LIMIT;
        if (isShowTalkBorder == mIsShowTalkBorder) {
            return;
        }
        mIvTalkBorder.setVisibility(isShowTalkBorder ? VISIBLE : GONE);
        mIsShowTalkBorder = isShowTalkBorder;
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.voiceroomcore_seat_info_view, this, true);
        setOnClickListener(v -> onItemViewClicked(v, mSeatInfo));
        mTextName = findViewById(R.id.tv_name);
        mImgHead = findViewById(R.id.iv_head);
        mEmptyViewContainer = findViewById(R.id.empty_seat_container);
        mIvEmptyView = findViewById(R.id.iv_empty_seat);
        mIvMute = findViewById(R.id.iv_mute);
        mIvTalkBorder = findViewById(R.id.iv_talk_border);
        mIvRoomOwner = findViewById(R.id.iv_room_owner);
    }

    private void addObserver() {
        mVoiceRoomManager.getUserState().hasAudioStreamUserList.observe(hasAudioStreamUserListObserver);
    }

    private void removeObserver() {
        mVoiceRoomManager.getUserState().hasAudioStreamUserList.removeObserver(hasAudioStreamUserListObserver);
    }

    private void updateView(TUIRoomDefine.SeatInfo seatInfo) {
        if (TextUtils.isEmpty(seatInfo.userId)) {
            updateEmptySeatView(seatInfo);
        } else {
            updateSeatedView(seatInfo);
        }
    }

    private void updateEmptySeatView(TUIRoomDefine.SeatInfo seatInfo) {
        mEmptyViewContainer.setVisibility(View.VISIBLE);
        mIvEmptyView.setImageResource(seatInfo.isLocked ? R.drawable.voiceroomcore_ic_lock :
                R.drawable.voiceroomcore_empty_seat);
        mImgHead.setVisibility(View.GONE);
        mIvMute.setVisibility(View.GONE);
        mIvTalkBorder.setVisibility(View.GONE);
        mIsShowTalkBorder = false;
        mTextName.setVisibility(View.VISIBLE);
        mTextName.setText(String.valueOf(seatInfo.index + 1));
        mIvRoomOwner.setVisibility(View.GONE);
    }

    private void updateSeatedView(TUIRoomDefine.SeatInfo seatInfo) {
        mEmptyViewContainer.setVisibility(View.GONE);
        mTextName.setText(TextUtils.isEmpty(seatInfo.userName) ? seatInfo.userId : seatInfo.userName);
        updateUserAvatar(seatInfo.avatarUrl);
        updateUserRole(seatInfo.userId);
        updateMuteState(mVoiceRoomManager.getUserState().hasAudioStreamUserList.get());
        if (seatInfo.isAudioLocked) {
            mIvMute.setVisibility(View.VISIBLE);
            mIvTalkBorder.setVisibility(View.GONE);
            mIsShowTalkBorder = false;
        }
    }

    private void updateMuteState(LinkedHashSet<String> list) {
        if (TextUtils.isEmpty(mSeatInfo.userId)) {
            return;
        }
        boolean hasAudioStream = list.contains(mSeatInfo.userId);
        if (hasAudioStream) {
            mIvMute.setVisibility(GONE);
        } else {
            mIvTalkBorder.setVisibility(GONE);
            mIsShowTalkBorder = false;
            mIvMute.setVisibility(VISIBLE);
        }
    }

    private void updateUserRole(String userId) {
        boolean isOwner = mVoiceRoomManager.getRoomState().ownerId.equals(userId);
        mIvRoomOwner.setVisibility(isOwner ? View.VISIBLE : View.GONE);
    }

    private void updateUserAvatar(String avatarUrl) {
        mImgHead.setVisibility(View.VISIBLE);
        if (TextUtils.isEmpty(avatarUrl)) {
            mImgHead.setImageResource(R.drawable.voiceroomcore_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImgHead, avatarUrl, R.drawable.voiceroomcore_ic_avatar);
        }
    }

    private void onItemViewClicked(View view, TUIRoomDefine.SeatInfo seatInfo) {
        for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
            observer.onSeatViewClicked(view, seatInfo);
        }
    }

    private final Observer<LinkedHashSet<String>> hasAudioStreamUserListObserver = this::updateMuteState;
}
