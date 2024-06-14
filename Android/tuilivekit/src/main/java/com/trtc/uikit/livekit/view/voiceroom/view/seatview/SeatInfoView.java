package com.trtc.uikit.livekit.view.voiceroom.view.seatview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.view.voiceroom.model.ListMenuInfo;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.voiceroom.model.MenuDataGenerate;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.view.voiceroom.view.panel.seatactionsheet.SeatActionSheetPanel;

import java.util.LinkedHashSet;
import java.util.List;

@SuppressLint("ViewConstructor")
public class SeatInfoView extends BasicView {
    private final SeatState.SeatInfo mSeatInfo;

    private ImageFilterView mImgHead;
    private TextView        mTextName;
    private ImageView       mIvMute;
    private ImageView       mIvTalkBorder;

    private SeatActionSheetPanel mSeatActionSheetPanel;

    private final MenuDataGenerate mMenuDataGenerate;

    private final Observer<LinkedHashSet<String>> hasAudioStreamUserListObserver = this::updateMuteState;

    private final Observer<LinkedHashSet<String>> hasAudioVolumeUserListObserver = this::updateUserVolumeState;

    private final Observer<String> userIdObserver = this::updateUserId;

    private final Observer<String> nameObserver = this::updateName;

    private final Observer<String> avatarObserver = this::updateAvatar;

    private final Observer<Boolean> isLockedObserver = this::updateLockState;

    private final Observer<Boolean> isAudioLockedObserver = this::updateAudioLockState;

    public SeatInfoView(@NonNull Context context, LiveController liveController, SeatState.SeatInfo seatInfo) {
        super(context, liveController);
        mSeatInfo = seatInfo;
        mMenuDataGenerate = new MenuDataGenerate(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_seat_info_view, this, true);
        mTextName = findViewById(R.id.tv_name);
        mImgHead = findViewById(R.id.iv_head);
        mIvMute = findViewById(R.id.iv_mute);
        mIvTalkBorder = findViewById(R.id.iv_talk_border);
        updateLockState(mSeatInfo.isLocked.get());
        mImgHead.setOnClickListener(view -> {
            List<ListMenuInfo> listMenuInfoList = mMenuDataGenerate.generateOperateSeatMenuInfo(mSeatInfo);
            if (listMenuInfoList.isEmpty()) {
                return;
            }
            if (mSeatActionSheetPanel == null) {
                mSeatActionSheetPanel = new SeatActionSheetPanel(mContext);
            }
            mSeatActionSheetPanel.updateActionButton(listMenuInfoList);
            mSeatActionSheetPanel.show();
        });
    }

    private void updateMuteState(LinkedHashSet<String> list) {
        String userId = mSeatInfo.userId.get();
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        boolean hasAudioStream = list.contains(userId);
        if (hasAudioStream) {
            mIvMute.setVisibility(GONE);
        } else {
            mIvTalkBorder.setVisibility(GONE);
            mIvMute.setVisibility(VISIBLE);
        }
    }

    private void updateUserVolumeState(LinkedHashSet<String> list) {
        String userId = mSeatInfo.userId.get();
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        if (!mUserState.hasAudioStreamUserList.get().contains(userId)) {
            mIvTalkBorder.setVisibility(GONE);
            mIvMute.setVisibility(VISIBLE);
            return;
        }
        boolean hasAudioVolume = list.contains(userId);
        mIvTalkBorder.setVisibility(hasAudioVolume ? VISIBLE : GONE);
    }

    private void updateEmptySeatView() {
        mImgHead.setImageResource(R.drawable.livekit_voiceroom_add_seat);
        mTextName.setText("");
        mIvMute.setVisibility(View.GONE);
        mIvTalkBorder.setVisibility(View.GONE);
    }

    private void updateUserId(String userId) {
        if (TextUtils.isEmpty(userId)) {
            updateEmptySeatView();
        } else {
            updateName(mSeatInfo.name.get());
            updateAvatar(mSeatInfo.avatarUrl.get());
            updateMuteState(mUserState.hasAudioStreamUserList.get());
            updateUserVolumeState(mUserState.speakingUserList.get());
        }
    }

    private void updateName(String name) {
        mTextName.setText(name);
    }

    private void updateAvatar(String avatar) {
        if (TextUtils.isEmpty(mSeatInfo.userId.get())) {
            return;
        }
        if (TextUtils.isEmpty(avatar)) {
            mImgHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImgHead, avatar, R.drawable.livekit_ic_avatar);
        }
    }

    private void updateLockState(boolean isLocked) {
        if (isLocked) {
            mImgHead.setImageResource(R.drawable.livekit_voiceroom_ic_lock);
            mTextName.setText("");
            mIvMute.setVisibility(View.GONE);
            mIvTalkBorder.setVisibility(View.GONE);
        } else {
            updateUserId(mSeatInfo.userId.get());
        }
    }

    private void updateAudioLockState(boolean isAudioLocked) {
        if (isAudioLocked) {
            mIvMute.setVisibility(View.VISIBLE);
            mIvTalkBorder.setVisibility(View.GONE);
        }
    }

    @Override
    protected void addObserver() {
        mSeatInfo.userId.observe(userIdObserver);
        mSeatInfo.name.observe(nameObserver);
        mSeatInfo.avatarUrl.observe(avatarObserver);
        mSeatInfo.isLocked.observe(isLockedObserver);
        mSeatInfo.isAudioLocked.observe(isAudioLockedObserver);
        mUserState.hasAudioStreamUserList.observe(hasAudioStreamUserListObserver);
        mUserState.speakingUserList.observe(hasAudioVolumeUserListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatInfo.userId.removeObserver(userIdObserver);
        mSeatInfo.name.removeObserver(nameObserver);
        mSeatInfo.avatarUrl.removeObserver(avatarObserver);
        mSeatInfo.isLocked.removeObserver(isLockedObserver);
        mSeatInfo.isAudioLocked.removeObserver(isAudioLockedObserver);
        mUserState.hasAudioStreamUserList.removeObserver(hasAudioStreamUserListObserver);
        mUserState.speakingUserList.removeObserver(hasAudioVolumeUserListObserver);
    }
}
