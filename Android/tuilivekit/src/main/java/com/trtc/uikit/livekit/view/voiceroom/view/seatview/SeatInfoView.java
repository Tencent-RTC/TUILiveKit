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
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.RippleView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.voiceroom.model.ListMenuInfo;
import com.trtc.uikit.livekit.view.voiceroom.model.SeatActionSheetGenerator;
import com.trtc.uikit.livekit.view.voiceroom.view.panel.seatactionsheet.SeatActionSheetPanel;

import java.util.LinkedHashSet;
import java.util.List;

@SuppressLint("ViewConstructor")
public class SeatInfoView extends BasicView {
    private final SeatState.SeatInfo  mSeatInfo;
    private final SeatListView.Config mConfig;

    private ImageFilterView mImgHead;
    private View            mEmptyViewContainer;
    private ImageView       mIvEmptyView;
    private TextView        mTextName;
    private ImageView       mIvMute;
    private ImageView       mIvRoomOwner;
    private RippleView      mIvTalkBorder;

    private SeatActionSheetPanel mSeatActionSheetPanel;

    private final SeatActionSheetGenerator mSeatActionSheetGenerator;

    private final Observer<LinkedHashSet<String>> hasAudioStreamUserListObserver = this::updateMuteState;

    private final Observer<LinkedHashSet<String>> hasAudioVolumeUserListObserver = this::updateUserVolumeState;

    private final Observer<String> userIdObserver = this::updateUserId;

    private final Observer<String> nameObserver = this::updateName;

    private final Observer<String> avatarObserver = this::updateAvatar;

    private final Observer<Boolean> isLockedObserver = this::updateLockState;

    private final Observer<Boolean> isAudioLockedObserver = this::updateAudioLockState;

    public SeatInfoView(@NonNull Context context, LiveController liveController, SeatState.SeatInfo seatInfo,
                        SeatListView.Config config) {
        super(context, liveController);
        mSeatInfo = seatInfo;
        mSeatActionSheetGenerator = new SeatActionSheetGenerator(context, liveController);
        mConfig = config;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_seat_info_view, this, true);
        mTextName = findViewById(R.id.tv_name);
        mImgHead = findViewById(R.id.iv_head);
        mEmptyViewContainer = findViewById(R.id.empty_seat_container);
        mIvEmptyView = findViewById(R.id.iv_empty_seat);
        mIvMute = findViewById(R.id.iv_mute);
        mIvTalkBorder = findViewById(R.id.iv_talk_border);
        mIvRoomOwner = findViewById(R.id.iv_room_owner);
        updateLockState(mSeatInfo.isLocked.get());
        findViewById(R.id.seat_container).setOnClickListener(view -> {
            if (mConfig.isPreview) {
                return;
            }
            List<ListMenuInfo> listMenuInfoList = mSeatActionSheetGenerator.generate(mSeatInfo);
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
        mEmptyViewContainer.setVisibility(View.VISIBLE);
        mIvEmptyView.setImageResource(mSeatInfo.isLocked.get()
                ? R.drawable.livekit_voiceroom_ic_lock : R.drawable.livekit_voiceroom_empty_seat);
        mImgHead.setVisibility(View.GONE);
        mIvMute.setVisibility(View.GONE);
        mIvTalkBorder.setVisibility(View.GONE);
        if (mConfig.isPreview) {
            mTextName.setVisibility(View.GONE);
        } else {
            mTextName.setVisibility(View.VISIBLE);
            mTextName.setText(String.valueOf(mSeatInfo.index + 1));
        }
    }

    private void updateUserId(String userId) {
        if (TextUtils.isEmpty(userId)) {
            updateEmptySeatView();
        } else {
            mEmptyViewContainer.setVisibility(View.GONE);
            updateName(mSeatInfo.name.get());
            updateAvatar(mSeatInfo.avatarUrl.get());
            updateMuteState(mUserState.hasAudioStreamUserList.get());
            updateUserVolumeState(mUserState.speakingUserList.get());
            updateUserRole(userId);
        }
    }

    private void updateUserRole(String userId) {
        boolean isOwner = mRoomState.ownerInfo.userId.equals(userId);
        mIvRoomOwner.setVisibility(isOwner ? View.VISIBLE : View.GONE);
    }

    private void updateName(String name) {
        mTextName.setText(name);
    }

    private void updateAvatar(String avatar) {
        if (TextUtils.isEmpty(mSeatInfo.userId.get())) {
            return;
        }
        mEmptyViewContainer.setVisibility(View.GONE);
        mImgHead.setVisibility(View.VISIBLE);
        if (TextUtils.isEmpty(avatar)) {
            mImgHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImgHead, avatar, R.drawable.livekit_ic_avatar);
        }
    }

    private void updateLockState(boolean isLocked) {
        if (isLocked) {
            updateEmptySeatView();
        } else {
            updateUserId(mSeatInfo.userId.get());
        }
    }

    private void updateAudioLockState(boolean isAudioLocked) {
        if (isAudioLocked && !TextUtils.isEmpty(mSeatInfo.userId.get())) {
            mIvMute.setVisibility(View.VISIBLE);
            mIvTalkBorder.setVisibility(View.GONE);
        }
    }
}
