package com.trtc.uikit.livekit.view.liveroom.view.common.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.MediaController;

@SuppressLint("ViewConstructor")
public class PlayerVideoView extends VideoView {

    public PlayerVideoView(@NonNull Context context, LiveController liveController, RenderVideoViewModel viewModel) {
        super(context, liveController, viewModel);
    }

    @Override
    protected void initView() {
        super.initView();

        initTUIVideoView();
        initImageEnableAudio();
        initNicknameView();
    }

    @Override
    protected void addObserver() {
        super.addObserver();
    }

    @Override
    protected void removeObserver() {
        super.removeObserver();
    }

    private void initTUIVideoView() {
        MediaController mediaController = mLiveController.getMediaController();
        mediaController.setRemoteVideoView(mViewModel.userId, TUIRoomDefine.VideoStreamType.CAMERA_STREAM,
                getTUIVideoView());
        mediaController.startPlayRemoteVideo(mViewModel.userId, TUIRoomDefine.VideoStreamType.CAMERA_STREAM, null);
    }

    protected void initImageEnableAudio() {
        mImageEnableAudio.setVisibility(GONE);
    }

    private void initNicknameView() {
        if (!mSeatState.seatList.get().isEmpty() || !mConnectionState.connectedUsers.get().isEmpty()) {
            mTextName.setVisibility(VISIBLE);
        } else {
            mTextName.setVisibility(GONE);
        }
        if (TextUtils.isEmpty(mViewModel.name)) {
            mTextName.setText(mViewModel.userId);
        } else {
            mTextName.setText(mViewModel.name);
        }
    }
}
