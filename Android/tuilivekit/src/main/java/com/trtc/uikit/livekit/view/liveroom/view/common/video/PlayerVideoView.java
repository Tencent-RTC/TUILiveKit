package com.trtc.uikit.livekit.view.liveroom.view.common.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.MediaController;
import com.trtc.uikit.livekit.state.operation.SeatState;

import java.util.List;

@SuppressLint("ViewConstructor")
public class PlayerVideoView extends VideoView {

    private final Observer<String>                   userNickNameChangedListener = this::onNicknameChange;
    private final Observer<List<SeatState.SeatInfo>> mLinkAudienceListObserver   = this::onLinkAudienceListChange;

    public PlayerVideoView(@NonNull Context context, LiveController liveController, SeatState.SeatInfo seatInfo) {
        super(context, liveController, seatInfo);
    }

    @Override
    protected void initView() {
        super.initView();

        initTUIVideoView();
        initNicknameView();
    }

    @Override
    protected void addObserver() {
        super.addObserver();
        mSeatInfo.name.observe(userNickNameChangedListener);
        mSeatState.seatList.observe(mLinkAudienceListObserver);
    }

    @Override
    protected void removeObserver() {
        super.removeObserver();
        mSeatInfo.name.removeObserver(userNickNameChangedListener);
        mSeatState.seatList.removeObserver(mLinkAudienceListObserver);
    }

    private void initTUIVideoView() {
        MediaController mediaController = mLiveController.getMediaController();
        mediaController.setRemoteVideoView(mSeatInfo.userId.get(), TUIRoomDefine.VideoStreamType.CAMERA_STREAM,
                getTUIVideoView());
        mediaController.startPlayRemoteVideo(mSeatInfo.userId.get(), TUIRoomDefine.VideoStreamType.CAMERA_STREAM, null);
    }

    private void initNicknameView() {
        if (mSeatState.seatList.get().size() > 1) {
            mTextName.setVisibility(VISIBLE);
        } else {
            mTextName.setVisibility(GONE);
        }
        if (TextUtils.isEmpty(mSeatInfo.name.get())) {
            mTextName.setText(mSeatInfo.userId.get());
        } else {
            mTextName.setText(mSeatInfo.name.get());
        }
    }

    private void onNicknameChange(String name) {
        initNicknameView();
    }

    private void onLinkAudienceListChange(List<SeatState.SeatInfo> seatInfoList) {
        initNicknameView();
    }
}
