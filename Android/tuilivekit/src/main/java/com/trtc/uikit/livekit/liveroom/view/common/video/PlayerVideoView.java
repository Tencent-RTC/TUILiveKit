package com.trtc.uikit.livekit.liveroom.view.common.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.concurrent.CopyOnWriteArraySet;

@SuppressLint("ViewConstructor")
public class PlayerVideoView extends VideoView {

    public Observer<Boolean> enableHiddenNicknameObserver = (enable) -> {
        if (enable) {
            mTextName.setVisibility(GONE);
        } else {
            mTextName.setVisibility(VISIBLE);
        }
    };
    public Observer<String>  userNickNameChangedListener  = (name) -> refreshView();

    public Observer<CopyOnWriteArraySet<UserInfo>> mLinkingAudienceListObserver = (userList) -> refreshView();

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

    private void addObserver() {
        mUserInfo.name.observe(userNickNameChangedListener);
        mLiveRoomInfo.roomConfig.enableHiddenNickname.observe(enableHiddenNicknameObserver);
        mLiveRoomInfo.linkingAudienceList.observe(mLinkingAudienceListObserver);
    }


    private void removeObserver() {
        mUserInfo.name.remove(userNickNameChangedListener);
        mLiveRoomInfo.roomConfig.enableHiddenNickname.removeObserver(enableHiddenNicknameObserver);
        mLiveRoomInfo.linkingAudienceList.removeObserver(mLinkingAudienceListObserver);
    }

    public PlayerVideoView(@NonNull Context context, LiveRoomInfo info, UserInfo userInfo, RoomEngineService service) {
        super(context, info, userInfo, service);

        mRoomEngineService.setRemoteVideoView(mUserInfo.userId,
                TUIRoomDefine.VideoStreamType.CAMERA_STREAM, getTUIVideoView());
        mRoomEngineService.startPlayRemoteVideo(mUserInfo.userId,
                TUIRoomDefine.VideoStreamType.CAMERA_STREAM, null);

        if (mUserInfo.audioInfo.muteAudio.get()) {
            mImageEnableAudio.setVisibility(VISIBLE);
        } else {
            mImageEnableAudio.setVisibility(GONE);
        }

        if (mLiveRoomInfo.roomConfig.enableHiddenNickname.get()) {
            mTextName.setVisibility(GONE);
        } else {
            mTextName.setVisibility(VISIBLE);
        }
        refreshView();
    }

    private void refreshView() {
        mTextName.setVisibility(VISIBLE);
        if (TextUtils.isEmpty(mUserInfo.name.get())) {
            mTextName.setText(mUserInfo.userId);
        } else {
            mTextName.setText(mUserInfo.name.get());
        }
    }
}
