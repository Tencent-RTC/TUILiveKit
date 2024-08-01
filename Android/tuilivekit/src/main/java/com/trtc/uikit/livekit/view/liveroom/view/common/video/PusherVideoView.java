package com.trtc.uikit.livekit.view.liveroom.view.common.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.operation.SeatState;

@SuppressLint("ViewConstructor")
public class PusherVideoView extends VideoView {

    private       WaitLinkMicAnimationView        mWaitLinkMicAnimationView;
    private final Observer<LiveDefine.LinkStatus> mLinkStatusObserver = this::onLinkStatusChange;
    private       TUIVideoView                    mVideoView;

    public PusherVideoView(@NonNull Context context, LiveController liveController, SeatState.SeatInfo seatInfo) {
        super(context, liveController, seatInfo);
    }

    @Override
    protected void initView() {
        super.initView();

        initTUIVideoView();
        initImageEnableAudio();
        initTextName();
    }

    @Override
    protected void addObserver() {
        super.addObserver();
        mLiveController.getViewState().linkStatus.observe(mLinkStatusObserver);
    }

    @Override
    protected void removeObserver() {
        super.removeObserver();
        mLiveController.getViewState().linkStatus.removeObserver(mLinkStatusObserver);
    }

    private void initTUIVideoView() {
        mVideoView = getTUIVideoView();
        mLiveController.getMediaController().setLocalVideoView(mVideoView);
    }

    protected void initImageEnableAudio() {
        mImageEnableAudio.setVisibility(GONE);
    }

    private void initTextName() {
        mTextName.setVisibility(GONE);
    }

    private void removeWaitLinkMicView() {
        int childIndex = indexOfChild(mWaitLinkMicAnimationView);
        if (childIndex != -1) {
            removeViewAt(childIndex);
            mWaitLinkMicAnimationView = null;
        }
    }

    private void addWaitLinkMicView() {
        if (mWaitLinkMicAnimationView != null) {
            removeWaitLinkMicView();
        }
        mWaitLinkMicAnimationView = new WaitLinkMicAnimationView(mContext);
        LayoutParams layoutParams = new LayoutParams(ScreenUtil.dip2px(82),
                ScreenUtil.dip2px(82));
        layoutParams.gravity = Gravity.CENTER;
        addView(mWaitLinkMicAnimationView, layoutParams);

        mImageAvatar.setVisibility(GONE);
    }

    private void onLinkStatusChange(LiveDefine.LinkStatus linkStatus) {
        if (mLiveController.getUserState().selfInfo.role.get() != TUIRoomDefine.Role.ROOM_OWNER) {
            if (linkStatus == LiveDefine.LinkStatus.APPLYING) {
                addWaitLinkMicView();
                mVideoView.setVisibility(View.GONE);
            } else {
                removeWaitLinkMicView();
            }
            if (linkStatus == LiveDefine.LinkStatus.LINKING) {
                initImageAvatar();
                mVideoView.setVisibility(View.VISIBLE);
            }
        }
    }
}
