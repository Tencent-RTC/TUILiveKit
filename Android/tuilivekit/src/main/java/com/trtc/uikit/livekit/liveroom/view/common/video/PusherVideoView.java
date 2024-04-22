package com.trtc.uikit.livekit.liveroom.view.common.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.Gravity;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.RoleType;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.UserInteractionStatus;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

@SuppressLint("ViewConstructor")
public class PusherVideoView extends VideoView {

    private       WaitLinkMicAnimationView        mWaitLinkMicAnimationView;
    private final Observer<UserInteractionStatus> mUserStatusObserver = (userStatus) -> {
        if (LiveKitStore.sharedInstance().selfInfo.role.get() == RoleType.AUDIENCE) {
            if (userStatus == UserInteractionStatus.APPLYING) {
                addWaitLinkMicView();
            } else {
                removeWaitLinkMicView();
            }
        }
    };


    public PusherVideoView(@NonNull Context context, LiveRoomInfo roomInfo, UserInfo userInfo,
                           RoomEngineService service) {
        super(context, roomInfo, userInfo, service);

        mRoomEngineService.setLocalVideoView(getTUIVideoView());

        mImageEnableAudio.setVisibility(GONE);
        mTextName.setVisibility(GONE);
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

    private void addObserver() {
        LiveKitStore.sharedInstance().selfInfo.status.observe(mUserStatusObserver);
    }

    private void removeObserver() {
        LiveKitStore.sharedInstance().selfInfo.status.removeObserver(mUserStatusObserver);
    }

    private void removeWaitLinkMicView() {
        int childIndex = indexOfChild(mWaitLinkMicAnimationView);
        if (childIndex != -1) {
            removeViewAt(childIndex);
            mWaitLinkMicAnimationView = null;
            mImageAvatar.setVisibility(mUserInfo.videoInfo.isCameraOpened.get() ? GONE : VISIBLE);
            ImageLoader.load(mContext, mImageAvatar, mUserInfo.avatarUrl.get(), R.drawable.livekit_ic_avatar);
        }
    }

    private void addWaitLinkMicView() {
        mWaitLinkMicAnimationView = new WaitLinkMicAnimationView(mContext);
        FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ScreenUtil.dip2px(82),
                ScreenUtil.dip2px(82));
        layoutParams.gravity = Gravity.CENTER;
        addView(mWaitLinkMicAnimationView, layoutParams);

        mImageAvatar.setVisibility(GONE);
    }
}
