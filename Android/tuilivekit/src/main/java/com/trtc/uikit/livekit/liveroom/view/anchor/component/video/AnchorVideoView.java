package com.trtc.uikit.livekit.liveroom.view.anchor.component.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;

import androidx.gridlayout.widget.GridLayout;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LinkMicGridHelper;
import com.trtc.uikit.livekit.common.utils.PermissionRequest;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;
import com.trtc.uikit.livekit.liveroom.view.common.video.VideoView;
import com.trtc.uikit.livekit.liveroom.view.common.video.VideoViewFactory;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;

@SuppressLint("ViewConstructor")
public class AnchorVideoView extends FrameLayout {

    private final Context                                 mContext;
    private final LiveRoomInfo                            mLiveRoomInfo;
    private final RoomEngineService                       mRoomEngineService;
    private       LinkMicGridHelper                       mLinkMicGridHelper;
    private       CopyOnWriteArrayList<UserInfo>          mLinkUserList;
    private final Observer<CopyOnWriteArraySet<UserInfo>> mLinkAudienceListObserver = (audienceList) -> {
        for (UserInfo userInfo : audienceList) {
            if (!mLinkUserList.contains(userInfo)) {
                mLinkUserList.add(userInfo);
                addView(userInfo);
            }
        }

        for (UserInfo userInfo : mLinkUserList) {
            if (!audienceList.contains(userInfo)) {
                mLinkUserList.remove(userInfo);
                removeView(userInfo);
            }
        }
    };

    public AnchorVideoView(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context.getApplicationContext());
        mContext = context.getApplicationContext();
        mLiveRoomInfo = roomInfo;
        mLinkUserList = new CopyOnWriteArrayList<>(mLiveRoomInfo.linkingAudienceList.get());
        mRoomEngineService = service;
        init();
    }

    private void init() {
        View rootView = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_anchor_video_view, this, true);
        GridLayout gridLayout = rootView.findViewById(R.id.gl_video_layout);
        mLinkMicGridHelper = new LinkMicGridHelper(gridLayout);

        mLiveRoomInfo.anchorInfo.videoInfo.isCameraOpened.set(true);
        VideoView selfVideoView = VideoViewFactory.instance.createVideoView(mLiveRoomInfo.anchorInfo, mLiveRoomInfo,
                mContext);
        mLinkMicGridHelper.addAnchorView(selfVideoView);
        PermissionRequest.requestPermissions(mContext.getApplicationContext(), new PermissionCallback() {
            @Override
            public void onGranted() {
                mRoomEngineService.openLocalCamera(mLiveRoomInfo.anchorInfo.videoInfo.isFrontCamera.get(),
                        mLiveRoomInfo.anchorInfo.videoInfo.videoQuality.get(),
                        new TUIRoomDefine.ActionCallback() {
                            @Override
                            public void onSuccess() {
                                mRoomEngineService.initLivingConfig();
                            }

                            @Override
                            public void onError(TUICommonDefine.Error error, String message) {

                            }
                        });
            }
        });

        if (!mLinkUserList.isEmpty()) {
            for (UserInfo userInfo : mLinkUserList) {
                VideoView linkAudienceVideoView = VideoViewFactory.instance.createVideoView(userInfo, mLiveRoomInfo,
                        mContext);
                mLinkMicGridHelper.addAudienceView(linkAudienceVideoView);
            }
        }
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
        removeAllViews();
    }


    private void addObserver() {
        mLiveRoomInfo.linkingAudienceList.observe(mLinkAudienceListObserver);
    }

    private void removeObserver() {
        mLiveRoomInfo.linkingAudienceList.removeObserver(mLinkAudienceListObserver);
    }

    private void addView(UserInfo userInfo) {
        VideoView linkAudienceVideoView = VideoViewFactory.instance.createVideoView(userInfo, mLiveRoomInfo, mContext);
        mLinkMicGridHelper.addAudienceView(linkAudienceVideoView);
    }

    private void removeView(UserInfo userInfo) {
        VideoView linkAudienceVideoView = VideoViewFactory.instance.createVideoView(userInfo, mLiveRoomInfo, mContext);
        mLinkMicGridHelper.removeAudienceView(linkAudienceVideoView);
        VideoViewFactory.instance.mVideoViewMap.remove(userInfo.userId);
    }
}
