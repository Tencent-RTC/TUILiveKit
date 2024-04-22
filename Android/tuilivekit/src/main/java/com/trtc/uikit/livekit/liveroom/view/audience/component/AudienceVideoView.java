package com.trtc.uikit.livekit.liveroom.view.audience.component;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;

import androidx.gridlayout.widget.GridLayout;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LinkMicGridHelper;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;
import com.trtc.uikit.livekit.liveroom.view.common.video.VideoView;
import com.trtc.uikit.livekit.liveroom.view.common.video.VideoViewFactory;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;

@SuppressLint("ViewConstructor")
public class AudienceVideoView extends FrameLayout {

    private final Context                                       mContext;
    private final LiveRoomInfo                                  mLiveRoomInfo;
    private       LinkMicGridHelper                             mLinkMicGridHelper;
    private       CopyOnWriteArrayList<UserInfo>                mLinkUserList;
    private       LiveKitStore                                  mLiveKitStore;
    private final Observer<CopyOnWriteArraySet<UserInfo>>       mLinkAudienceListObserver = (audienceList) -> {
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
    private final Observer<TUILiveDefine.UserInteractionStatus> mUserStatusObserver       = (userStatus) -> {
        if (mLiveKitStore.selfInfo.role.get() == TUILiveDefine.RoleType.AUDIENCE) {
            if (userStatus == TUILiveDefine.UserInteractionStatus.APPLYING) {
                mLinkUserList.add(mLiveKitStore.selfInfo);
                addView(mLiveKitStore.selfInfo);
            } else if (userStatus == TUILiveDefine.UserInteractionStatus.NONE) {
                mLinkUserList.remove(mLiveKitStore.selfInfo);
                removeView(mLiveKitStore.selfInfo);
            }
        }
    };

    public AudienceVideoView(Context context, LiveRoomInfo roomInfo) {
        super(context.getApplicationContext());
        mContext = context.getApplicationContext();
        mLiveRoomInfo = roomInfo;
        mLiveKitStore = LiveKitStore.sharedInstance();
        mLinkUserList = new CopyOnWriteArrayList<>(mLiveRoomInfo.linkingAudienceList.get());
        init();
    }

    private void init() {
        View rootView = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_audience_video_view, this, true);
        GridLayout gridLayout = rootView.findViewById(R.id.gl_video_layout);
        mLinkMicGridHelper = new LinkMicGridHelper(gridLayout);

        VideoView anchorVideoView = VideoViewFactory.instance.createVideoView(mLiveRoomInfo.anchorInfo, mLiveRoomInfo
                , mContext);
        mLinkMicGridHelper.addAnchorView(anchorVideoView);

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
        mLiveKitStore.selfInfo.status.observe(mUserStatusObserver);
        mLiveRoomInfo.linkingAudienceList.observe(mLinkAudienceListObserver);
    }

    private void removeObserver() {
        mLiveKitStore.selfInfo.status.removeObserver(mUserStatusObserver);
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
