package com.trtc.uikit.livekit.view.liveroom.view.audience.component.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;

import androidx.gridlayout.widget.GridLayout;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LinkMicGridHelper;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoView;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoViewFactory;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class AudienceVideoView extends BasicView {

    private       GridLayout                               mLayoutVideoList;
    private       LinkMicGridHelper                        mLinkMicGridHelper;
    private       SeatState.SeatInfo                       mSelfSeatInfo;
    private final CopyOnWriteArrayList<SeatState.SeatInfo> mLinkUserList;
    private final Observer<List<SeatState.SeatInfo>>       mLinkAudienceListObserver = this::onLinkAudienceListChange;
    private final Observer<LiveDefine.LinkStatus>          mLinkStatusObserver       = this::onLinkStatusChange;

    public AudienceVideoView(Context context, LiveController liveController) {
        super(context.getApplicationContext(), liveController);
        mLinkUserList = new CopyOnWriteArrayList<>(mSeatState.seatList.get());
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_audience_video_view, this, true);
        bindViewId();

        initVideoViewList();
    }

    protected void addObserver() {
        mViewState.linkStatus.observe(mLinkStatusObserver);
        mSeatState.seatList.observe(mLinkAudienceListObserver);
    }

    protected void removeObserver() {
        mViewState.linkStatus.removeObserver(mLinkStatusObserver);
        mSeatState.seatList.removeObserver(mLinkAudienceListObserver);
    }

    private void initVideoViewList() {
        mLinkMicGridHelper = new LinkMicGridHelper(mLayoutVideoList);

        if (!mLinkUserList.isEmpty()) {
            for (SeatState.SeatInfo seatInfo : mLinkUserList) {
                addView(seatInfo);
            }
        }
    }

    private void bindViewId() {
        mLayoutVideoList = findViewById(R.id.gl_video_layout);
    }

    private void addPlaceHolderView(SeatState.SeatInfo seatInfo) {
        VideoView videoView = VideoViewFactory.instance.createPlaceHolderVideoView(seatInfo, mLiveController, mContext);
        mLinkMicGridHelper.addAudienceView(videoView);
    }

    private void removePlaceHolderView(SeatState.SeatInfo seatInfo) {
        VideoView videoView = VideoViewFactory.instance.createPlaceHolderVideoView(seatInfo, mLiveController, mContext);
        mLinkMicGridHelper.removeAudienceView(videoView);
        VideoViewFactory.instance.destroyPlaceHolderVideoView();
    }

    private void addView(SeatState.SeatInfo seatInfo) {
        boolean isOwner = mRoomState.ownerInfo.userId.equals(seatInfo.userId.get());
        VideoView videoView = VideoViewFactory.instance.createVideoView(seatInfo, mLiveController, mContext);
        if (isOwner) {
            mLinkMicGridHelper.addAnchorView(videoView);
        } else {
            mLinkMicGridHelper.addAudienceView(videoView);
        }
    }

    private void removeView(SeatState.SeatInfo seatInfo) {
        VideoView linkAudienceVideoView = VideoViewFactory.instance.createVideoView(seatInfo, mLiveController,
                mContext);
        mLinkMicGridHelper.removeAudienceView(linkAudienceVideoView);
        VideoViewFactory.instance.mVideoViewMap.remove(seatInfo.userId.get());
    }

    private void onLinkAudienceListChange(List<SeatState.SeatInfo> audienceList) {
        for (SeatState.SeatInfo userInfo : audienceList) {
            if (!mLinkUserList.contains(userInfo)) {
                mLinkUserList.add(userInfo);
                addView(userInfo);
            }
        }

        for (SeatState.SeatInfo userInfo : mLinkUserList) {
            if (!audienceList.contains(userInfo)) {
                mLinkUserList.remove(userInfo);
                removeView(userInfo);
            }
        }
    }

    private void onLinkStatusChange(LiveDefine.LinkStatus linkStatus) {
        if (mUserState.selfInfo.role.get() == TUIRoomDefine.Role.GENERAL_USER) {
            if (linkStatus == LiveDefine.LinkStatus.APPLYING) {
                mSelfSeatInfo = new SeatState.SeatInfo();
                mSelfSeatInfo.userId.set(mUserState.selfInfo.userId);
                mSelfSeatInfo.name.set(mUserState.selfInfo.name.get());
                mSelfSeatInfo.avatarUrl.set(mUserState.selfInfo.avatarUrl.get());
                addPlaceHolderView(mSelfSeatInfo);
            } else if (mSelfSeatInfo != null) {
                removePlaceHolderView(mSelfSeatInfo);
            }
        }
    }

}
