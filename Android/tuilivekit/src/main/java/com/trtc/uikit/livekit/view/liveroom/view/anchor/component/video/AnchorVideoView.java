package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;

import androidx.gridlayout.widget.GridLayout;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LinkMicGridHelper;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoView;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class AnchorVideoView extends BasicView {

    private       GridLayout                               mLayoutVideoList;
    private       LinkMicGridHelper                        mLinkMicGridHelper;
    private final CopyOnWriteArrayList<SeatState.SeatInfo> mLinkUserList;
    private final Observer<List<SeatState.SeatInfo>>       mLinkAudienceListObserver = this::onLinkAudienceListChange;

    public AnchorVideoView(Context context, LiveController liveController) {
        super(context, liveController);
        mLinkUserList = new CopyOnWriteArrayList<>(mSeatState.seatList.get());
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_video_view, this, true);
        bindViewId();

        initVideoViewList();
    }

    @Override
    protected void addObserver() {
        mSeatState.seatList.observe(mLinkAudienceListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatList.removeObserver(mLinkAudienceListObserver);
    }

    private void bindViewId() {
        mLayoutVideoList = findViewById(R.id.gl_video_layout);
    }

    private void initVideoViewList() {
        mLinkMicGridHelper = new LinkMicGridHelper(mLayoutVideoList);
        mLiveController.getMediaController().openLocalCamera();
        addAnchorView();
        for (SeatState.SeatInfo seatInfo : mLinkUserList) {
            addAudienceView(seatInfo);
        }
    }

    private void addAnchorView() {
        SeatState.SeatInfo seatInfo = new SeatState.SeatInfo();
        seatInfo.userId.set(mUserState.selfInfo.userId);
        seatInfo.avatarUrl.set(mUserState.selfInfo.avatarUrl.get());
        seatInfo.name.set(mUserState.selfInfo.name.get());
        VideoView linkAudienceVideoView = mLiveController.getVideoViewFactory().createVideoView(seatInfo,
                mLiveController, mContext);
        mLinkMicGridHelper.addAnchorView(linkAudienceVideoView);
    }

    private void addAudienceView(SeatState.SeatInfo seatInfo) {
        if (mUserState.selfInfo.userId.equals(seatInfo.userId.get())) {
            return;
        }
        VideoView linkAudienceVideoView = mLiveController.getVideoViewFactory().createVideoView(seatInfo,
                mLiveController,
                mContext);
        mLinkMicGridHelper.addAudienceView(linkAudienceVideoView);
    }

    private void removeView(SeatState.SeatInfo seatInfo) {
        VideoView linkAudienceVideoView = mLiveController.getVideoViewFactory().createVideoView(seatInfo,
                mLiveController,
                mContext);
        mLinkMicGridHelper.removeAudienceView(linkAudienceVideoView);
        mLiveController.getVideoViewFactory().removeVideoViewByUserId(seatInfo.userId.get());
    }

    private void onLinkAudienceListChange(List<SeatState.SeatInfo> seatInfoList) {
        for (SeatState.SeatInfo seatInfo : seatInfoList) {
            boolean isContainer = false;
            for (int i = 0; i < mLinkUserList.size(); i++) {
                if (mLinkUserList.get(i).userId.get().equals(seatInfo.userId.get())) {
                    isContainer = true;
                    mLinkUserList.get(i).updateState(seatInfo);
                    break;
                }
            }
            if (!isContainer) {
                mLinkUserList.add(seatInfo);
                addAudienceView(seatInfo);
            }
        }

        for (SeatState.SeatInfo userInfo : mLinkUserList) {
            if (!seatInfoList.contains(userInfo)) {
                mLinkUserList.remove(userInfo);
                removeView(userInfo);
            }
        }
    }
}
