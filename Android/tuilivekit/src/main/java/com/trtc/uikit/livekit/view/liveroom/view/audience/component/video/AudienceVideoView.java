package com.trtc.uikit.livekit.view.liveroom.view.audience.component.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;

import androidx.gridlayout.widget.GridLayout;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LinkMicGridHelper;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.RenderVideoViewModel;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoView;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class AudienceVideoView extends BasicView {
    private static final String TAG = "AudienceVideoView";

    private       GridLayout                                           mLayoutVideoList;
    private       LinkMicGridHelper                                    mLinkMicGridHelper;
    private       SeatState.SeatInfo                                   mSelfSeatInfo;
    private final CopyOnWriteArrayList<SeatState.SeatInfo>             mLinkUserList;
    private final CopyOnWriteArrayList<ConnectionState.ConnectionUser> mConnectedUserList;
    private final Observer<List<SeatState.SeatInfo>>                   mLinkAudienceListObserver
            = this::onLinkAudienceListChange;
    private final Observer<List<ConnectionState.ConnectionUser>>       mConnectedListObserver
            = this::onConnectedListChange;
    private final Observer<LiveDefine.LinkStatus>                      mLinkStatusObserver
            = this::onLinkStatusChange;

    public AudienceVideoView(Context context, LiveController liveController) {
        super(context.getApplicationContext(), liveController);
        mLinkUserList = new CopyOnWriteArrayList<>(mSeatState.seatList.get());
        mConnectedUserList = new CopyOnWriteArrayList<>(mConnectionState.connectedUsers.get());
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_audience_video_view, this, true);
        bindViewId();

        initVideoViewList();
    }

    @Override
    protected void addObserver() {
        mViewState.linkStatus.observe(mLinkStatusObserver);
        mSeatState.seatList.observe(mLinkAudienceListObserver);
        mConnectionState.connectedUsers.observe(mConnectedListObserver);
    }

    @Override
    protected void removeObserver() {
        mViewState.linkStatus.removeObserver(mLinkStatusObserver);
        mSeatState.seatList.removeObserver(mLinkAudienceListObserver);
        mConnectionState.connectedUsers.removeObserver(mConnectedListObserver);
    }

    private void initVideoViewList() {
        mLinkMicGridHelper = new LinkMicGridHelper(mLayoutVideoList);
        for (SeatState.SeatInfo seatInfo : mLinkUserList) {
            addView(new RenderVideoViewModel(seatInfo, mRoomState.roomId));
        }
        for (ConnectionState.ConnectionUser user : mConnectedUserList) {
            if (mRoomState.ownerInfo.userId.equals(user.userId)) {
                addView(new RenderVideoViewModel(user));
            }
        }
        for (ConnectionState.ConnectionUser user : mConnectedUserList) {
            if (!mRoomState.ownerInfo.userId.equals(user.userId)) {
                addView(new RenderVideoViewModel(user));
            }
        }

    }

    private void bindViewId() {
        mLayoutVideoList = findViewById(R.id.gl_video_layout);
    }

    private void addPlaceHolderView(SeatState.SeatInfo seatInfo) {
        VideoView videoView = mLiveController.getVideoViewFactory().createPlaceHolderVideoView(seatInfo,
                mLiveController, mContext);
        mLinkMicGridHelper.addAudienceView(videoView);
    }

    private void removePlaceHolderView(SeatState.SeatInfo seatInfo) {
        VideoView videoView = mLiveController.getVideoViewFactory().createPlaceHolderVideoView(seatInfo,
                mLiveController, mContext);
        mLinkMicGridHelper.removeAudienceView(videoView);
        mLiveController.getVideoViewFactory().destroyPlaceHolderVideoView();
    }

    private void addView(RenderVideoViewModel renderVideoViewModel) {
        boolean isOwner = mRoomState.ownerInfo.userId.equals(renderVideoViewModel.userId);
        VideoView videoView = mLiveController.getVideoViewFactory().createVideoView(
                renderVideoViewModel, mLiveController, mContext);
        if (videoView == null) {
            Log.e(TAG, "addView fail, renderVideoViewModel.userId = " + renderVideoViewModel.userId);
            return;
        }
        if (isOwner) {
            mLinkMicGridHelper.addAnchorView(videoView);
        } else {
            mLinkMicGridHelper.addAudienceView(videoView);
        }
    }

    private void removeView(SeatState.SeatInfo seatInfo) {
        VideoView videoView = mLiveController.getVideoViewFactory().createVideoView(new RenderVideoViewModel(seatInfo,
                mLiveController.getRoomState().roomId), mLiveController, mContext);
        mLiveController.getVideoViewFactory().removeVideoViewByUserId(seatInfo.userId.get());
        if (videoView == null) {
            Log.e(TAG, "removeView fail, seatInfo.userId = " + seatInfo.userId.get());
            return;
        }
        mLinkMicGridHelper.removeAudienceView(videoView);
    }

    private void removeView(ConnectionState.ConnectionUser connectionUser) {
        VideoView videoView = mLiveController.getVideoViewFactory().createVideoView(
                new RenderVideoViewModel(connectionUser), mLiveController, mContext);
        mLiveController.getVideoViewFactory().removeVideoViewByUserId(connectionUser.userId);
        if (videoView == null) {
            Log.e(TAG, "removeView fail, seatInfo.userId = " + connectionUser.userId);
            return;
        }
        mLinkMicGridHelper.removeAudienceView(videoView);
    }

    private void addUserView(ConnectionState.ConnectionUser user) {
        boolean isContainer = false;
        for (int i = 0; i < mConnectedUserList.size(); i++) {
            if (mConnectedUserList.get(i).userId.equals(user.userId)) {
                isContainer = true;
                break;
            }
        }
        if (!isContainer) {
            mConnectedUserList.add(user);
            addView(new RenderVideoViewModel(user));
        }
    }

    private void onConnectedListChange(List<ConnectionState.ConnectionUser> connectedUsers) {
        for (ConnectionState.ConnectionUser user : connectedUsers) {
            if (mRoomState.ownerInfo.userId.equals(user.userId)) {
                addUserView(user);
            }
        }
        for (ConnectionState.ConnectionUser user : connectedUsers) {
            addUserView(user);
        }
        for (ConnectionState.ConnectionUser user : mConnectedUserList) {
            if (!connectedUsers.contains(user)) {
                mConnectedUserList.remove(user);
                removeView(user);
            }
        }
    }

    private void onLinkAudienceListChange(List<SeatState.SeatInfo> audienceList) {
        for (SeatState.SeatInfo seatInfo : audienceList) {
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
                addView(new RenderVideoViewModel(seatInfo, mRoomState.roomId));
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
