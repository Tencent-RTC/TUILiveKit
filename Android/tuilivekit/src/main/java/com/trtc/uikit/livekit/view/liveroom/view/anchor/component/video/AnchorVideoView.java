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
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.common.view.StandardDialog;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.RenderVideoViewModel;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoView;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class AnchorVideoView extends BasicView {

    private       GridLayout                                           mLayoutVideoList;
    private       LinkMicGridHelper                                    mLinkMicGridHelper;
    private       StandardDialog                                       dialog;
    private final CopyOnWriteArrayList<SeatState.SeatInfo>             mLinkUserList;
    private final CopyOnWriteArrayList<ConnectionState.ConnectionUser> mConnectedUserList;
    private final Observer<List<SeatState.SeatInfo>>                   mLinkAudienceListObserver
            = this::onLinkAudienceListChange;
    private final Observer<List<ConnectionState.ConnectionUser>>       mConnectionListObserver
            = this::onConnectedListChange;
    private final Observer<ConnectionState.ConnectionUser>             mReceivedRequestObserver
            = this::onReceivedRequestChange;

    public AnchorVideoView(Context context, LiveController liveController) {
        super(context, liveController);
        mLinkUserList = new CopyOnWriteArrayList<>(mSeatState.seatList.get());
        mConnectedUserList = new CopyOnWriteArrayList<>(mConnectionState.connectedUsers.get());
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
        mConnectionState.connectedUsers.observe(mConnectionListObserver);
        mConnectionState.receivedConnectionRequest.observe(mReceivedRequestObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatList.removeObserver(mLinkAudienceListObserver);
        mConnectionState.connectedUsers.removeObserver(mConnectionListObserver);
        mConnectionState.receivedConnectionRequest.removeObserver(mReceivedRequestObserver);
    }

    private void bindViewId() {
        mLayoutVideoList = findViewById(R.id.gl_video_layout);
    }

    private void initVideoViewList() {
        mLinkMicGridHelper = new LinkMicGridHelper(mLayoutVideoList);
        mLiveController.getMediaController().openLocalCamera();
        addAnchorView();
        if (mLinkUserList != null) {
            for (SeatState.SeatInfo seatInfo : mLinkUserList) {
                addAudienceView(seatInfo);
            }
        }
        if (mConnectedUserList != null) {
            for (ConnectionState.ConnectionUser connectionUser : mConnectedUserList) {
                addCoHostView(connectionUser);
            }
        }
    }

    private void addAnchorView() {
        RenderVideoViewModel videoUser = new RenderVideoViewModel();
        videoUser.userId = mUserState.selfInfo.userId;
        videoUser.avatarUrl = mUserState.selfInfo.avatarUrl.get();
        videoUser.name = mUserState.selfInfo.name.get();

        VideoView linkAudienceVideoView = mLiveController.getVideoViewFactory().createVideoView(videoUser,
                mLiveController, mContext);
        mLinkMicGridHelper.addAnchorView(linkAudienceVideoView);
    }

    private void addAudienceView(SeatState.SeatInfo seatInfo) {
        if (mUserState.selfInfo.userId.equals(seatInfo.userId.get())) {
            return;
        }
        VideoView linkAudienceVideoView =
                mLiveController.getVideoViewFactory().createVideoView(new RenderVideoViewModel(seatInfo,
                                mLiveController.getRoomState().roomId),
                        mLiveController,
                        mContext);
        mLinkMicGridHelper.addAudienceView(linkAudienceVideoView);
    }

    private void addCoHostView(ConnectionState.ConnectionUser connectionUser) {
        if (mUserState.selfInfo.userId.equals(connectionUser.userId)) {
            return;
        }
        VideoView connectedAudienceVideoView = mLiveController.getVideoViewFactory().createVideoView(
                new RenderVideoViewModel(connectionUser),
                mLiveController,
                mContext);
        mLinkMicGridHelper.addAudienceView(connectedAudienceVideoView);
    }

    private void removeView(SeatState.SeatInfo seatInfo) {
        VideoView linkAudienceVideoView = mLiveController.getVideoViewFactory().createVideoView(
                new RenderVideoViewModel(seatInfo, mLiveController.getRoomState().roomId), mLiveController, mContext);
        mLinkMicGridHelper.removeAudienceView(linkAudienceVideoView);
        mLiveController.getVideoViewFactory().removeVideoViewByUserId(seatInfo.userId.get());
    }

    private void removeView(ConnectionState.ConnectionUser connectionUser) {
        VideoView linkAudienceVideoView = mLiveController.getVideoViewFactory().createVideoView(
                new RenderVideoViewModel(connectionUser),
                mLiveController,
                mContext);
        mLinkMicGridHelper.removeAudienceView(linkAudienceVideoView);
        mLiveController.getVideoViewFactory().removeVideoViewByUserId(connectionUser.userId);
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

    private void onConnectedListChange(List<ConnectionState.ConnectionUser> connectionUsers) {
        for (ConnectionState.ConnectionUser connectionUser : connectionUsers) {
            boolean isContainer = false;
            for (int i = 0; i < mConnectedUserList.size(); i++) {
                if (mConnectedUserList.get(i).userId.equals(connectionUser.userId)) {
                    isContainer = true;
                    break;
                }
            }
            if (!isContainer) {
                mConnectedUserList.add(connectionUser);
                addCoHostView(connectionUser);
            }
        }

        for (ConnectionState.ConnectionUser userInfo : mConnectedUserList) {
            if (!connectionUsers.contains(userInfo)) {
                mConnectedUserList.remove(userInfo);
                removeView(userInfo);
            }
        }
    }

    private void onReceivedRequestChange(ConnectionState.ConnectionUser receivedConnectionRequest) {
        if (receivedConnectionRequest == null) {
            dialog.dismiss();
            return;
        }
        String content = receivedConnectionRequest.userName
                + getContext().getString(R.string.livekit_connect_inviting_append);
        showConnectionRequestDialog(content, receivedConnectionRequest.avatarUrl, receivedConnectionRequest.roomId);
    }

    private void showConnectionRequestDialog(String content, String avatarUrl, String roomId) {
        dialog = new StandardDialog(getContext());
        dialog.setContent(content);
        dialog.setAvatar(avatarUrl);

        String rejectText = getContext().getString(R.string.livekit_reject);
        dialog.setNegativeText(rejectText, negativeView -> {
            mConnectionController.reject(roomId);
            dialog.dismiss();
        });

        String receiveText = getContext().getString(R.string.livekit_receive);
        dialog.setPositiveText(receiveText, positiveView -> {
            mConnectionController.accept(roomId);
            dialog.dismiss();
        });
        dialog.show();
    }
}
