package com.trtc.uikit.livekit.view.liveroom.view.common.video;

import android.content.Context;
import android.text.TextUtils;
import android.util.Log;

import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class VideoViewFactory {

    private static final String TAG = "VideoViewFactory";

    private final Map<String, VideoView> mVideoViewMap = new HashMap<>();
    private       VideoView              mPlaceholderVideoView;

    public VideoViewFactory() {
    }

    public VideoView createVideoView(RenderVideoViewModel videoUser, LiveController liveController, Context context) {
        if (videoUser == null || TextUtils.isEmpty(videoUser.userId)) {
            Log.e(TAG, "createVideoView seatInfo:" + videoUser);
            return null;
        }
        VideoView videoView = findVideoView(videoUser.userId);
        if (null != videoView) {
            return videoView;
        }
        if (liveController.getUserState().selfInfo.userId.equals(videoUser.userId)) {
            videoView = new PusherVideoView(context, liveController, videoUser);
        } else {
            videoView = new PlayerVideoView(context, liveController, videoUser);
        }
        mVideoViewMap.put(videoUser.userId, videoView);
        return videoView;
    }

    public VideoView createPlaceHolderVideoView(SeatState.SeatInfo seatInfo, LiveController liveController,
                                                Context context) {
        if (mPlaceholderVideoView == null) {
            if (liveController.getUserState().selfInfo.userId.equals(seatInfo.userId.get())) {
                mPlaceholderVideoView = new PusherVideoView(context, liveController,
                        new RenderVideoViewModel(seatInfo,liveController.getRoomState().roomId));
            } else {
                mPlaceholderVideoView = new PlayerVideoView(context, liveController,
                        new RenderVideoViewModel(seatInfo,liveController.getRoomState().roomId));
            }
        }
        return mPlaceholderVideoView;
    }

    public void destroyPlaceHolderVideoView() {
        if (mPlaceholderVideoView != null) {
            mPlaceholderVideoView.clear();
            mPlaceholderVideoView = null;
        }
    }

    public void clearBySeatList(List<SeatState.SeatInfo> seatList) {
        if (!seatList.isEmpty()) {
            for (SeatState.SeatInfo seatInfo : seatList) {
                this.removeVideoViewByUserId(seatInfo.userId.get());
            }
        }
    }

    public void removeVideoViewByUserId(String userId) {
        this.mVideoViewMap.remove(userId);
    }

    public void clear() {
        destroyPlaceHolderVideoView();
        if (!mVideoViewMap.isEmpty()) {
            for (String userId : mVideoViewMap.keySet()) {
                VideoView videoView = mVideoViewMap.get(userId);
                if (videoView != null) {
                    videoView.clear();
                }
            }
            mVideoViewMap.clear();
        }
    }

    public VideoView findVideoView(String userId) {
        if (TextUtils.isEmpty(userId)) {
            return null;
        }
        if (mVideoViewMap.containsKey(userId)) {
            return mVideoViewMap.get(userId);
        }
        return null;
    }
}
