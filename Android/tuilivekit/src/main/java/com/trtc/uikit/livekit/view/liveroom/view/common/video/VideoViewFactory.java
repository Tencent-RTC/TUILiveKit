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

    public static VideoViewFactory instance = new VideoViewFactory();

    public Map<String, VideoView> mVideoViewMap = new HashMap<>();

    public VideoView mPlaceholderVideoView;

    private VideoViewFactory() {
    }

    public VideoView createVideoView(SeatState.SeatInfo seatInfo, LiveController liveController, Context context) {
        if (seatInfo == null || TextUtils.isEmpty(seatInfo.userId.get())) {
            Log.e(TAG, "createVideoView seatInfo:" + seatInfo);
            return null;
        }
        VideoView videoView = findVideoView(seatInfo.userId.get());
        if (null != videoView) {
            return videoView;
        }
        if (liveController.getUserState().selfInfo.userId.equals(seatInfo.userId.get())) {
            videoView = new PusherVideoView(context, liveController, seatInfo);
        } else {
            videoView = new PlayerVideoView(context, liveController, seatInfo);
        }
        mVideoViewMap.put(seatInfo.userId.get(), videoView);
        return videoView;
    }

    public VideoView createPlaceHolderVideoView(SeatState.SeatInfo seatInfo, LiveController liveController,
                                                Context context) {
        if (mPlaceholderVideoView == null) {
            if (liveController.getUserState().selfInfo.userId.equals(seatInfo.userId.get())) {
                mPlaceholderVideoView = new PusherVideoView(context, liveController, seatInfo);
            } else {
                mPlaceholderVideoView = new PlayerVideoView(context, liveController, seatInfo);
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
                VideoViewFactory.instance.mVideoViewMap.remove(seatInfo.userId.get());
            }
        }
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

    private VideoView findVideoView(String userId) {
        if (TextUtils.isEmpty(userId)) {
            return null;
        }
        if (mVideoViewMap.containsKey(userId)) {
            return mVideoViewMap.get(userId);
        }
        return null;
    }
}
