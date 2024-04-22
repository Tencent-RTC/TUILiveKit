package com.trtc.uikit.livekit.liveroom.view.common.video;

import android.content.Context;
import android.text.TextUtils;
import android.util.Log;

import com.trtc.uikit.livekit.liveroom.core.EngineManager;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.HashMap;
import java.util.Map;

public class VideoViewFactory {

    private static final String TAG = "VideoViewFactory";

    public static VideoViewFactory instance = new VideoViewFactory();

    public Map<String, VideoView> mVideoViewMap = new HashMap<>();

    private VideoViewFactory() {
    }

    public VideoView createVideoView(UserInfo userInfo, LiveRoomInfo roomInfo, Context context) {
        if (userInfo == null || TextUtils.isEmpty(userInfo.userId)) {
            Log.e(TAG, "createVideoView userInfo:" + userInfo);
            return null;
        }
        VideoView videoView = findVideoView(userInfo.userId);
        if (null != videoView) {
            return videoView;
        }
        if (LiveKitStore.sharedInstance().selfInfo.userId.equals(userInfo.userId)) {
            videoView = new PusherVideoView(context, roomInfo, userInfo,
                    getRoomEngineService(roomInfo));
        } else {
            videoView = new PlayerVideoView(context, roomInfo,
                    userInfo, getRoomEngineService(roomInfo));
        }
        mVideoViewMap.put(userInfo.userId, videoView);
        return videoView;
    }

    private RoomEngineService getRoomEngineService(LiveRoomInfo roomInfo) {
        return roomInfo.anchorInfo.equals(LiveKitStore.sharedInstance().selfInfo)
                ? EngineManager.sharedInstance().mAnchorEngineService :
                EngineManager.sharedInstance().mRoomEngineMap.get(roomInfo.roomId);
    }

    public void clear() {
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
