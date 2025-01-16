package com.trtc.uikit.livekit.livestream.view;

import android.content.Context;
import android.content.Intent;

import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.VideoLiveKit;
import com.trtc.uikit.livekit.livestream.view.anchor.VideoLiveAnchorActivity;
import com.trtc.uikit.livekit.livestream.view.audience.VideoLiveAudienceActivity;

public class VideoLiveKitImpl implements VideoLiveKit {

    private static volatile VideoLiveKitImpl sInstance;
    private final           Context          mContext;
    private                 boolean          mEnableFollowFeature = true;

    private VideoLiveKitImpl(Context context) {
        mContext = context.getApplicationContext();
    }

    public static synchronized VideoLiveKitImpl createInstance(Context context) {
        if (null == sInstance) {
            synchronized (VideoLiveKitImpl.class) {
                if (null == sInstance) {
                    sInstance = new VideoLiveKitImpl(context);
                }
            }
        }
        return sInstance;
    }

    @Override
    public void startLive(String roomId) {
        FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
        if (floatWindowManager.isShowingFloatWindow()) {
            floatWindowManager.releaseFloatWindow();
        }
        Intent intent = new Intent(mContext, VideoLiveAnchorActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_ROOM_ID, roomId);
        mContext.startActivity(intent);
    }

    @Override
    public void joinLive(String roomId) {
        Intent intent = new Intent(mContext, VideoLiveAudienceActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VideoLiveAudienceActivity.INTENT_KEY_ROOM_ID, roomId);
        mContext.startActivity(intent);
    }

    @Override
    public void enableFollowFeature(boolean enable) {
        mEnableFollowFeature = enable;
    }

    public boolean isEnableFollowFeature() {
        return mEnableFollowFeature;
    }
}
