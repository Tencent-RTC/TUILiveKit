package com.trtc.uikit.livekit.view.liveroom;

import android.content.Context;
import android.content.Intent;

import com.trtc.uikit.livekit.VideoLiveKit;

public class VideoLiveKitImpl implements VideoLiveKit {

    private static volatile VideoLiveKitImpl sInstance;
    private final           Context          mContext;

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
}
