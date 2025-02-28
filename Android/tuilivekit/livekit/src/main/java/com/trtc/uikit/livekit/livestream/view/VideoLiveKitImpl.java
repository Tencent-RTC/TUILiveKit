package com.trtc.uikit.livekit.livestream.view;

import android.content.Context;
import android.content.Intent;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.VideoLiveKit;
import com.trtc.uikit.livekit.livestream.view.anchor.VideoLiveAnchorActivity;
import com.trtc.uikit.livekit.livestream.view.audience.VideoLiveAudienceActivity;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class VideoLiveKitImpl implements VideoLiveKit {

    private static volatile VideoLiveKitImpl sInstance;

    private final Context                                 mContext;
    private final List<WeakReference<CallingAPIListener>> mCallingAPIListenerList = new CopyOnWriteArrayList<>();
    private       boolean                                 mEnableFollowFeature    = true;

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
    public void leaveLive(TUIRoomDefine.ActionCallback callback) {
        TUIRoomEngine.sharedInstance().exitRoom(true, callback);
        notifyLeaveLive();
        if (FloatWindowManager.getInstance().isShowingFloatWindow()) {
            FloatWindowManager.getInstance().releaseFloatWindow();
        }
    }

    @Override
    public void stopLive(TUIRoomDefine.ActionCallback callback) {
        TUIRoomEngine.sharedInstance().destroyRoom(callback);
        notifyStopLive();
        if (FloatWindowManager.getInstance().isShowingFloatWindow()) {
            FloatWindowManager.getInstance().releaseFloatWindow();
        }
    }

    @Override
    public void enableFollowFeature(boolean enable) {
        mEnableFollowFeature = enable;
    }

    public boolean isEnableFollowFeature() {
        return mEnableFollowFeature;
    }

    public void addCallingAPIListener(CallingAPIListener listener) {
        if (listener != null) {
            mCallingAPIListenerList.add(new WeakReference<>(listener));
        }
    }

    public void removeCallingAPIListener(CallingAPIListener listener) {
        for (WeakReference<CallingAPIListener> listenerWeakReference : mCallingAPIListenerList) {
            if (listenerWeakReference.get() == listener) {
                mCallingAPIListenerList.remove(listenerWeakReference);
            }
        }
    }

    public interface CallingAPIListener {
        void onLeaveLive();

        void onStopLive();
    }

    private void notifyLeaveLive() {
        for (WeakReference<CallingAPIListener> listenerWeakReference : mCallingAPIListenerList) {
            CallingAPIListener callingAPIListener = listenerWeakReference.get();
            if (callingAPIListener != null) {
                callingAPIListener.onLeaveLive();
            }
        }
    }

    private void notifyStopLive() {
        for (WeakReference<CallingAPIListener> listenerWeakReference : mCallingAPIListenerList) {
            CallingAPIListener callingAPIListener = listenerWeakReference.get();
            if (callingAPIListener != null) {
                callingAPIListener.onStopLive();
            }
        }
    }
}
