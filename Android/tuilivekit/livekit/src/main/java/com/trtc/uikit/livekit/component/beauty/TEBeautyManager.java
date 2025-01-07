package com.trtc.uikit.livekit.component.beauty;

import android.content.Context;
import android.os.Bundle;
import android.util.Log;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;
import com.tencent.trtc.TRTCCloudDef;
import com.tencent.trtc.TRTCCloudListener;
import com.trtc.tuikit.common.system.ContextProvider;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;

public class TEBeautyManager {
    private static final String TAG                 = "TEBeautyManager";
    private static final String TE_BEAUTY_EXTENSION = "TEBeautyExtension";

    private static final TEBeautyManager INSTANCE = new TEBeautyManager();

    private WeakReference<OnBeautyListener> mListenerRef;

    private volatile boolean mHasBeautyKit = false;

    private final TRTCCloudListener.TRTCVideoFrameListener mTRTCVideoFrameListener = new TRTCCloudListener.TRTCVideoFrameListener() {
        @Override
        public void onGLContextCreated() {

        }

        @Override
        public int onProcessVideoFrame(TRTCCloudDef.TRTCVideoFrame srcFrame, TRTCCloudDef.TRTCVideoFrame dstFrame) {
            Map<String, Object> params = new HashMap<>();
            params.put("srcTextureId", srcFrame.texture.textureId);
            params.put("frameWidth", srcFrame.width);
            params.put("frameHeight", srcFrame.height);
            dstFrame.texture.textureId = (int) TUICore.callService(TE_BEAUTY_EXTENSION, "processVideoFrame", params);
            return 0;
        }

        @Override
        public void onGLContextDestory() {
            destroyBeautyKit();
            if (mListenerRef != null) {
                OnBeautyListener listener = mListenerRef.get();
                if (listener != null) {
                    listener.onDestroyBeautyKit();
                }
            }
        }
    };

    public static TEBeautyManager getInstance() {
        return INSTANCE;
    }

    private TEBeautyManager() {
    }

    public boolean isSupportTEBeauty() {
        return TUICore.getService(TE_BEAUTY_EXTENSION) != null;
    }

    public void setCustomVideoProcess() {
        if (isSupportTEBeauty()) {
            TUIRoomEngine.sharedInstance().getTRTCCloud().setLocalVideoProcessListener(
                    TRTCCloudDef.TRTC_VIDEO_PIXEL_FORMAT_Texture_2D,
                    TRTCCloudDef.TRTC_VIDEO_BUFFER_TYPE_TEXTURE,
                    mTRTCVideoFrameListener);
        }
    }

    public void setListener(OnBeautyListener listener) {
        mListenerRef = new WeakReference<>(listener);
    }

    public boolean hasBeautyKit() {
        return mHasBeautyKit;
    }

    public void initBeautyKit(OnBeautyListener beautyListener) {
        Log.i(TAG, "initBeautyKit");
        Map<String, Object> params = new HashMap<>();
        params.put("context", ContextProvider.getApplicationContext());
        TUICore.callService(TE_BEAUTY_EXTENSION, "initBeautyKit", params, new TUIServiceCallback() {
            @Override
            public void onServiceCallback(int errorCode, String errorMessage, Bundle bundle) {
                Log.i(TAG, "initBeautyKit success");
                synchronized (TEBeautyManager.class) {
                    mHasBeautyKit = true;
                }
                if (mListenerRef != null) {
                    OnBeautyListener listener = mListenerRef.get();
                    if (listener != null) {
                        listener.onCreateBeautyKit();
                    }
                }
                if (beautyListener != null) beautyListener.onCreateBeautyKit();
            }
        });
    }

    public void checkBeautyResource(Context context, TUIServiceCallback callback) {
        Log.i(TAG, "checkBeautyResource");
        Map<String, Object> params = new HashMap<>();
        params.put("context", context);
        TUICore.callService(TE_BEAUTY_EXTENSION, "checkResource", params, callback);
    }

    private void destroyBeautyKit() {
        Log.i(TAG, "destroyBeautyKit");
        TUICore.callService(TE_BEAUTY_EXTENSION, "destroyBeautyKit", null);
        synchronized (TEBeautyManager.class) {
            mHasBeautyKit = false;
        }
    }

    public interface OnBeautyListener {
        void onCreateBeautyKit();

        void onDestroyBeautyKit();
    }
}
