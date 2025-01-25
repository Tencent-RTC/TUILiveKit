package com.trtc.uikit.livekit.livestream.view.widgets.beauty;

import android.content.Context;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;
import com.tencent.trtc.TRTCCloudDef;
import com.tencent.trtc.TRTCCloudListener.TRTCVideoFrameListener;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TEBeautyManager {
    private static final String TAG                 = "TEBeautyManager";
    private static final String TE_BEAUTY_EXTENSION = "TEBeautyExtension";

    private static final TEBeautyManager INSTANCE = new TEBeautyManager();

    private       WeakReference<OnBeautyListener> mListenerRef;
    private       View                            mBeautyPanel;
    private final Handler                         mMainHandler = new Handler(Looper.getMainLooper());

    private final TRTCVideoFrameListener mTRTCVideoFrameListener = new TRTCVideoFrameListener() {
        @Override
        public void onGLContextCreated() {

        }

        @Override
        public int onProcessVideoFrame(TRTCCloudDef.TRTCVideoFrame srcFrame, TRTCCloudDef.TRTCVideoFrame dstFrame) {
            if (mBeautyPanel == null) {
                dstFrame.texture.textureId = srcFrame.texture.textureId;
                return 0;
            }
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
            mMainHandler.post(() -> {
                mBeautyPanel = null;
                if (mListenerRef != null) {
                    OnBeautyListener listener = mListenerRef.get();
                    if (listener != null) {
                        listener.onDestroyBeautyView();
                    }
                }
            });
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

    public void init(Context context) {
        if (mBeautyPanel == null) {
            createBeautyKit(context);
            return;
        }
        ViewGroup parent = (ViewGroup) mBeautyPanel.getParent();
        if (parent != null) {
            parent.removeView(mBeautyPanel);
        }
        if (mListenerRef != null) {
            OnBeautyListener listener = mListenerRef.get();
            if (listener != null) {
                listener.onCreateBeautyView(mBeautyPanel);
            }
        }
    }

    private void createBeautyKit(Context context) {
        checkBeautyResource(context, new TUIServiceCallback() {
            @Override
            public void onServiceCallback(int errorCode, String errorMessage, Bundle bundle) {
                if (errorCode == 0) {
                    initBeautyKit(context);
                } else {
                    Log.e(TAG, "checkBeautyResource failed: " + errorMessage);
                }
            }
        });
    }

    private void initBeautyKit(Context context) {
        Log.i(TAG, "initBeautyKit");
        Map<String, Object> params = new HashMap<>();
        params.put("context", context);
        TUICore.callService(TE_BEAUTY_EXTENSION, "initBeautyKit", params, new TUIServiceCallback() {
            @Override
            public void onServiceCallback(int errorCode, String errorMessage, Bundle bundle) {
                if (errorCode != 0) {
                    Log.e(TAG, "initBeautyKit failed: " + errorMessage);
                    return;
                }
                Log.i(TAG, "initBeautyKit success");
                mMainHandler.post(() -> {
                    mBeautyPanel = createTEBeautyPanel(context);
                    if (mListenerRef != null) {
                        OnBeautyListener listener = mListenerRef.get();
                        if (listener != null) {
                            listener.onCreateBeautyView(mBeautyPanel);
                        }
                    }
                });
            }
        });
    }

    private void checkBeautyResource(Context context, TUIServiceCallback callback) {
        Log.i(TAG, "checkBeautyResource");
        Map<String, Object> params = new HashMap<>();
        params.put("context", context);
        TUICore.callService(TE_BEAUTY_EXTENSION, "checkResource", params, callback);
    }

    private void destroyBeautyKit() {
        Log.i(TAG, "destroyBeautyKit");
        TUICore.callService(TE_BEAUTY_EXTENSION, "destroyBeautyKit", null);
    }

    private View createTEBeautyPanel(Context context) {
        Map<String, Object> param = new HashMap<>();
        param.put("context", context);
        List<TUIExtensionInfo> extensionList = TUICore.getExtensionList("TEBeautyExtension", param);
        for (TUIExtensionInfo extensionInfo : extensionList) {
            Map<String, Object> paramMap = extensionInfo.getData();
            Object object = paramMap.get("beautyPanel");
            if (object instanceof View) {
                return (View) object;
            }
        }
        return null;
    }

    public interface OnBeautyListener {
        void onCreateBeautyView(View view);

        void onDestroyBeautyView();
    }
}
