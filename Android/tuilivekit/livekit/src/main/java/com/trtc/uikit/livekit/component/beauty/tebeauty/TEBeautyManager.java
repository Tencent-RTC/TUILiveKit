package com.trtc.uikit.livekit.component.beauty.tebeauty;

import android.content.Context;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.view.ViewGroup;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;
import com.tencent.trtc.TRTCCloudDef;
import com.tencent.trtc.TRTCCloudListener.TRTCVideoFrameListener;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.common.LiveKitLogger;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TEBeautyManager {
    private static final String        TE_BEAUTY_EXTENSION = "TEBeautyExtension";
    private static final LiveKitLogger LOGGER              = LiveKitLogger.getComponentLogger("TEBeautyManager");

    private static final TEBeautyManager INSTANCE = new TEBeautyManager();

    private       WeakReference<OnBeautyListener> mListenerRef;
    private       View                            mBeautyPanel;
    private       String                          mLastParamList;
    private final Handler                         mMainHandler = new Handler(Looper.getMainLooper());

    private static boolean mHasLoaded = false;

    private final TRTCVideoFrameListener mTRTCVideoFrameListener = new TRTCVideoFrameListener() {
        @Override
        public void onGLContextCreated() {
            if (mHasLoaded) {
                mMainHandler.post(() -> initBeautyKit(ContextProvider.getApplicationContext(), false));
            }
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
            String lastParamList = exportParam();
            destroyBeautyKit();
            mMainHandler.post(() -> {
                if (mLastParamList != null) {
                    mLastParamList = lastParamList;
                }
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
        mLastParamList = "";
        if (isSupportTEBeauty()) {
            TUIRoomEngine.sharedInstance().getTRTCCloud().setLocalVideoProcessListener(
                    TRTCCloudDef.TRTC_VIDEO_PIXEL_FORMAT_Texture_2D,
                    TRTCCloudDef.TRTC_VIDEO_BUFFER_TYPE_TEXTURE,
                    mTRTCVideoFrameListener);
        }
    }

    public void clear() {
        LOGGER.info("clear");
        mLastParamList = null;
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
                    initBeautyKit(context, true);
                } else {
                    LOGGER.error("checkBeautyResource failed: " + errorMessage);
                }
            }
        });
    }

    private void initBeautyKit(Context context, boolean createPanel) {
        LOGGER.info("initBeautyKit");
        Map<String, Object> params = new HashMap<>();
        params.put("context", context);
        params.put("lastParamList", mLastParamList);
        TUICore.callService(TE_BEAUTY_EXTENSION, "initBeautyKit", params, new TUIServiceCallback() {
            @Override
            public void onServiceCallback(int errorCode, String errorMessage, Bundle bundle) {
                if (errorCode != 0) {
                    LOGGER.error("initBeautyKit failed: " + errorMessage);
                    return;
                }
                LOGGER.info("initBeautyKit success");
                if (createPanel) {
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
            }
        });
    }

    public void checkBeautyResource(Context context, TUIServiceCallback callback) {
        LOGGER.info("checkBeautyResource");
        Map<String, Object> params = new HashMap<>();
        params.put("context", context);
        TUICore.callService(TE_BEAUTY_EXTENSION, "checkResource", params, new TUIServiceCallback() {
            @Override
            public void onServiceCallback(int errorCode, String errorMessage, Bundle bundle) {
                mHasLoaded = true;
                if (callback != null) {
                    callback.onServiceCallback(errorCode, errorMessage, bundle);
                }
            }
        });
    }

    private void destroyBeautyKit() {
        LOGGER.info("destroyBeautyKit");
        TUICore.callService(TE_BEAUTY_EXTENSION, "destroyBeautyKit", null);
    }

    private String exportParam() {
        String param = String.valueOf(TUICore.callService(TE_BEAUTY_EXTENSION, "exportParam", null));
        LOGGER.info("exportParam:" + param);
        return param;
    }

    private View createTEBeautyPanel(Context context) {
        Map<String, Object> param = new HashMap<>();
        param.put("context", context);
        param.put("lastParamList", mLastParamList);
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
