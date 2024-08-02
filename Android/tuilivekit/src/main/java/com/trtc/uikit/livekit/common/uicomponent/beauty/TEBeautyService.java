package com.trtc.uikit.livekit.common.uicomponent.beauty;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.view.View;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;
import com.tencent.trtc.TRTCCloud;
import com.tencent.trtc.TRTCCloudDef;
import com.tencent.trtc.TRTCCloudDef.TRTCVideoFrame;
import com.tencent.trtc.TRTCCloudListener;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.state.LiveState;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class TEBeautyService {

    public static final String TAG = "TEBeautyService";

    public static final String TE_BEAUTY_EXTENSION = "TEBeautyExtension";

    public static final boolean SUPPORT_TE_BEAUTY_EXTENSION = TUICore.getService(TE_BEAUTY_EXTENSION) != null;

    private final Handler   mMainHandler = new Handler(Looper.getMainLooper());
    private final LiveState mLiveState;
    private final TRTCCloud mTRTCCloud;
    private       View      mBeautyView;

    public TEBeautyService(LiveState liveState, TRTCCloud trtcCloud) {
        mLiveState = liveState;
        mTRTCCloud = trtcCloud;
    }

    public void setCustomVideoProcess() {
        LiveKitLog.info(TAG + " setCustomVideoProcess");
        if (SUPPORT_TE_BEAUTY_EXTENSION) {
            mTRTCCloud.setLocalVideoProcessListener(
                    TRTCCloudDef.TRTC_VIDEO_PIXEL_FORMAT_Texture_2D,
                    TRTCCloudDef.TRTC_VIDEO_BUFFER_TYPE_TEXTURE, new TRTCCloudListener.TRTCVideoFrameListener() {
                        @Override
                        public void onGLContextCreated() {
                            LiveKitLog.info(TAG + " onGLContextCreated");
                            mLiveState.operationState.beautyState.glContextCreateFlag.set(true);
                        }

                        @Override
                        public int onProcessVideoFrame(TRTCVideoFrame srcFrame, TRTCVideoFrame dstFrame) {
                            Map<String, Object> params = new HashMap<>();
                            params.put("srcTextureId", srcFrame.texture.textureId);
                            params.put("frameWidth", srcFrame.width);
                            params.put("frameHeight", srcFrame.height);
                            dstFrame.texture.textureId = (int) TUICore.callService(TE_BEAUTY_EXTENSION,
                                    "processVideoFrame", params);
                            return 0;
                        }

                        @Override
                        public void onGLContextDestory() {
                            LiveKitLog.info(TAG + " onGLContextDestory");
                            mLiveState.operationState.beautyState.glContextCreateFlag.set(false);
                            mMainHandler.post(() -> clearBeautyView());
                            destroyBeautyKit();
                        }
                    });
        }
    }

    public View getBeautyView(Context context) {
        if (mBeautyView == null) {
            mBeautyView = createTEBeautyPanel(context);
        }
        return mBeautyView;
    }

    public void clearBeautyView() {
        mBeautyView = null;
    }

    public void initBeautyKit() {
        LiveKitLog.info(TAG + " initBeautyKit");
        if (SUPPORT_TE_BEAUTY_EXTENSION) {
            Map<String, Object> params = new HashMap<>();
            params.put("context", ContextProvider.getApplicationContext());
            TUICore.callService(TE_BEAUTY_EXTENSION, "initBeautyKit", params);
        }
    }

    private void destroyBeautyKit() {
        LiveKitLog.info(TAG + " destroyBeautyKit");
        TUICore.callService(TE_BEAUTY_EXTENSION, "destroyBeautyKit", null);
    }

    private View createTEBeautyPanel(Context context) {
        if (TEBeautyService.SUPPORT_TE_BEAUTY_EXTENSION) {
            Map<String, Object> param = new HashMap<>();
            param.put("context", context);
            List<TUIExtensionInfo> extensionList = TUICore.getExtensionList(TE_BEAUTY_EXTENSION, param);
            for (TUIExtensionInfo extensionInfo : extensionList) {
                Map<String, Object> paramMap = extensionInfo.getData();
                Object object = paramMap.get("beautyPanel");
                if (object instanceof View) {
                    return (View) object;
                }
            }
        }
        return null;
    }
}
