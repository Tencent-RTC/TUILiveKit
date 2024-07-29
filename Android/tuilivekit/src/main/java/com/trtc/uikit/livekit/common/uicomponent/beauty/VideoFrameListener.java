package com.trtc.uikit.livekit.common.uicomponent.beauty;

import android.content.Context;
import android.util.Log;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.trtc.TRTCCloudDef;
import com.tencent.trtc.TRTCCloudListener;

import java.util.HashMap;
import java.util.Map;

public class VideoFrameListener implements TRTCCloudListener.TRTCVideoFrameListener {
    private final Context mContext;

    public VideoFrameListener(Context context) {
        mContext = context;
    }

    @Override
    public void onGLContextCreated() {
        Map<String, Object> param = new HashMap<>();
        param.put("context", mContext);
        TUICore.callService("TEBeautyExtension", "initBeautyKit", param);
    }

    @Override
    public int onProcessVideoFrame(TRTCCloudDef.TRTCVideoFrame srcFrame, TRTCCloudDef.TRTCVideoFrame dstFrame) {
        Map<String, Object> param = new HashMap<>();
        param.put("srcTextureId", srcFrame.texture.textureId);
        param.put("frameWidth", srcFrame.width);
        param.put("frameHeight", srcFrame.height);
        dstFrame.texture.textureId =  (int) TUICore.callService("TEBeautyExtension",
                "processVideoFrame", param);
        return 0;
    }

    @Override
    public void onGLContextDestory() {
        TUICore.callService("TEBeautyExtension", "destroyBeautyKit", null);
    }
}
