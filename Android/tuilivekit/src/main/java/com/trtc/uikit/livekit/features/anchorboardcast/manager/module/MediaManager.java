package com.trtc.uikit.livekit.features.anchorboardcast.manager.module;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.TypedValue;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;

import java.util.HashMap;
import java.util.Map;

public class MediaManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("MediaManager");

    public MediaManager(AnchorState state, IAnchorAPI service) {
        super(state, service);
    }

    @Override
    public void destroy() {
        LOGGER.info("destroy");
        releaseVideoMuteBitmap();
        enableMultiPlaybackQuality(false);
    }

    public void setLocalVideoView(TUIVideoView view) {
        mLiveService.setLocalVideoView(view);
    }

    public void enableMultiPlaybackQuality(boolean enable) {
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("AdvanceSettingManager", "enableMultiPlaybackQuality", params);
    }

    public void createVideoMuteBitmap(Context context, int bigResId, int smallResId) {
        if (mMediaState.bigMuteBitmap == null)
            mMediaState.bigMuteBitmap = createMuteBitmap(context, bigResId);
        if (mMediaState.smallMuteBitmap == null)
            mMediaState.smallMuteBitmap = createMuteBitmap(context, smallResId);
    }

    private void releaseVideoMuteBitmap() {
        if (mMediaState.bigMuteBitmap != null && !mMediaState.bigMuteBitmap.isRecycled())
            mMediaState.bigMuteBitmap.recycle();
        if (mMediaState.smallMuteBitmap != null && !mMediaState.smallMuteBitmap.isRecycled())
            mMediaState.smallMuteBitmap.recycle();
        mMediaState.bigMuteBitmap = mMediaState.smallMuteBitmap = null;
    }

    private Bitmap createMuteBitmap(Context context, int resId) {
        TypedValue tv = new TypedValue();
        context.getResources().openRawResource(resId, tv);
        BitmapFactory.Options opt = new BitmapFactory.Options();
        opt.inDensity = tv.density;
        opt.inScaled = false;
        return BitmapFactory.decodeResource(context.getResources(), resId, opt);
    }

    public void setCustomVideoProcess() {
        TEBeautyManager.INSTANCE.setCustomVideoProcess();
    }

    public void enablePipMode(boolean enable) {
        mMediaState.isPipModeEnabled.setValue(enable);
    }

    public void onError(TUICommonDefine.Error errorCode, TUICommonDefine.Error errorCode1) {
        if (TUICommonDefine.Error.CAMERA_OCCUPIED == errorCode) {
            mMediaState.isCameraOccupied = true;
        }
    }

    public void resetCameraOccupied() {
        mMediaState.isCameraOccupied = false;
    }
}
