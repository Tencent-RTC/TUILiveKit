package com.trtc.uikit.livekit.features.audiencecontainer.manager.module;

import static com.trtc.uikit.livekit.common.utils.MutableLiveDataUtils.setValue;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.TypedValue;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.api.ILiveService;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceState;

import java.util.HashMap;
import java.util.Map;

public class MediaManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("MediaManager");

    public MediaManager(AudienceState state, ILiveService service) {
        super(state, service);
        enableUltimate(true);
        enableH265(true);
    }

    @Override
    public void destroy() {
        LOGGER.info("destroy");
        enableUltimate(false);
        enableH265(false);
    }

    public void createVideoMuteBitmap(Context context, int bigResId, int smallResId) {
        if (mMediaState.bigMuteBitmap == null)
            mMediaState.bigMuteBitmap = createMuteBitmap(context, bigResId);
        if (mMediaState.smallMuteBitmap == null)
            mMediaState.smallMuteBitmap = createMuteBitmap(context, smallResId);
    }

    public void releaseVideoMuteBitmap() {
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

    public void setLocalVideoView(TUIVideoView view) {
        mLiveService.setLocalVideoView(view);
    }

    public void enableUltimate(boolean enable) {
        mMediaState.videoAdvanceSetting.isUltimateEnabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("AdvanceSettingManager", "enableUltimate", params);

        if (enable) {
            enableBFrame(false);
        }
    }

    public void enableBFrame(boolean enable) {
        mMediaState.videoAdvanceSetting.isBFrameEnabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("AdvanceSettingManager", "enableBFrame", params);
    }

    public void enableH265(boolean enable) {
        mMediaState.videoAdvanceSetting.isH265Enabled = enable;
        Map<String, Object> params = new HashMap<>();
        params.put("enable", enable);
        TUICore.callService("AdvanceSettingManager", "enableH265", params);
    }

    public void setCustomVideoProcess() {
        TEBeautyManager.getInstance().setCustomVideoProcess();
    }

    public void enablePictureInPictureMode(boolean enable) {
        LOGGER.info("enablePictureInPictureMode enable:" + enable);
        setValue(mMediaState.isPictureInPictureMode, enable);
    }
}
