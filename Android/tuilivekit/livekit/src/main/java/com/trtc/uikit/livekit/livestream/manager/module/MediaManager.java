package com.trtc.uikit.livekit.livestream.manager.module;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.state.LiveState;

import java.util.HashMap;
import java.util.Map;

public class MediaManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("MediaManager");

    public MediaManager(LiveState state, ILiveService service) {
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
}
