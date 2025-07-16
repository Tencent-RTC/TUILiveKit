package com.trtc.uikit.livekit.features.anchorboardcast.state.mediator;

import androidx.lifecycle.LiveData;

import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.utils.ReadOnlyLiveData;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;

public final class AnchorStateMediator {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorStateMediator");

    public LiveData<Boolean> anchorData;

    public AnchorStateMediator(AnchorState state) {
        anchorData = new ReadOnlyLiveData<>(state.roomState.isPublicVisible,
                value -> {
                    LOGGER.info(" AnchorStateMediator anchorData Change:" + value);
                    return value;
                });
    }

    public void destroy() {
        LOGGER.info("AnchorStateMediator State destroy");
        ((ReadOnlyLiveData<?>) anchorData).destroy();
    }
}
