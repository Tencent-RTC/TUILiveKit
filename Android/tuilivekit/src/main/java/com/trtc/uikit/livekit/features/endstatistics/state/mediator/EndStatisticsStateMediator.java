package com.trtc.uikit.livekit.features.endstatistics.state.mediator;

import androidx.lifecycle.LiveData;

import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.utils.ReadOnlyLiveData;
import com.trtc.uikit.livekit.features.endstatistics.state.EndStatisticsState;

public class EndStatisticsStateMediator {
    private final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("EndStatisticsStateMediator");

    public final LiveData<Boolean> exitClick;

    public EndStatisticsStateMediator(EndStatisticsState state) {
        exitClick = new ReadOnlyLiveData<>(state.exitClick, value -> {
            LOGGER.info("exitClick Change:" + value);
            return value;
        });
    }
}
