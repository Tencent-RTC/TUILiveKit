package com.trtc.uikit.livekit.features.anchorprepare.state.mediator;

import androidx.lifecycle.LiveData;

import com.trtc.uikit.livekit.common.utils.Logger;
import com.trtc.uikit.livekit.common.utils.ReadOnlyLiveData;
import com.trtc.uikit.livekit.common.utils.ReadOnlyTransformLiveData;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareState;

public final class PrepareStateMediator {
    private static final Logger LOGGER = Logger.getLiveStreamLogger("PrepareStateMediator");

    public LiveData<String>                                          coverURL;
    public LiveData<AnchorPrepareViewDefine.LiveStreamPrivacyStatus> liveMode;
    public LiveData<String>                                          roomName;
    public LiveData<Boolean>                                         startLiveClick;

    public PrepareStateMediator(AnchorPrepareState state) {
        coverURL = new ReadOnlyLiveData<>(state.coverURL,
                value -> {
                    LOGGER.info(" AnchorPrepareState coverURL Change:" + value);
                    return value;
                });

        liveMode = new ReadOnlyLiveData<>(state.liveMode,
                value -> {
                    LOGGER.info(" AnchorPrepareState liveMode Change:" + value);
                    return value;
                });

        roomName = new ReadOnlyLiveData<>(state.roomName,
                value -> {
                    LOGGER.info(" AnchorPrepareState roomName Change:" + value);
                    return value;
                });
        startLiveClick = new ReadOnlyLiveData<>(state.startLiveClick,
                value -> {
                    LOGGER.info(" AnchorPrepareState startLiveClick Change:" + value);
                    return value;
                });
    }

    public void destroy() {
        LOGGER.info("PrepareStateMediator State destroy");
        ((ReadOnlyLiveData<?>) coverURL).destroy();
        ((ReadOnlyLiveData<?>) liveMode).destroy();
        ((ReadOnlyLiveData<?>) roomName).destroy();
        ((ReadOnlyLiveData<?>) startLiveClick).destroy();
    }
}
