package com.trtc.uikit.livekit.livestream.state;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.DEFAULT;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_1080P;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;

public class MediaState {
    public LiveData<Boolean>                    hasMicrophonePermission = new LiveData<>(false);
    public LiveData<Boolean>                    isMicrophoneOpened      = new LiveData<>(false);
    public LiveData<Boolean>                    isMicrophoneMuted       = new LiveData<>(true);
    public LiveData<TUIRoomDefine.AudioQuality> audioQuality            = new LiveData<>(DEFAULT);
    public LiveData<Boolean>                    hasCameraPermission     = new LiveData<>(false);
    public LiveData<Boolean>                    isCameraOpened          = new LiveData<>(false);
    public LiveData<TUIRoomDefine.VideoQuality> videoQuality            = new LiveData<>(Q_1080P);

    public void reset() {
        hasMicrophonePermission.set(false);
        isMicrophoneOpened.set(false);
        isMicrophoneMuted.set(true);
        audioQuality.set(DEFAULT);
        hasCameraPermission.set(false);
        isCameraOpened.set(false);
        videoQuality.set(Q_1080P);
    }
}
