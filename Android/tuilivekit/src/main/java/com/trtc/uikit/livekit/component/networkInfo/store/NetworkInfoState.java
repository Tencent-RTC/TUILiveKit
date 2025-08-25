package com.trtc.uikit.livekit.component.networkInfo.store;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.NetworkQuality.UNKNOWN;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.DEFAULT;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public class NetworkInfoState {
    public MutableLiveData<Status>                         videoStatus              =
            new MutableLiveData<>(Status.Normal);
    public MutableLiveData<Status>                         audioStatus              =
            new MutableLiveData<>(Status.Normal);
    public MutableLiveData<Boolean>                        isTakeInSeat             = new MutableLiveData<>(false);
    public MutableLiveData<Boolean>                        isNetworkConnected       = new MutableLiveData<>(true);
    public MutableLiveData<TUICommonDefine.NetworkQuality> networkStatus            = new MutableLiveData<>(UNKNOWN);
    public MutableLiveData<String>                         resolution               = new MutableLiveData<>("");
    public MutableLiveData<TUIRoomDefine.AudioQuality>     audioMode                = new MutableLiveData<>(DEFAULT);
    public MutableLiveData<Integer>                        audioCaptureVolume       = new MutableLiveData<>(0);
    public MutableLiveData<Long>                           createTime               = new MutableLiveData<>(0L);
    public Boolean                                         isDeviceThermal          = false;
    public MutableLiveData<Boolean>                        isDisplayNetworkWeakTips = new MutableLiveData<>(false);
    public MutableLiveData<Integer>                        rtt                      = new MutableLiveData<>(0);
    public MutableLiveData<Integer>                        upLoss                   = new MutableLiveData<>(0);
    public MutableLiveData<Integer>                        downLoss                 = new MutableLiveData<>(0);
    public MutableLiveData<Boolean>                        roomDismissed            = new MutableLiveData<>(false);

    public NetworkInfoState() {
    }

    public enum Status {
        Mute,
        Closed,
        Normal,
        Abnormal
    }
}
