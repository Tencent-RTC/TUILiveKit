package com.trtc.uikit.livekit.component.networkInfo.service;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.NetworkQuality.BAD;
import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.NetworkQuality.DOWN;
import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.NetworkQuality.VERY_BAD;
import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_STREAM_TYPE_BIG;
import static com.trtc.uikit.livekit.component.networkInfo.store.NetworkInfoState.Status.Abnormal;
import static com.trtc.uikit.livekit.component.networkInfo.store.NetworkInfoState.Status.Closed;
import static com.trtc.uikit.livekit.component.networkInfo.store.NetworkInfoState.Status.Mute;
import static com.trtc.uikit.livekit.component.networkInfo.store.NetworkInfoState.Status.Normal;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.ConnectivityManager;
import android.net.Network;
import android.net.NetworkCapabilities;
import android.net.NetworkRequest;
import android.os.BatteryManager;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.trtc.TRTCCloud;
import com.tencent.trtc.TRTCCloudListener;
import com.tencent.trtc.TRTCStatistics;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.networkInfo.store.NetworkInfoState;

import java.util.List;
import java.util.Map;

public class NetworkInfoService {
    private static final long NETWORK_WEAK_THRESHOLD_MS     = 30_000;
    private static final int  LOW_FRAME_RATE_THRESHOLD      = 15;
    private static final int  LOW_BITRATE_THRESHOLD_240P    = 100;
    private static final int  LOW_BITRATE_THRESHOLD_360P    = 200;
    private static final int  LOW_BITRATE_THRESHOLD_480P    = 350;
    private static final int  LOW_BITRATE_THRESHOLD_540x540 = 500;
    private static final int  LOW_BITRATE_THRESHOLD_540x960 = 800;
    private static final int  LOW_BITRATE_THRESHOLD_1080P   = 1500;

    private final        Context                             mContext;
    private static final LiveKitLogger                       LOGGER                     =
            LiveKitLogger.getComponentLogger("NetworkInfoService");
    private final        String                              mUserId;
    public               NetworkInfoState                    mNetworkInfoState          = new NetworkInfoState();
    private final        TRTCCloud                           mTRTCCloud                 =
            TUIRoomEngine.sharedInstance().getTRTCCloud();
    private final        TUIRoomEngine                       mTUIRoomEngine             =
            TUIRoomEngine.sharedInstance();
    private              ConnectivityManager.NetworkCallback networkCallback;
    private              BroadcastReceiver                   networkReceiver;
    private              long                                mNetworkBadStartTime       = 0;
    private final        Observer<Boolean>                   mNetworkConnectionObserver =
            this::onNetworkConnectionChange;

    private final TUIRoomObserver mEngineObserver = new TUIRoomObserver() {
        @Override
        public void onUserNetworkQualityChanged(Map<String, TUICommonDefine.NetworkInfo> networkMap) {
            handleUserNetworkQualityChange(networkMap);
        }

        @Override
        public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
                , TUIRoomDefine.ChangeReason reason) {
            handleVideoStateChanged(userId, hasVideo);
        }

        @Override
        public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
            handleAudioStateChanged(userId, hasAudio);
        }

        @Override
        public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                      List<TUIRoomDefine.SeatInfo> leftList) {
            handleSeatListChanged(seatList);
        }

        @Override
        public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
            handleRoomDismissed();
        }
    };

    private final TRTCCloudListener mTRTCObserver = new TRTCCloudListener() {
        @Override
        public void onStatistics(TRTCStatistics statistics) {
            handleLocalStreamStatistics(statistics);
        }
    };

    public NetworkInfoService(Context context) {
        mContext = context.getApplicationContext();
        mUserId = TUIRoomEngine.getSelfInfo().userId;
    }

    public void initAudioCaptureVolume() {
        LOGGER.info("initAudioCaptureVolume:[]");
        mNetworkInfoState.audioCaptureVolume.setValue(mTRTCCloud.getAudioCaptureVolume());
    }

    public void setAudioCaptureVolume(int volume) {
        LOGGER.info("setAudioCaptureVolume:[volume:" + volume + "]");
        mNetworkInfoState.audioCaptureVolume.setValue(volume);
        mTRTCCloud.setAudioCaptureVolume(volume);
    }

    public void updateAudioStatusByVolume(int volume) {
        LOGGER.info("updateAudioStatusByVolume:[volume:" + volume + "]");
        if (mNetworkInfoState.audioStatus.getValue() != Closed) {
            mNetworkInfoState.audioStatus.setValue(volume == 0 ? Mute : Normal);
        }
    }

    public void updateAudioMode(TUIRoomDefine.AudioQuality audioQuality) {
        LOGGER.info("updateAudioMode:[audioQuality:" + audioQuality + "]");
        mNetworkInfoState.audioMode.setValue(audioQuality);
        mTUIRoomEngine.updateAudioQuality(audioQuality);
    }

    public void checkDeviceTemperature(Context context) {
        LOGGER.info("checkDeviceTemperature:[context:" + context + "]");
        Intent intent = context.registerReceiver(null, new IntentFilter(Intent.ACTION_BATTERY_CHANGED));
        if (intent != null) {
            int temp = intent.getIntExtra(BatteryManager.EXTRA_TEMPERATURE, 0);
            float temperature = temp / 10.0f;
            mNetworkInfoState.isDeviceThermal = temperature > 45.0f;
        }
    }

    public void addObserver() {
        startListenNetworkConnection();
        mTRTCCloud.addListener(mTRTCObserver);
        mTUIRoomEngine.addObserver(mEngineObserver);
        mNetworkInfoState.isNetworkConnected.observeForever(mNetworkConnectionObserver);
    }

    public void removeObserver() {
        stopListenNetworkConnection();
        mTRTCCloud.removeListener(mTRTCObserver);
        mTUIRoomEngine.removeObserver(mEngineObserver);
        mNetworkInfoState.isNetworkConnected.removeObserver(mNetworkConnectionObserver);
    }

    private void handleUserNetworkQualityChange(Map<String, TUICommonDefine.NetworkInfo> networkMap) {
        if (Boolean.FALSE.equals(mNetworkInfoState.isNetworkConnected.getValue())) {
            return;
        }
        TUICommonDefine.NetworkInfo info = networkMap.get(mUserId);
        if (info != null) {
            updateNetworkInfo(info);
            checkAndShowNetworkWeakTips(info);
        }
    }

    private void handleAudioStateChanged(String userId, boolean hasAudio) {
        if (TextUtils.equals(userId, mUserId)) {
            if (hasAudio && mNetworkInfoState.audioStatus.getValue() != Mute) {
                mNetworkInfoState.audioStatus.setValue(Normal);
            } else {
                mNetworkInfoState.audioStatus.setValue(Closed);
            }
        }
    }

    private void handleSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList) {
        if (seatList == null || seatList.isEmpty()) {
            return;
        }
        for (TUIRoomDefine.SeatInfo seat : seatList) {
            if (TextUtils.equals(seat.userId, mUserId)) {
                mNetworkInfoState.isTakeInSeat.setValue(true);
                return;
            }
        }
        mNetworkInfoState.isTakeInSeat.setValue(false);
    }

    private void handleRoomDismissed() {
        mNetworkInfoState.roomDismissed.setValue(true);
    }

    private void handleVideoStateChanged(String userId, boolean hasVideo) {
        if (TextUtils.equals(userId, mUserId)) {
            mNetworkInfoState.videoStatus.setValue(hasVideo ? Normal : Closed);
        }
    }

    private void handleLocalStreamStatistics(TRTCStatistics statistics) {
        TRTCStatistics.TRTCLocalStatistics localStreamStats = getLocalStreamStats(statistics);
        if (localStreamStats == null) {
            return;
        }
        updateResolution(localStreamStats);
        updateVideoStatus(localStreamStats);
        updateAudioStatus(localStreamStats);
    }

    private void updateNetworkInfo(TUICommonDefine.NetworkInfo info) {
        mNetworkInfoState.rtt.setValue(info.delay);
        mNetworkInfoState.downLoss.setValue(info.downLoss);
        mNetworkInfoState.upLoss.setValue(info.upLoss);
        mNetworkInfoState.networkStatus.setValue(info.quality);
    }

    private void checkAndShowNetworkWeakTips(TUICommonDefine.NetworkInfo info) {
        boolean isNetworkWeak = (info.quality == BAD || info.quality == VERY_BAD || info.quality == DOWN);
        long currentTime = System.currentTimeMillis();
        if (isNetworkWeak) {
            if (mNetworkBadStartTime == 0) {
                mNetworkBadStartTime = currentTime;
            } else if (currentTime - mNetworkBadStartTime >= NETWORK_WEAK_THRESHOLD_MS) {
                mNetworkInfoState.isDisplayNetworkWeakTips.setValue(true);
                mNetworkBadStartTime = currentTime;
            }
        } else {
            mNetworkBadStartTime = 0;
        }
        TUICommonDefine.NetworkQuality networkStatus = mNetworkInfoState.networkStatus.getValue();
        if (networkStatus == VERY_BAD || networkStatus == DOWN) {
            mNetworkInfoState.videoStatus.setValue(Abnormal);
        }
    }

    private TRTCStatistics.TRTCLocalStatistics getLocalStreamStats(TRTCStatistics statistics) {
        for (TRTCStatistics.TRTCLocalStatistics stat : statistics.localArray) {
            if (stat.streamType == TRTC_VIDEO_STREAM_TYPE_BIG) {
                return stat;
            }
        }
        return null;
    }

    private void updateResolution(TRTCStatistics.TRTCLocalStatistics statistics) {
        mNetworkInfoState.resolution.setValue(statistics.width + " P");
    }

    private void updateVideoStatus(TRTCStatistics.TRTCLocalStatistics statistics) {
        if (mNetworkInfoState.videoStatus.getValue() == Closed) {
            return;
        }
        TUICommonDefine.NetworkQuality networkStatus = mNetworkInfoState.networkStatus.getValue();
        if (networkStatus == VERY_BAD || networkStatus == DOWN) {
            mNetworkInfoState.videoStatus.setValue(Abnormal);
            return;
        }
        if (statistics.frameRate < LOW_FRAME_RATE_THRESHOLD || isBitrateAbnormal(statistics)) {
            mNetworkInfoState.videoStatus.setValue(Abnormal);
        } else {
            mNetworkInfoState.videoStatus.setValue(Normal);
        }
    }

    private boolean isBitrateAbnormal(TRTCStatistics.TRTCLocalStatistics statistics) {
        int width = statistics.width;
        int height = statistics.height;
        int bitrate = statistics.videoBitrate;
        if (width == 240) {
            return bitrate < LOW_BITRATE_THRESHOLD_240P;
        } else if (width == 360) {
            return bitrate < LOW_BITRATE_THRESHOLD_360P;
        } else if (width == 480) {
            return bitrate < LOW_BITRATE_THRESHOLD_480P;
        } else if (width == 540) {
            if (height == 540) {
                return bitrate < LOW_BITRATE_THRESHOLD_540x540;
            } else if (height == 960) {
                return bitrate < LOW_BITRATE_THRESHOLD_540x960;
            }
        } else if (width == 1080) {
            return bitrate < LOW_BITRATE_THRESHOLD_1080P;
        }
        return false;
    }

    private void updateAudioStatus(TRTCStatistics.TRTCLocalStatistics statistics) {
        if (mNetworkInfoState.audioStatus.getValue() == Closed || mNetworkInfoState.audioStatus.getValue() == Mute) {
            return;
        }
        if (statistics.audioCaptureState == 0) {
            mNetworkInfoState.audioStatus.setValue(NetworkInfoState.Status.Normal);
        } else {
            mNetworkInfoState.audioStatus.setValue(Abnormal);
        }
    }

    private void startListenNetworkConnection() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            registerNetworkCallback();
        } else {
            registerBroadcastReceiver();
        }
    }

    private void stopListenNetworkConnection() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            ConnectivityManager cm = (ConnectivityManager) mContext.getSystemService(Context.CONNECTIVITY_SERVICE);
            if (cm != null && networkCallback != null) {
                cm.unregisterNetworkCallback(networkCallback);
            }
        } else {
            if (networkReceiver != null) {
                mContext.unregisterReceiver(networkReceiver);
            }
        }
    }

    private void registerNetworkCallback() {
        ConnectivityManager cm = (ConnectivityManager) mContext.getSystemService(Context.CONNECTIVITY_SERVICE);
        if (cm == null) return;
        networkCallback = new ConnectivityManager.NetworkCallback() {
            @Override
            public void onAvailable(@NonNull Network network) {
                LOGGER.info("network connected");
                mNetworkInfoState.isNetworkConnected.postValue(true);
            }

            @Override
            public void onLost(@NonNull Network network) {
                LOGGER.info("network disconnected");
                mNetworkInfoState.isNetworkConnected.postValue(false);
            }
        };
        NetworkRequest request = new NetworkRequest.Builder()
                .addCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET)
                .build();
        cm.registerNetworkCallback(request, networkCallback);
    }

    private void registerBroadcastReceiver() {
        networkReceiver = new BroadcastReceiver() {
            private boolean lastConnected = false;

            @Override
            public void onReceive(Context context, Intent intent) {
                boolean connected = isNetworkAvailable(context);
                if (connected != lastConnected) {
                    lastConnected = connected;
                    mNetworkInfoState.isNetworkConnected.postValue(connected);
                    LOGGER.info("network " + (connected ? "connected" : "disconnected"));
                }
            }
        };
        IntentFilter filter = new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION);
        mContext.registerReceiver(networkReceiver, filter);
    }

    private boolean isNetworkAvailable(Context context) {
        if (context == null) return false;
        ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        if (cm == null) return false;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
            Network network = cm.getActiveNetwork();
            if (network == null) return false;
            NetworkCapabilities capabilities = cm.getNetworkCapabilities(network);
            if (capabilities == null) return false;
            return capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET)
                    && (capabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI)
                    || capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR)
                    || capabilities.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET));
        } else {
            android.net.NetworkInfo networkInfo = cm.getActiveNetworkInfo();
            return networkInfo != null && networkInfo.isConnected();
        }
    }

    private void onNetworkConnectionChange(Boolean isConnection) {
        if (!isConnection) {
            mNetworkInfoState.networkStatus.setValue(DOWN);
            mNetworkInfoState.videoStatus.setValue(Abnormal);
        }
    }
}
