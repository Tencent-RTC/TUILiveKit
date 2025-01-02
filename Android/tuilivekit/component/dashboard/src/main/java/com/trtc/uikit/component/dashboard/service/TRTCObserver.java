package com.trtc.uikit.component.dashboard.service;

import com.tencent.trtc.TRTCCloudListener;
import com.tencent.trtc.TRTCStatistics;
import com.trtc.uikit.component.dashboard.service.TRTCStatisticsListener;

public class TRTCObserver extends TRTCCloudListener {
    private TRTCStatisticsListener mTRTCStatisticsListener;

    public void setListener(TRTCStatisticsListener listener) {
        mTRTCStatisticsListener = listener;
    }

    @Override
    public void onStatistics(TRTCStatistics statistics) {
        if (mTRTCStatisticsListener == null) {
            return;
        }
        mTRTCStatisticsListener.onNetworkStatisticsChange(statistics.rtt, statistics.upLoss, statistics.downLoss);
        mTRTCStatisticsListener.onLocalStatisticsChange(statistics.localArray);
        mTRTCStatisticsListener.onRemoteStatisticsChange(statistics.remoteArray);
    }
}
