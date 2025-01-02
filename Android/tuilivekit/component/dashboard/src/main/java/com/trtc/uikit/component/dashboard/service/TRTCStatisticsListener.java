package com.trtc.uikit.component.dashboard.service;

import com.tencent.trtc.TRTCStatistics;

import java.util.ArrayList;

public abstract class TRTCStatisticsListener {

    public void onNetworkStatisticsChange(int rtt, int upLoss, int downLoss) {
    }

    public void onLocalStatisticsChange(ArrayList<TRTCStatistics.TRTCLocalStatistics> localArray) {
    }

    public void onRemoteStatisticsChange(ArrayList<TRTCStatistics.TRTCRemoteStatistics> remoteArray) {
    }
}
