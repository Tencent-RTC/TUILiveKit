package com.trtc.uikit.component.dashboard;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.Configuration;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.trtc.TRTCStatistics;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.component.dashboard.store.StreamDashboardUserState;
import com.trtc.uikit.component.dashboard.service.TRTCObserver;
import com.trtc.uikit.component.dashboard.service.TRTCStatisticsListener;
import com.trtc.uikit.component.dashboard.view.StreamInfoAdapter;

import java.util.ArrayList;
import java.util.List;

public class StreamDashboardDialog extends PopupDialog {
    private final Context                        mContext;
    private final TRTCObserver                   mTRTCObserver;
    private       RecyclerView                   mRecyclerMediaInfo;
    private       TextView                       mTextUpLoss;
    private       TextView                       mTextDownLoss;
    private       TextView                       mTextRtt;
    private       TextView                       mTextLoading;
    private       LinearLayout                   mLayoutMediaInfo;
    private       StreamInfoAdapter              mAdapter;
    private       int                            mColorGreen;
    private       int                            mColorPink;
    private final List<StreamDashboardUserState> mVideoStatusList = new ArrayList<>();

    public StreamDashboardDialog(@NonNull Context context) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        mContext = context;
        mTRTCObserver = new TRTCObserver();
        initView();
    }

    private void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_stream_dashboard, null);

        bindViewId(view);
        initMediaInfoRecyclerView();
        setTRTCListener();
        setView(view);
    }

    private void bindViewId(View view) {
        mTextRtt = view.findViewById(R.id.tv_rtt);
        mTextDownLoss = view.findViewById(R.id.tv_downLoss);
        mTextUpLoss = view.findViewById(R.id.tv_upLoss);
        mRecyclerMediaInfo = view.findViewById(R.id.rv_media_info);
        mLayoutMediaInfo = view.findViewById(R.id.ll_media_info);
        mTextLoading = view.findViewById(R.id.tv_loading);
        mColorGreen = mContext.getResources().getColor(R.color.livekit_dashboard_color_green);
        mColorPink = mContext.getResources().getColor(R.color.livekit_dashboard_color_pink);
    }

    @Override
    protected void onStart() {
        super.onStart();
        addObserver();
        Window window = getWindow();
        if (window != null) {
            setDialogMaxHeight(window);
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        removeObserver();
    }

    protected void setDialogMaxHeight(Window window) {
        Configuration configuration = getContext().getResources().getConfiguration();
        window.setBackgroundDrawableResource(android.R.color.transparent);
        WindowManager.LayoutParams params = window.getAttributes();
        int screenHeight = getContext().getResources().getDisplayMetrics().heightPixels;
        int height = (int) (screenHeight * 0.75);
        if (configuration.orientation == Configuration.ORIENTATION_LANDSCAPE) {
            params.gravity = Gravity.END;
            params.width = getContext().getResources().getDisplayMetrics().widthPixels / 2;
        } else {
            params.gravity = Gravity.BOTTOM;
            params.width = WindowManager.LayoutParams.MATCH_PARENT;
        }
        params.height = height;
        window.setAttributes(params);
    }

    private void initMediaInfoRecyclerView() {
        mAdapter = new StreamInfoAdapter(getContext(), mVideoStatusList);
        mRecyclerMediaInfo.setLayoutManager(new LinearLayoutManager(getContext()));
        mRecyclerMediaInfo.setAdapter(mAdapter);
    }

    private void setTRTCListener() {
        mTRTCObserver.setListener(new TRTCStatisticsListener() {
            @SuppressLint("DefaultLocale")
            @Override
            public void onNetworkStatisticsChange(int rtt, int upLoss, int downLoss) {
                super.onNetworkStatisticsChange(rtt, upLoss, downLoss);
                if (mLayoutMediaInfo.getVisibility() == View.GONE) {
                    mLayoutMediaInfo.setVisibility(View.VISIBLE);
                    mTextLoading.setVisibility(View.GONE);
                }
                mTextRtt.setText(String.format("%d ms", rtt));
                mTextRtt.setTextColor(rtt > 100 ? mColorPink : mColorGreen);
                mTextDownLoss.setText(String.format("%d%%", downLoss));
                mTextDownLoss.setTextColor(downLoss > 10 ? mColorPink : mColorGreen);
                mTextUpLoss.setText(String.format("%d%%", upLoss));
                mTextUpLoss.setTextColor(upLoss > 10 ? mColorPink : mColorGreen);
            }

            @Override
            public void onLocalStatisticsChange(ArrayList<TRTCStatistics.TRTCLocalStatistics> localArray) {
                super.onLocalStatisticsChange(localArray);
                mAdapter.updateLocalVideoStatus(localArray);
            }

            @Override
            public void onRemoteStatisticsChange(ArrayList<TRTCStatistics.TRTCRemoteStatistics> remoteArray) {
                super.onRemoteStatisticsChange(remoteArray);
                mAdapter.updateRemoteVideoStatus(remoteArray);
            }
        });
    }

    private void addObserver() {
        TUIRoomEngine.sharedInstance().getTRTCCloud().addListener(mTRTCObserver);
    }

    private void removeObserver() {
        TUIRoomEngine.sharedInstance().getTRTCCloud().removeListener(mTRTCObserver);
    }
}
