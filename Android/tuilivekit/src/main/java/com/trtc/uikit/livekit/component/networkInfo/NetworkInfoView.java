package com.trtc.uikit.livekit.component.networkInfo;

import static java.lang.Long.min;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.networkInfo.service.NetworkInfoService;
import com.trtc.uikit.livekit.component.networkInfo.store.NetworkInfoState;
import com.trtc.uikit.livekit.component.networkInfo.view.NetworkBadTipsDialog;
import com.trtc.uikit.livekit.component.networkInfo.view.NetworkInfoPanel;

public class NetworkInfoView extends FrameLayout {
    private final Context                                  mContext;
    private final NetworkInfoService                       mService;
    private final NetworkInfoState                         mState;
    private       ImageView                                mImageNetworkStatus;
    private       TextView                                 mTextCreateTime;
    private       NetworkInfoPanel                         mNetworkInfoPanel;
    private       LinearLayout                             mLayoutNetworkInfo;
    private       long                                     mCreateTime              = 0L;
    private       Runnable                                 mLiveTimeRunnable;
    private final Handler                                  mHandler                 =
            new Handler(Looper.getMainLooper());
    private final Observer<TUICommonDefine.NetworkQuality> mNetWorkQualityObserver  = this::onNetworkQualityChange;
    private final Observer<Boolean>                        mNetworkWeakTipsObserver = this::onNetworkWeakTipsChange;
    private final Observer<Boolean>                        mRoomDismissedObserver   = this::onRoomRoomDismissed;

    public NetworkInfoView(Context context) {
        this(context, null);
    }

    public NetworkInfoView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public NetworkInfoView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        mService = new NetworkInfoService(context);
        mState = mService.mNetworkInfoState;

        initView();
    }

    public void init(long createTime) {
        long now = System.currentTimeMillis();
        if (createTime <= 0) {
            createTime = now;
        }
        mCreateTime = min(createTime, now);
        startLiveTimer();
    }

    public void setScreenOrientation(boolean isPortrait) {
        if (mLayoutNetworkInfo != null) {
            mLayoutNetworkInfo.setEnabled(isPortrait);
        }
    }

    private void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.network_info_view, this, true);

        bindViewId();
        initNetworkView();
    }

    private void bindViewId() {
        mLayoutNetworkInfo = findViewById(R.id.ll_network_info);
        mImageNetworkStatus = findViewById(R.id.iv_network_status);
        mTextCreateTime = findViewById(R.id.tv_live_time);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
        stopLiveTimer();
    }

    private void addObserver() {
        mService.addObserver();
        mState.networkStatus.observeForever(mNetWorkQualityObserver);
        mState.isDisplayNetworkWeakTips.observeForever(mNetworkWeakTipsObserver);
        mState.roomDismissed.observeForever(mRoomDismissedObserver);
    }

    private void removeObserver() {
        mService.removeObserver();
        mState.networkStatus.removeObserver(mNetWorkQualityObserver);
        mState.isDisplayNetworkWeakTips.removeObserver(mNetworkWeakTipsObserver);
        mState.roomDismissed.removeObserver(mRoomDismissedObserver);
    }

    private void initNetworkView() {
        mLayoutNetworkInfo.setOnClickListener(v -> {
            mNetworkInfoPanel = new NetworkInfoPanel(mContext, mService,
                    Boolean.TRUE.equals(mState.isTakeInSeat.getValue()));
            mNetworkInfoPanel.show();
        });
    }

    private void startLiveTimer() {
        if (mLiveTimeRunnable != null) {
            mHandler.removeCallbacks(mLiveTimeRunnable);
        }
        mLiveTimeRunnable = new Runnable() {
            @Override
            public void run() {
                long now = System.currentTimeMillis();
                long duration = now - mCreateTime;
                mTextCreateTime.setText(formatDuration(duration));
                mHandler.postDelayed(this, 1000);
            }
        };
        mHandler.post(mLiveTimeRunnable);
    }

    private void stopLiveTimer() {
        if (mLiveTimeRunnable != null) {
            mHandler.removeCallbacks(mLiveTimeRunnable);
            mLiveTimeRunnable = null;
        }
    }

    private String formatDuration(long durationMillis) {
        long totalSeconds = durationMillis / 1000;
        long hours = totalSeconds / 3600;
        long minutes = (totalSeconds % 3600) / 60;
        long seconds = totalSeconds % 60;
        return String.format("%02d:%02d:%02d", hours, minutes, seconds);
    }

    private void onNetworkQualityChange(TUICommonDefine.NetworkQuality networkQuality) {
        int resId;
        switch (networkQuality) {
            case POOR:
                resId = R.drawable.network_info_network_status_poor;
                break;
            case BAD:
                resId = R.drawable.network_info_network_status_very_bad;
                break;
            case VERY_BAD:
            case DOWN:
                resId = R.drawable.network_info_network_status_down;
                break;
            default:
                resId = R.drawable.network_info_network_status_good;
                break;
        }
        mImageNetworkStatus.setImageResource(resId);
    }

    private void onNetworkWeakTipsChange(Boolean isShow) {
        if (isShow) {
            NetworkBadTipsDialog dialog = new NetworkBadTipsDialog(mContext);
            dialog.show();
        }
    }

    private void onRoomRoomDismissed(Boolean dismissed) {
        if (dismissed && mNetworkInfoPanel != null && mNetworkInfoPanel.isShowing()) {
            mNetworkInfoPanel.dismiss();
        }
    }
}
