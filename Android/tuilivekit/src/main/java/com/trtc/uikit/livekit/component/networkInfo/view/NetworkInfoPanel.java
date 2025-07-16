package com.trtc.uikit.livekit.component.networkInfo.view;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.DEFAULT;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.MUSIC;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.SPEECH;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.core.content.ContextCompat;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.networkInfo.service.NetworkInfoService;
import com.trtc.uikit.livekit.component.networkInfo.store.NetworkInfoState;

public class NetworkInfoPanel extends PopupDialog {
    private final Context                                  mContext;
    private final NetworkInfoService                       mService;
    private final NetworkInfoState                         mState;
    private final int                                      mColorNormal;
    private final int                                      mColorAbnormal;
    private       boolean                                  mIsTakeSeat;
    private       ImageView                                mImageVideoStatus;
    private       ImageView                                mImageAudioStatus;
    private       ImageView                                mImageDeviceTemp;
    private       ImageView                                mImageNetworkStatus;
    private       TextView                                 mTextVideoStatus;
    private       TextView                                 mTextAudioStatus;
    private       TextView                                 mTextDeviceTemp;
    private       TextView                                 mTextNetworkStatus;
    private       TextView                                 mTextResolution;
    private       TextView                                 mTextAudioMode;
    private       TextView                                 mTextVideoDescription;
    private       LinearLayout                             mLayoutStreamStatus;
    private       TextView                                 mTextRTT;
    private       TextView                                 mTextDownLoss;
    private       TextView                                 mTextUpLoss;
    private       SeekBar                                  mSeekVolume;
    private       TextView                                 mTextVolume;
    private       LinearLayout                             mLayoutAudioMode;
    private final Observer<NetworkInfoState.Status>        mVideoStatusObserver        = this::onVideoStatusChange;
    private final Observer<String>                         mResolutionObserver         = this::onVideoResolutionChange;
    private final Observer<NetworkInfoState.Status>        mAudioStatusObserver        = this::onAudioStatusChange;
    private final Observer<TUIRoomDefine.AudioQuality>     mAudioModeObserver          = this::onAudioQualityChange;
    private final Observer<Integer>                        mAudioCaptureVolumeObserver = this::onVolumeChange;
    private final Observer<TUICommonDefine.NetworkQuality> mNetWorkStatusObserver      = this::onNetWorkStatusChange;
    private final Observer<Integer>                        mRTTObserver                = this::onRTTChange;
    private final Observer<Integer>                        mUpLossObserver             = this::onUpLossChange;
    private final Observer<Integer>                        mDownLossObserver           = this::onDownLossChange;
    private final Observer<Boolean>                        mTakeSeatStatusObserver     = this::onTakeSeatStatusChange;

    public NetworkInfoPanel(Context context, NetworkInfoService service, boolean isTakeSeat) {
        super(context);
        mContext = context;
        mService = service;
        mState = mService.mNetworkInfoState;
        mIsTakeSeat = isTakeSeat;
        mColorNormal = ContextCompat.getColor(mContext, R.color.common_text_color_normal);
        mColorAbnormal = ContextCompat.getColor(mContext, R.color.common_text_color_abnormal);
        initView();
    }

    private void initView() {
        View view = LayoutInflater.from(mContext).inflate(R.layout.network_info_panel, null);
        bindViewId(view);

        initVolumeView();
        setView(view);
    }

    private void bindViewId(View view) {
        mLayoutStreamStatus = view.findViewById(R.id.ll_host_stream_status);
        mImageVideoStatus = view.findViewById(R.id.iv_video_status);
        mImageAudioStatus = view.findViewById(R.id.iv_audio_status);
        mImageDeviceTemp = view.findViewById(R.id.iv_device_temp);
        mImageNetworkStatus = view.findViewById(R.id.iv_network_status);
        mTextVideoStatus = view.findViewById(R.id.tv_video_status);
        mTextAudioStatus = view.findViewById(R.id.tv_audio_status);
        mTextDeviceTemp = view.findViewById(R.id.tv_device_status);
        mTextNetworkStatus = view.findViewById(R.id.tv_network_status);
        mTextResolution = view.findViewById(R.id.tv_resolution);
        mTextAudioMode = view.findViewById(R.id.tv_audio_mode);
        mTextVideoDescription = view.findViewById(R.id.tv_video_quality);
        mTextRTT = view.findViewById(R.id.tv_rtt);
        mTextDownLoss = view.findViewById(R.id.tv_down_loss);
        mTextUpLoss = view.findViewById(R.id.tv_up_loss);
        mSeekVolume = view.findViewById(R.id.sb_audio_volume);
        mTextVolume = view.findViewById(R.id.tv_audio_volume);
        mLayoutAudioMode = view.findViewById(R.id.ll_audio_mode);
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        mService.initAudioCaptureVolume();
        addObserver();
        initStreamStatusVisible();
        initDeviceTempView();
        initAudioModeView();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mState.videoStatus.observeForever(mVideoStatusObserver);
        mState.resolution.observeForever(mResolutionObserver);
        mState.audioStatus.observeForever(mAudioStatusObserver);
        mState.audioMode.observeForever(mAudioModeObserver);
        mState.audioCaptureVolume.observeForever(mAudioCaptureVolumeObserver);
        mState.networkStatus.observeForever(mNetWorkStatusObserver);
        mState.rtt.observeForever(mRTTObserver);
        mState.upLoss.observeForever(mUpLossObserver);
        mState.downLoss.observeForever(mDownLossObserver);
        mState.isTakeInSeat.observeForever(mTakeSeatStatusObserver);
    }

    private void removeObserver() {
        mState.videoStatus.removeObserver(mVideoStatusObserver);
        mState.resolution.removeObserver(mResolutionObserver);
        mState.audioStatus.removeObserver(mAudioStatusObserver);
        mState.audioMode.removeObserver(mAudioModeObserver);
        mState.audioCaptureVolume.removeObserver(mAudioCaptureVolumeObserver);
        mState.networkStatus.removeObserver(mNetWorkStatusObserver);
        mState.rtt.removeObserver(mRTTObserver);
        mState.upLoss.removeObserver(mUpLossObserver);
        mState.downLoss.removeObserver(mDownLossObserver);
        mState.isTakeInSeat.removeObserver(mTakeSeatStatusObserver);
    }

    public void initVolumeView() {
        mSeekVolume.setMax(100);
        mSeekVolume.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                mTextVolume.setText(String.valueOf(progress));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                mService.setAudioCaptureVolume(seekBar.getProgress());
                mService.updateAudioStatusByVolume(seekBar.getProgress());
            }
        });
    }

    private void initStreamStatusVisible() {
        mLayoutStreamStatus.setVisibility(mIsTakeSeat ? View.VISIBLE : View.GONE);
    }

    private void initAudioModeView() {
        if (mState.audioMode.getValue() == SPEECH) {
            mTextAudioMode.setText(R.string.common_audio_mode_speech);
        } else if (mState.audioMode.getValue() == MUSIC) {
            mTextAudioMode.setText(R.string.common_audio_mode_music);
        } else {
            mTextAudioMode.setText(R.string.common_audio_mode_default);
        }
        mLayoutAudioMode.setOnClickListener(v -> {
            hide();
            AudioModePanel audioModePanel = new AudioModePanel(mContext);
            audioModePanel.setAudioModeListener(mService::updateAudioMode);
            audioModePanel.show();
        });
    }

    private void initDeviceTempView() {
        mService.checkDeviceTemperature(mContext);
        mImageDeviceTemp.setImageResource(mState.isDeviceThermal ? R.drawable.network_info_device_temp_abnormal :
                R.drawable.network_info_device_temp_normal);
        mTextDeviceTemp.setText(mState.isDeviceThermal ? R.string.common_exception : R.string.common_normal);
    }

    private void onVideoResolutionChange(String resolution) {
        mTextResolution.setText(resolution);
    }

    private void onVideoStatusChange(NetworkInfoState.Status videoStatus) {
        switch (videoStatus) {
            case Normal:
                mImageVideoStatus.setImageResource(R.drawable.network_info_video_status_normal);
                mTextVideoStatus.setText(R.string.common_normal);
                mTextVideoDescription.setText(R.string.common_video_stream_smooth);
                return;
            case Abnormal:
                mImageVideoStatus.setImageResource(R.drawable.network_info_video_status_abnormal);
                mTextVideoStatus.setText(R.string.common_exception);
                mTextVideoDescription.setText(R.string.common_video_stream_freezing);
                return;
            default:
                mImageVideoStatus.setImageResource(R.drawable.network_info_video_status_abnormal);
                mTextVideoStatus.setText(R.string.common_close);
                mTextVideoDescription.setText(R.string.common_video_capture_closed);
        }
    }

    private void onAudioStatusChange(NetworkInfoState.Status audioStatus) {
        switch (audioStatus) {
            case Normal:
                mImageAudioStatus.setImageResource(R.drawable.network_info_audio_status_normal);
                mTextAudioStatus.setText(R.string.common_normal);
                return;
            case Abnormal:
                mImageAudioStatus.setImageResource(R.drawable.network_info_audio_status_abnormal);
                mTextAudioStatus.setText(R.string.common_exception);
                return;
            default:
                mImageAudioStatus.setImageResource(R.drawable.network_info_audio_status_abnormal);
                mTextAudioStatus.setText(R.string.common_close);
        }
    }

    private void onAudioQualityChange(TUIRoomDefine.AudioQuality audioQuality) {
        int resId;
        if (audioQuality == SPEECH) {
            resId = R.string.common_audio_mode_speech;
        } else if (audioQuality == DEFAULT) {
            resId = R.string.common_audio_mode_default;
        } else {
            resId = R.string.common_audio_mode_music;
        }
        mTextAudioMode.setText(resId);
    }

    private void onVolumeChange(Integer volume) {
        mSeekVolume.setProgress(volume);
        mTextVolume.setText(String.valueOf(volume));
    }

    private void onNetWorkStatusChange(TUICommonDefine.NetworkQuality networkQuality) {
        switch (networkQuality) {
            case POOR:
                mImageNetworkStatus.setImageResource(R.drawable.network_info_network_status_poor);
                mTextNetworkStatus.setText(R.string.common_exception);
                return;
            case BAD:
                mImageNetworkStatus.setImageResource(R.drawable.network_info_network_status_very_bad);
                mTextNetworkStatus.setText(R.string.common_exception);
                return;
            case VERY_BAD:
            case DOWN:
                mImageNetworkStatus.setImageResource(R.drawable.network_info_network_status_down);
                mTextNetworkStatus.setText(R.string.common_exception);
                return;
            default:
                mImageNetworkStatus.setImageResource(R.drawable.network_info_network_status_good);
                mTextNetworkStatus.setText(R.string.common_normal);
        }
    }

    private void onRTTChange(Integer rtt) {
        mTextRTT.setText(String.format("%dms", rtt));
        mTextRTT.setTextColor(rtt > 100 ? mColorAbnormal : mColorNormal);
    }

    private void onUpLossChange(Integer upLoss) {
        mTextUpLoss.setText(String.format("%d%%", upLoss));
        mTextUpLoss.setTextColor(upLoss > 100 ? mColorAbnormal : mColorNormal);
    }

    private void onDownLossChange(Integer downLoss) {
        mTextDownLoss.setText(String.format("%d%%", downLoss));
        mTextDownLoss.setTextColor(downLoss > 100 ? mColorAbnormal : mColorNormal);
    }

    private void onTakeSeatStatusChange(Boolean isTakeSeat) {
        mLayoutStreamStatus.setVisibility(isTakeSeat ? View.VISIBLE : View.GONE);
    }

    public interface OnAudioModeListener {
        void onAudioModeChecked(TUIRoomDefine.AudioQuality audioQuality);
    }

}
