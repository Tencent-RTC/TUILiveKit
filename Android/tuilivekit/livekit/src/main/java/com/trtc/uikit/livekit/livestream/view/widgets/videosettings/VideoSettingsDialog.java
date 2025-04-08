package com.trtc.uikit.livekit.livestream.view.widgets.videosettings;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_1080P;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_540P;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_720P;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.widget.SwitchCompat;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class VideoSettingsDialog extends PopupDialog {
    private static final int                                  BITRATE_STEP = 100;
    private final        Context                              mContext;
    private final        LiveStreamManager                    mLiveManager;
    private final        LiveCoreView                         mLiveCoreView;
    private final        BitrateRange                         mBitrateRange;
    private              SeekBar                              mSbBitrate;
    private              TextView                             mTextBitrate;
    private              TextView                             mTextVideoResolution;
    private              ConstraintLayout                     mLayoutResolution;
    private              SeekBar                              mSbFps;
    private              TextView                             mTextFps;
    private              ResolutionPickerDialog               mResolutionDialog;
    private              SwitchCompat                         mSwitchMirror;
    private final        TUIRoomDefine.RoomVideoEncoderParams mVideoEncoderParams;
    private              TextView                             mTextTitle;
    private              SwitchCompat                         mSwitchUltimate;
    private              SwitchCompat                         mSwitchHevc;


    public VideoSettingsDialog(Context context, LiveCoreView liveCoreView, LiveStreamManager manager) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        mContext = context;
        mLiveCoreView = liveCoreView;
        mLiveManager = manager;
        mBitrateRange = new BitrateRange();
        mVideoEncoderParams = manager.getMediaState().videoEncParams.getCurrentEnc();
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_settings_video_config, null);

        bindViewId(view);
        initTitle();
        initBitrateRange();
        initResolutionView();
        initBitrateView();
        initFpsView();
        initMirror();
        initUltimate();
        initHevc();
        setView(view);
    }

    private void bindViewId(View view) {
        mTextTitle = view.findViewById(R.id.tv_title);
        mTextVideoResolution = view.findViewById(R.id.tv_resolution_selector);
        mLayoutResolution = view.findViewById(R.id.cl_resolution);
        mSbBitrate = view.findViewById(R.id.sb_bitrate);
        mTextBitrate = view.findViewById(R.id.tv_bitrate);
        mSbFps = view.findViewById(R.id.sb_fps);
        mTextFps = view.findViewById(R.id.tv_fps);
        mSwitchMirror = view.findViewById(R.id.sc_mirror);
        mSwitchUltimate = view.findViewById(R.id.sc_ultimate);
        mSwitchHevc = view.findViewById(R.id.sc_hevc);
    }

    private void initMirror() {
        mSwitchMirror.setChecked(Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isMirrorEnabled.getValue()));
        mSwitchMirror.setOnClickListener(v -> mLiveCoreView.enableMirror(mSwitchMirror.isChecked()));
    }

    private void initUltimate() {
        boolean ultimateVideoVisible = mLiveManager.getMediaState().videoAdvanceSetting.isVisible;
        mSwitchUltimate.setVisibility(ultimateVideoVisible ? View.VISIBLE : View.GONE);
        mSwitchUltimate.setChecked(mLiveManager.getMediaState().videoAdvanceSetting.isUltimateEnabled);
        mSwitchUltimate.setOnCheckedChangeListener((buttonView, isChecked) -> {
            mLiveManager.getMediaManager().enableUltimate(isChecked);
        });
    }

    private void initHevc() {
        boolean hevcVisible = mLiveManager.getMediaState().videoAdvanceSetting.isVisible;
        mSwitchHevc.setVisibility(hevcVisible ? View.VISIBLE : View.GONE);
        mSwitchHevc.setChecked(mLiveManager.getMediaState().videoAdvanceSetting.isH265Enabled);
        mSwitchHevc.setOnCheckedChangeListener((buttonView, isChecked) -> {
            mLiveManager.getMediaManager().enableH265(isChecked);
        });
    }

    void initTitle() {
        mTextTitle.setOnLongClickListener(v -> {
            mLiveManager.getMediaManager().enableAdvancedVisible(true);
            mSwitchUltimate.setVisibility(View.VISIBLE);
            mSwitchHevc.setVisibility(View.VISIBLE);
            return false;
        });
    }

    void initResolutionView() {
        mLayoutResolution.setOnClickListener(v -> showResolutionDialog());
    }

    void initBitrateView() {
        int minBitrate = mBitrateRange.mMinBitrate;
        int maxBitrate = mBitrateRange.mMaxBitrate;
        int newMax = (maxBitrate - minBitrate) / VideoSettingsDialog.BITRATE_STEP;
        if (mSbBitrate.getMax() == newMax) {
            return;
        }
        mSbBitrate.setMax(newMax);
        mSbBitrate.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @SuppressLint("SetTextI18n")
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean b) {
                int bitrate = minBitrate + progress * VideoSettingsDialog.BITRATE_STEP;
                mTextBitrate.setText(bitrate + " kbps");
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {
            }

            @SuppressLint("SetTextI18n")
            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                int bitrate = minBitrate + seekBar.getProgress() * VideoSettingsDialog.BITRATE_STEP;
                mTextBitrate.setText(bitrate + " kbps");
                mVideoEncoderParams.bitrate = bitrate;
                updateVideoEncParams();
            }
        });
    }

    static class BitrateRange {
        int mMinBitrate;
        int mMaxBitrate;

        public BitrateRange() {
        }
    }

    public void initBitrateRange() {
        switch (mVideoEncoderParams.videoResolution) {
            case Q_1080P:
                mBitrateRange.mMinBitrate = 1800;
                mBitrateRange.mMaxBitrate = 4000;
                break;
            case Q_720P:
            case Q_540P:
                mBitrateRange.mMinBitrate = 900;
                mBitrateRange.mMaxBitrate = 1800;
                break;
            case Q_360P:
                mBitrateRange.mMinBitrate = 300;
                mBitrateRange.mMaxBitrate = 900;
                break;
            default:
                break;
        }
    }

    public void updateResolution(TUIRoomDefine.VideoQuality resolution) {
        mVideoEncoderParams.videoResolution = resolution;
        updateBitrateByResolution();
        updateView();
        updateVideoEncParams();
    }

    public void updateBitrateByResolution() {
        initBitrateRange();
        int currentBitrate = mVideoEncoderParams.bitrate;
        if (currentBitrate < mBitrateRange.mMinBitrate) {
            mVideoEncoderParams.bitrate = mBitrateRange.mMinBitrate;
        } else if (currentBitrate > mBitrateRange.mMaxBitrate) {
            mVideoEncoderParams.bitrate = mBitrateRange.mMaxBitrate;
        }
        initBitrateView();
    }

    @SuppressLint("SetTextI18n")
    void initFpsView() {
        mSbFps.setMax(45);
        mSbFps.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @SuppressLint("SetTextI18n")
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean b) {
                int fps = progress + 15;
                mTextFps.setText(String.valueOf(fps));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @SuppressLint("SetTextI18n")
            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                int fps = seekBar.getProgress() + 15;
                mTextFps.setText(String.valueOf(fps));
                mVideoEncoderParams.fps = fps;
                updateVideoEncParams();
            }
        });
    }

    private void updateVideoEncParams() {
        mLiveManager.getMediaManager().updateVideoQualityEx(mVideoEncoderParams);
    }

    private void showResolutionDialog() {
        if (mResolutionDialog == null) {
            mResolutionDialog = new ResolutionPickerDialog(mContext, this::updateResolution);
        }
        mResolutionDialog.show();
    }

    @Override
    public void show() {
        super.show();
        updateView();
    }

    @SuppressLint("SetTextI18n")
    public void updateView() {
        updateBitrateView();
        updateFPSView();
        updateResolutionView();
    }

    @SuppressLint("SetTextI18n")
    private void updateBitrateView() {
        int bitrate = mVideoEncoderParams.bitrate;
        mTextBitrate.setText(bitrate + " kbps");
        int bitrateProgress = (bitrate - mBitrateRange.mMinBitrate) / BITRATE_STEP;
        mSbBitrate.setProgress(bitrateProgress);
    }

    @SuppressLint("SetTextI18n")
    private void updateFPSView() {
        int fps = mVideoEncoderParams.fps;
        mTextFps.setText(String.valueOf(fps));
        int fpsProgress = fps - 15;
        mSbFps.setProgress(fpsProgress);
    }

    private void updateResolutionView() {
        TUIRoomDefine.VideoQuality videoResolution = mVideoEncoderParams.videoResolution;
        if (videoResolution == Q_1080P) {
            mTextVideoResolution.setText(mContext.getString(R.string.common_resolution_1080p));
        } else if (videoResolution == Q_720P) {
            mTextVideoResolution.setText(mContext.getString(R.string.common_resolution_720p));
        } else if (videoResolution == Q_540P) {
            mTextVideoResolution.setText(mContext.getString(R.string.common_resolution_540p));
        } else {
            mTextVideoResolution.setText(mContext.getString(R.string.common_resolution_360p));
        }
    }
}
