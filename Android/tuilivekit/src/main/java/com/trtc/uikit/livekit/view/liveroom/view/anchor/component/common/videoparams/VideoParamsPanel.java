package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common.videoparams;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.TextView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

import java.util.Arrays;

@SuppressLint("ViewConstructor")
public class VideoParamsPanel extends BasicView {

    private       TextView                             mTextResolution;
    private final PopupDialog.DialogActionListener     mDismissListener;
    private final Observer<TUIRoomDefine.VideoQuality> mResolutionObserver = this::onVideoQualityChanged;

    public VideoParamsPanel(Context context, LiveController liveController, PopupDialog.DialogActionListener listener) {
        super(context, liveController);
        mDismissListener = listener;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_video_params_panel, this, true);
        bindViewId();

        initTextResolutionView();
        initBackView();
    }

    @Override
    protected void addObserver() {
        mMediaState.videoQuality.observe(mResolutionObserver);
    }

    @Override
    protected void removeObserver() {
        mMediaState.videoQuality.removeObserver(mResolutionObserver);
    }

    private void initBackView() {
        findViewById(R.id.iv_back).setOnClickListener(view -> {
            if (mDismissListener != null) {
                mDismissListener.dismiss();
            }
        });
    }

    private void initTextResolutionView() {
        mTextResolution.setText(getResolutionShowText(mMediaState.videoQuality.get()));
        mTextResolution.setOnClickListener(view -> {
            VideoResolutionPicker picker = new VideoResolutionPicker(mContext, mLiveController);
            picker.show();
        });
    }

    private void bindViewId() {
        mTextResolution = findViewById(R.id.tv_resolution);
    }

    private String getResolutionShowText(TUIRoomDefine.VideoQuality videoQuality) {
        return Arrays.asList(mContext.getResources().getStringArray(R.array.livekit_video_resolution))
                .get(videoQuality.getValue() - 1);
    }

    private void onVideoQualityChanged(TUIRoomDefine.VideoQuality videoQuality) {
        mTextResolution.setText(getResolutionShowText(videoQuality));
    }

}
