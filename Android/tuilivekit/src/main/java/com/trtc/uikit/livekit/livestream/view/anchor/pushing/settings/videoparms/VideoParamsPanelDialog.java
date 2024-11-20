package com.trtc.uikit.livekit.livestream.view.anchor.pushing.settings.videoparms;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;

import java.util.Arrays;

@SuppressLint("ViewConstructor")
public class VideoParamsPanelDialog extends PopupDialog {
    private       TextView                             mTextResolution;
    private       ImageView                            mImageBack;
    private final LiveStreamManager                    mLiveManager;
    private final Observer<TUIRoomDefine.VideoQuality> mResolutionObserver = this::onVideoQualityChanged;

    public VideoParamsPanelDialog(Context context, LiveStreamManager manager) {
        super(context);
        mLiveManager = manager;
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_video_params_panel, null);
        bindViewId(view);
        initTextResolutionView();
        initBackView();
        addObserver();
        setView(view);
    }

    protected void addObserver() {
        mLiveManager.getMediaState().videoQuality.observe(mResolutionObserver);
    }

    protected void removeObserver() {
        mLiveManager.getMediaState().videoQuality.removeObserver(mResolutionObserver);
    }

    private void initBackView() {
        mImageBack.setOnClickListener(view -> {
            removeObserver();
            dismiss();
        });
    }

    private void initTextResolutionView() {
        mTextResolution.setText(getResolutionShowText(mLiveManager.getMediaState().videoQuality.get()));
        mTextResolution.setOnClickListener(view -> {
            VideoResolutionPicker picker = new VideoResolutionPicker(getContext(), mLiveManager);
            picker.show();
        });
    }

    private void bindViewId(View view) {
        mTextResolution = view.findViewById(R.id.tv_resolution);
        mImageBack = view.findViewById(R.id.iv_back);
    }

    private String getResolutionShowText(TUIRoomDefine.VideoQuality videoQuality) {
        return Arrays.asList(getContext().getResources().getStringArray(R.array.livekit_video_resolution))
                .get(videoQuality.getValue() - 1);
    }

    private void onVideoQualityChanged(TUIRoomDefine.VideoQuality videoQuality) {
        mTextResolution.setText(getResolutionShowText(videoQuality));
    }

}
