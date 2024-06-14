package com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming.link;

import static com.trtc.uikit.livekit.state.LiveDefine.LinkType.AUDIO;
import static com.trtc.uikit.livekit.state.LiveDefine.LinkType.VIDEO;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;

@SuppressLint("ViewConstructor")
public class SelectLinkMicTypePanel extends BasicView {
    private static final int SEAT_INDEX = -1;

    private ImageView                        mImageLinkSettings;
    private ConstraintLayout                 mLayoutLinkVideo;
    private ConstraintLayout                 mLayoutLinkAudio;
    private PopupDialog.DialogActionListener mListener;

    public SelectLinkMicTypePanel(@NonNull Context context, LiveController controller) {
        super(context, controller);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_dialog_link_mic_selector, this, true);
        bindViewId();

        initLinkSettingsView();
        initLinkVideoView();
        initLinkAudioView();
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    private void bindViewId() {
        mImageLinkSettings = findViewById(R.id.iv_link_settings);
        mLayoutLinkVideo = findViewById(R.id.cl_link_video);
        mLayoutLinkAudio = findViewById(R.id.cl_link_audio);
    }

    private void initLinkAudioView() {
        mLayoutLinkAudio.setOnClickListener(view -> {
            applyLinkMic(AUDIO);
        });
    }

    private void initLinkVideoView() {
        mLayoutLinkVideo.setOnClickListener(view -> {
            applyLinkMic(VIDEO);
        });
    }

    private void initLinkSettingsView() {
        mImageLinkSettings.setOnClickListener(view -> {
            PopupDialog videoLinkDialog = new PopupDialog(mContext);
            VideoLinkSettingsPanel videoLinkPanel = new VideoLinkSettingsPanel(mContext, mLiveController);
            videoLinkPanel.setDialogActionListener(videoLinkDialog::dismiss);
            videoLinkDialog.setView(videoLinkPanel);
            videoLinkDialog.show();
            mListener.dismiss();
        });
    }

    private void applyLinkMic(LiveDefine.LinkType type) {
        mViewState.linkType.set(type);
        ToastUtil.toastShortMessageCenter(mContext.getString(R.string.livekit_toast_apply_link_mic));
        mSeatController.takeSeat(SEAT_INDEX);
        mListener.dismiss();
    }

    public void setDialogActionListener(PopupDialog.DialogActionListener listener) {
        mListener = listener;
    }
}