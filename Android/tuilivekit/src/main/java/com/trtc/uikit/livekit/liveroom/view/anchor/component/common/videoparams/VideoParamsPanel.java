package com.trtc.uikit.livekit.liveroom.view.anchor.component.common.videoparams;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

import java.util.Arrays;

@SuppressLint("ViewConstructor")
public class VideoParamsPanel extends LinearLayout {

    private       TextView                             mTextResolution;
    private final LiveRoomInfo                         mLiveRoomInfo;
    private final Context                              mContext;
    private final RoomEngineService                    mRoomEngineService;
    private final PopupDialog.DialogActionListener     mDismissListener;
    private final Observer<TUIRoomDefine.VideoQuality> mResolutionObserver = value -> {
        mTextResolution.setText(getResolutionShowText(value));
    };


    public VideoParamsPanel(Context context, LiveRoomInfo roomInfo, RoomEngineService service,
                            PopupDialog.DialogActionListener listener) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mDismissListener = listener;
        mRoomEngineService = service;
        init();
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
    }

    private void addObserver() {
        mLiveRoomInfo.anchorInfo.videoInfo.videoQuality.observe(mResolutionObserver);
    }

    private void removeObserver() {
        mLiveRoomInfo.anchorInfo.videoInfo.videoQuality.removeObserver(mResolutionObserver);
    }

    private void init() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_video_params_panel, this,
                true);

        mTextResolution = findViewById(R.id.tv_resolution);

        mTextResolution.setText(getResolutionShowText(mLiveRoomInfo.anchorInfo.videoInfo.videoQuality.get()));
        mTextResolution.setOnClickListener(view -> {
            VideoResolutionPicker picker = new VideoResolutionPicker(mContext, mLiveRoomInfo, mRoomEngineService);
            picker.show();
        });

        findViewById(R.id.iv_back).setOnClickListener(view -> {
            if (mDismissListener != null) {
                mDismissListener.dismiss();
            }
        });
    }

    private String getResolutionShowText(TUIRoomDefine.VideoQuality videoQuality) {
        return Arrays.asList(mContext.getResources().getStringArray(R.array.livekit_video_resolution))
                .get(videoQuality.getValue() - 1);
    }
}
