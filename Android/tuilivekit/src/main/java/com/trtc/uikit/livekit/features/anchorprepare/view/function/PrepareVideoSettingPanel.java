package com.trtc.uikit.livekit.features.anchorprepare.view.function;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import androidx.appcompat.widget.SwitchCompat;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.videoquality.VideoQualitySelectPanel;

import java.util.ArrayList;
import java.util.List;

import io.trtc.tuikit.atomicxcore.api.LiveCoreView;

public class PrepareVideoSettingPanel extends PopupDialog {

    private final LiveCoreView mLiveCoreView;
    private       SwitchCompat mSwitchMirror;
    private       TextView     mTextVideoQuality;

    public PrepareVideoSettingPanel(Context context, LiveCoreView coreView) {
        super(context);
        mLiveCoreView = coreView;
        initView();
    }

    private void initView() {
        View view = View.inflate(getContext(), R.layout.livekit_anchor_prepare_layout_video_setting, null);
        setView(view);
        mSwitchMirror = view.findViewById(R.id.switch_mirror);
        mTextVideoQuality = view.findViewById(R.id.tv_quality_value);
        mSwitchMirror.setChecked(Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isMirrorEnabled.getValue()));
        mSwitchMirror.setOnCheckedChangeListener((buttonView, isChecked) -> {
            mLiveCoreView.enableMirror(isChecked);
        });
        mTextVideoQuality.setOnClickListener(v -> {
            List<TUIRoomDefine.VideoQuality> videoQualityList = new ArrayList<>();
            videoQualityList.add(TUIRoomDefine.VideoQuality.Q_1080P);
            videoQualityList.add(TUIRoomDefine.VideoQuality.Q_720P);
            VideoQualitySelectPanel videoQualityPanel = new VideoQualitySelectPanel(getContext(), videoQualityList);
            videoQualityPanel.setOnVideoQualitySelectedListener(videoQuality -> {
                TUIRoomEngine.sharedInstance().updateVideoQuality(videoQuality);
                mTextVideoQuality.setText(videoQualityToString(videoQuality));
            });
            videoQualityPanel.show();
        });
    }

    private String videoQualityToString(TUIRoomDefine.VideoQuality quality) {
        switch (quality) {
            case Q_1080P:
                return "1080P";
            case Q_720P:
                return "720P";
            case Q_540P:
                return "540P";
            case Q_360P:
                return "360P";
            default:
                return "unknown";
        }
    }
}
