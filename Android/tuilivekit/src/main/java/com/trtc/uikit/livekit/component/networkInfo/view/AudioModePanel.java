package com.trtc.uikit.livekit.component.networkInfo.view;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.DEFAULT;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.MUSIC;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality.SPEECH;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

public class AudioModePanel extends PopupDialog {
    private final Context                              mContext;
    private       TextView                             mTextDefault;
    private       TextView                             mTextSpeech;
    private       TextView                             mTextMusic;
    private       TextView                             mTextCancel;
    private       NetworkInfoPanel.OnAudioModeListener mAudioModeListener;

    public AudioModePanel(@NonNull Context context) {
        super(context);
        mContext = context;
        initView();
    }

    private void initView() {
        View view = LayoutInflater.from(mContext).inflate(R.layout.network_info_audio_mode_panel, null);
        bindViewId(view);
        setClickListener();

        setView(view);
    }

    public void setAudioModeListener(NetworkInfoPanel.OnAudioModeListener audioModeListener) {
        mAudioModeListener = audioModeListener;
    }

    private void bindViewId(View view) {
        mTextDefault = view.findViewById(R.id.tv_default);
        mTextSpeech = view.findViewById(R.id.tv_speech);
        mTextMusic = view.findViewById(R.id.tv_music);
        mTextCancel = view.findViewById(R.id.tv_cancel);
    }

    private void setClickListener() {
        View.OnClickListener listener = v -> {
            int id = v.getId();
            if (id == R.id.tv_default) {
                mAudioModeListener.onAudioModeChecked(DEFAULT);
            } else if (id == R.id.tv_speech) {
                mAudioModeListener.onAudioModeChecked(SPEECH);
            } else if (id == R.id.tv_music) {
                mAudioModeListener.onAudioModeChecked(MUSIC);
            }
            hide();
        };
        mTextDefault.setOnClickListener(listener);
        mTextSpeech.setOnClickListener(listener);
        mTextMusic.setOnClickListener(listener);
        mTextCancel.setOnClickListener(listener);
    }

}
