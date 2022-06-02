package com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.SeekBar;

import com.tencent.qcloud.tuikit.tuiaudioeffect.R;

/**
 * 人声音量滑动条
 */
public class VoiceVolumeView extends BaseSeekView {

    public VoiceVolumeView(Context context) {
        super(context);
        init();
    }

    public VoiceVolumeView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        setTitle(getResources().getString(R.string.tuiaudioeffect_panel_mic_volume));
        setText("100");
        setProgress(100);
    }

    @Override
    public void setOnSeekBarChangeListener(final SeekBar.OnSeekBarChangeListener listener) {
        super.setOnSeekBarChangeListener(new AbsOnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean b) {
                setText(String.valueOf(progress));
                if (null != listener) {
                    listener.onProgressChanged(seekBar, progress, b);
                }
            }
        });
    }

    public int getVolume() {
        return getProgress();
    }
}
