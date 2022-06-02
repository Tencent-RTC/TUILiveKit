package com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.SeekBar;

import com.tencent.qcloud.tuikit.tuiaudioeffect.R;

/**
 * 声音升降调滑动条
 * 取值范围：[-1, 1]
 */
public class VoicePitchView extends BaseSeekView {

    private float mVoicePitch = 0;

    public VoicePitchView(Context context) {
        super(context);
        init();
    }

    public VoicePitchView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        setTitle(getResources().getString(R.string.tuiaudioeffect_panel_music_pitch));
        setText("0");
        setProgress(50);
    }

    @Override
    public void setProgress(int progress) {
        super.setProgress(progress);
        mVoicePitch = progress2Pitch(progress);
    }

    @Override
    public void setOnSeekBarChangeListener(final SeekBar.OnSeekBarChangeListener listener) {
        super.setOnSeekBarChangeListener(new AbsOnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean b) {
                mVoicePitch = progress2Pitch(progress);
                setText(String.valueOf(mVoicePitch));
                if (null != listener) {
                    listener.onProgressChanged(seekBar, progress, b);
                }
            }
        });
    }

    public float getPitch() {
        return mVoicePitch;
    }

    private float progress2Pitch(int progress) {
        return ((progress - 50) / (float) 50);
    }
}
