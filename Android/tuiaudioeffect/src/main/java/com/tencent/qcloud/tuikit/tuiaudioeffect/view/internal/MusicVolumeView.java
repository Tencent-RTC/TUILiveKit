package com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.SeekBar;

import com.tencent.qcloud.tuikit.tuiaudioeffect.R;

/**
 * 音乐音量滑动条
 * 取值范围：[0, 100]
 */
public class MusicVolumeView extends BaseSeekView {

    public MusicVolumeView(Context context) {
        super(context);
        init();
    }

    public MusicVolumeView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        setTitle(getResources().getString(R.string.tuiaudioeffect_panel_music_volume));
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
