package com.tencent.qcloud.tuikit.tuiaudioeffect.view.internal;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.SeekBar;
import android.widget.TextView;

import com.tencent.qcloud.tuikit.tuiaudioeffect.R;

/**
 * 滑动条View基类
 */
public class BaseSeekView extends LinearLayout {

    private SeekBar  mSeekBar;   // 滑动条
    private TextView mTextTitle; // 标题
    private TextView mTextValue; // 当前取值

    public BaseSeekView(Context context) {
        super(context);
        init();
    }

    public BaseSeekView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        LayoutInflater.from(getContext()).inflate(R.layout.tuiaudioeffect_seek, this);
        mSeekBar = findViewById(R.id.seek_bar);
        mTextTitle = findViewById(R.id.seek_title);
        mTextValue = findViewById(R.id.seek_value);
    }

    public void setTitle(String title) {
        mTextTitle.setText(title);
    }

    public void setText(String value) {
        mTextValue.setText(value);
    }

    public void setProgress(int progress) {
        mSeekBar.setProgress(progress);
    }

    public int getProgress() {
        return mSeekBar.getProgress();
    }

    public void setOnSeekBarChangeListener(final SeekBar.OnSeekBarChangeListener listener) {
        mSeekBar.setOnSeekBarChangeListener(listener);
    }

    public abstract static class AbsOnSeekBarChangeListener implements SeekBar.OnSeekBarChangeListener {
        @Override
        public abstract void onProgressChanged(SeekBar seekBar, int i, boolean b);

        @Override
        public void onStartTrackingTouch(SeekBar seekBar) {

        }

        @Override
        public void onStopTrackingTouch(SeekBar seekBar) {

        }
    }

}
