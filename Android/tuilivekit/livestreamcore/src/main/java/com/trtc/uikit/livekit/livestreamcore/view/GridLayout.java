package com.trtc.uikit.livekit.livestreamcore.view;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.livestreamcore.R;

public class GridLayout extends FreeLayout {

    public GridLayout(@NonNull Context context) {
        this(context, null);
    }

    public GridLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public GridLayout(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        setLayoutResource(R.raw.livestreamcore_video_layout_grid);
    }
}
