package com.trtc.uikit.livekit.component.beauty.tebeauty;

import android.content.Context;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

public class TEBeautyView extends FrameLayout {

    public TEBeautyView(@NonNull Context context) {
        super(context);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        init();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        unInit();
    }

    private void init() {
        TEBeautyManager.getInstance().setListener(new TEBeautyManager.OnBeautyListener() {
            @Override
            public void onCreateBeautyView(View view) {
                if (view != null) {
                    removeAllViews();
                    addView(view);
                }
            }

            @Override
            public void onDestroyBeautyView() {
                removeAllViews();
            }
        });
        TEBeautyManager.getInstance().init(getContext());
    }

    private void unInit() {
        TEBeautyManager.getInstance().setListener(null);
    }
}
