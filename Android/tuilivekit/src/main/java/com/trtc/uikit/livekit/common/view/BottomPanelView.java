package com.trtc.uikit.livekit.common.view;

import android.content.Context;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.DrawableRes;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;

public abstract class BottomPanelView extends BasicView {
    protected OnDismissListener mOnDismissListener;

    public BottomPanelView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    protected void setTitle(String title) {
        TextView tvTitle = findViewById(R.id.tv_title);
        if (tvTitle != null) {
            tvTitle.setText(title);
        }
    }

    protected void showBackButton() {
        ImageView imageBack = findViewById(R.id.iv_back);
        if (imageBack != null) {
            imageBack.setOnClickListener(view -> dismiss());
            imageBack.setVisibility(VISIBLE);
        }
    }

    protected void showEndButton(@DrawableRes int resId, OnClickListener listener) {
        ImageView endButton = findViewById(R.id.end_button);
        View endButtonContainer = findViewById(R.id.end_button_container);
        if (endButton != null) {
            endButton.setImageResource(resId);
        }
        if (endButtonContainer != null) {
            endButtonContainer.setVisibility(VISIBLE);
            endButtonContainer.setOnClickListener(listener);
        }
    }

    protected void dismiss() {
        if (mOnDismissListener != null) {
            mOnDismissListener.onDismiss();
        }
    }

    public interface OnDismissListener {
        void onDismiss();
    }

    public void setOnDismissListener(OnDismissListener listener) {
        mOnDismissListener = listener;
    }
}
