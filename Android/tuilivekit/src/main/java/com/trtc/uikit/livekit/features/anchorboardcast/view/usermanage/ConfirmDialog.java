package com.trtc.uikit.livekit.features.anchorboardcast.view.usermanage;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.R;

public class ConfirmDialog extends Dialog {
    private String               mContentText;
    private String               mPositiveText;
    private View.OnClickListener mPositiveClickListener;

    public ConfirmDialog(@NonNull Context context) {
        super(context, R.style.LiveKitConfirmDialogTheme);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.livekit_alert_confirm_dialog);
        setCancelable(false);
        initText();
        initButtonPositive();
        initButtonNegative();
    }

    private void initText() {
        TextView message = findViewById(R.id.content);
        message.setText(mContentText);
    }

    private void initButtonPositive() {
        Button buttonPositive = findViewById(R.id.btn_positive);
        if (!TextUtils.isEmpty(mPositiveText)) {
            buttonPositive.setText(mPositiveText);
        }
        buttonPositive.setOnClickListener(v -> {
            if (mPositiveClickListener != null) {
                mPositiveClickListener.onClick(v);
            }
            dismiss();
        });
    }

    private void initButtonNegative() {
        Button buttonNegative = findViewById(R.id.btn_negative);
        buttonNegative.setOnClickListener(v -> dismiss());
    }

    public void setContent(String content) {
        mContentText = content;
    }

    public void setPositiveText(String text) {
        mPositiveText = text;
    }

    public void setPositiveListener(View.OnClickListener listener) {
        mPositiveClickListener = listener;
    }
}
