package com.trtc.uikit.livekit.view.voiceroom.view;

import static android.view.View.GONE;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;

public class ConfirmDialog extends Dialog {
    private Context              mContext;
    private View                 mDivideLine;
    private String               mHeadIconUrl;
    private String               mContentText;
    private String               mPositiveText;
    private String               mNegativeText;
    private View.OnClickListener mPositiveClickListener;
    private View.OnClickListener mNegativeClickListener;

    public ConfirmDialog(@NonNull Context context) {
        super(context, R.style.LiveKitConfirmDialogTheme);
        mContext = context;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.livekit_dialog_confirm);
        setCancelable(false);
        mDivideLine = findViewById(R.id.vertical_divider);
        initHeadIcon();
        initText();
        initButtonPositive();
        initButtonNegative();
    }

    private void initText() {
        TextView tvContent = findViewById(R.id.content);
        tvContent.setText(mContentText);
    }

    private void initHeadIcon() {
        ImageFilterView imageHead = findViewById(R.id.iv_head);
        if (TextUtils.isEmpty(mHeadIconUrl)) {
            imageHead.setVisibility(GONE);
        } else {
            ImageLoader.load(mContext, imageHead, mHeadIconUrl, R.drawable.livekit_ic_avatar);
        }
    }

    private void initButtonPositive() {
        Button buttonPositive = findViewById(R.id.btn_positive);
        if (mPositiveClickListener == null) {
            buttonPositive.setVisibility(GONE);
            mDivideLine.setVisibility(GONE);
            return;
        }
        if (!TextUtils.isEmpty(mPositiveText)) {
            buttonPositive.setText(mPositiveText);
        }
        buttonPositive.setOnClickListener(v -> {
            if (mPositiveClickListener != null) {
                mPositiveClickListener.onClick(v);
                dismiss();
            }
        });
    }

    private void initButtonNegative() {
        Button buttonNegative = findViewById(R.id.btn_negative);
        if (mNegativeClickListener == null) {
            buttonNegative.setVisibility(GONE);
            mDivideLine.setVisibility(GONE);
            return;
        }
        if (!TextUtils.isEmpty(mNegativeText)) {
            buttonNegative.setText(mNegativeText);
        }
        buttonNegative.setOnClickListener(v -> {
            if (mNegativeClickListener != null) {
                mNegativeClickListener.onClick(v);
                dismiss();
            }
        });
    }

    public void setHeadIconUrl(String url) {
        mHeadIconUrl = url;
    }

    public void setContent(String content) {
        mContentText = content;
    }

    public void setPositiveText(String text, View.OnClickListener listener) {
        mPositiveText = text;
        mPositiveClickListener = listener;
    }

    public void setNegativeText(String text, View.OnClickListener listener) {
        mNegativeText = text;
        mNegativeClickListener = listener;
    }
}