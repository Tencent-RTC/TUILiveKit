package com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost;

import android.app.Dialog;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;

import java.util.Objects;

public class StandardDialog extends Dialog {
    private String               mAvatarUrl;
    private String               mContent;
    private String               mPositiveText;
    private String               mNegativeText;
    private int                  mPositiveTextColor;
    private View.OnClickListener mPositiveClickListener;
    private View.OnClickListener mNegativeClickListener;

    public StandardDialog(@NonNull Context context) {
        super(context);
        Objects.requireNonNull(getWindow()).setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
        mPositiveTextColor = context.getResources().getColor(R.color.common_design_standard_b1);
    }

    public void setContent(String content) {
        mContent = content;
    }

    public void setAvatar(String avatarUrl) {
        mAvatarUrl = avatarUrl;
    }

    public void setPositiveText(String positiveText, View.OnClickListener listener) {
        mPositiveText = positiveText;
        mPositiveClickListener = listener;
    }

    public void setNegativeText(String negativeText, View.OnClickListener listener) {
        mNegativeText = negativeText;
        mNegativeClickListener = listener;
    }

    public void setPositiveTextColor(int color) {
        mPositiveTextColor = color;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.livekit_connection_dialog);
        setCancelable(false);

        initText();
        initAvatarUrl();
        initButtonPositive();
        initButtonNegative();
    }

    private void initText() {
        TextView textContent = findViewById(R.id.tv_content);
        if (mContent == null) {
            textContent.setVisibility(View.GONE);
        } else {
            textContent.setText(mContent);
            textContent.setVisibility(View.VISIBLE);
        }
    }

    private void initAvatarUrl() {
        ImageView imageAvatar = findViewById(R.id.iv_picture);
        if (TextUtils.isEmpty(mAvatarUrl)) {
            imageAvatar.setVisibility(View.GONE);
        } else {
            ImageLoader.load(getContext(), imageAvatar, mAvatarUrl, R.drawable.livekit_ic_avatar);
            imageAvatar.setVisibility(View.VISIBLE);
        }
    }

    private void initButtonPositive() {
        Button buttonPositive = findViewById(com.trtc.tuikit.common.R.id.btn_positive);

        if (mPositiveClickListener == null) {
            buttonPositive.setVisibility(View.GONE);
            return;
        }
        if (!TextUtils.isEmpty(mPositiveText)) {
            buttonPositive.setText(mPositiveText);
        }
        buttonPositive.setTextColor(mPositiveTextColor);
        buttonPositive.setOnClickListener(mPositiveClickListener);
    }

    private void initButtonNegative() {
        Button buttonNegative = findViewById(com.trtc.tuikit.common.R.id.btn_negative);

        if (mNegativeClickListener == null) {
            buttonNegative.setVisibility(View.GONE);
            return;
        }
        if (!TextUtils.isEmpty(mNegativeText)) {
            buttonNegative.setText(mNegativeText);
        }
        buttonNegative.setOnClickListener(mNegativeClickListener);
    }

}