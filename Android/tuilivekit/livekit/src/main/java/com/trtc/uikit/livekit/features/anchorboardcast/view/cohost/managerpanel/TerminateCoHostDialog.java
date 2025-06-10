package com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.managerpanel;

import android.app.Dialog;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;

import java.util.Objects;

public class TerminateCoHostDialog extends Dialog {

    private AnchorManager mManager;

    public TerminateCoHostDialog(@NonNull Context context, AnchorManager manager) {
        super(context);
        mManager = manager;
        Objects.requireNonNull(getWindow()).setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
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
        textContent.setText(getContext().getString(R.string.common_disconnect_tips));
        textContent.setVisibility(View.VISIBLE);
    }

    private void initAvatarUrl() {
        ImageView imageAvatar = findViewById(R.id.iv_picture);
        imageAvatar.setVisibility(View.GONE);
    }

    private void initButtonPositive() {
        Button buttonPositive = findViewById(com.trtc.tuikit.common.R.id.btn_positive);
        buttonPositive.setText(getContext().getString(R.string.common_end_connect));
        buttonPositive.setTextColor(getContext().getResources().getColor(R.color.common_design_standard_b1));
        buttonPositive.setOnClickListener(v -> {
            dismiss();
            mManager.terminateCrossRoomConnection();
        });
    }

    private void initButtonNegative() {
        Button buttonNegative = findViewById(com.trtc.tuikit.common.R.id.btn_negative);
        buttonNegative.setText(getContext().getString(R.string.common_disconnect_cancel));
        buttonNegative.setOnClickListener(v -> {
            dismiss();
        });
    }

}