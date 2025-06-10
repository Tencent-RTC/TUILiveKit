package com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.managerpanel;

import android.annotation.SuppressLint;
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

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;

import java.util.Objects;

public class ConnectionRequestDialog extends Dialog {

    private AnchorManager                           mManager;
    private TUILiveConnectionManager.ConnectionUser mConnectionUser;


    public ConnectionRequestDialog(@NonNull Context context, AnchorManager manager) {
        super(context);
        mManager = manager;
        mConnectionUser = mManager.getCoreState().coHostState.receivedConnectionRequest.getValue();
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

    @SuppressLint("SetTextI18n")
    private void initText() {
        TextView textContent = findViewById(R.id.tv_content);
        textContent.setText(mConnectionUser.userName + getContext().getString(R.string.common_connect_inviting_append));
        textContent.setVisibility(View.VISIBLE);
    }

    private void initAvatarUrl() {
        ImageView imageAvatar = findViewById(R.id.iv_picture);
        if (TextUtils.isEmpty(mConnectionUser.avatarUrl)) {
            imageAvatar.setVisibility(View.GONE);
        } else {
            ImageLoader.load(getContext(), imageAvatar, mConnectionUser.avatarUrl, R.drawable.livekit_ic_avatar);
            imageAvatar.setVisibility(View.VISIBLE);
        }
    }

    private void initButtonPositive() {
        Button buttonPositive = findViewById(com.trtc.tuikit.common.R.id.btn_positive);
        buttonPositive.setText(getContext().getString(R.string.common_accept));
        buttonPositive.setTextColor(getContext().getResources().getColor(R.color.common_design_standard_b1));
        buttonPositive.setOnClickListener(v -> {
            mManager.respondToCrossRoomConnection(mConnectionUser.roomId, true, null);
            dismiss();
        });
    }

    private void initButtonNegative() {
        Button buttonNegative = findViewById(com.trtc.tuikit.common.R.id.btn_negative);
        buttonNegative.setText(getContext().getString(R.string.common_reject));
        buttonNegative.setOnClickListener(v -> {
            mManager.respondToCrossRoomConnection(mConnectionUser.roomId, false, null);
            dismiss();
        });
    }
}
