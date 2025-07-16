package com.trtc.uikit.livekit.component.networkInfo.view;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.R;

public class NetworkBadTipsDialog extends Dialog {
    private static final long      NETWORK_BAD_TIPS_DURATION = 5000;
    private final        Context   mContext;
    private              ImageView mImageClose;
    private              TextView  mTextSwitchNetwork;

    public NetworkBadTipsDialog(@NonNull Context context) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        mContext = context;

        initView();
    }

    private void initView() {
        View view = LayoutInflater.from(mContext).inflate(R.layout.network_info_network_bad_tips_view, null);

        bindViewId(view);
        initCloseView();
        initSwitchNetworkView();
        setContentView(view);
    }

    private void bindViewId(View view) {
        mTextSwitchNetwork = view.findViewById(R.id.tv_switch_network);
        mImageClose = view.findViewById(R.id.iv_close);
    }

    private void initCloseView() {
        mImageClose.setOnClickListener(v -> dismiss());
    }

    private void initSwitchNetworkView() {
        mTextSwitchNetwork.setOnClickListener(v -> {
            Intent intent = new Intent(android.provider.Settings.ACTION_WIFI_SETTINGS);
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            mContext.startActivity(intent);
            dismiss();
        });
    }

    @Override
    public void show() {
        super.show();
        setAutoDismiss();
    }

    private void setAutoDismiss() {
        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            if (isShowing()) {
                dismiss();
            }
        }, NETWORK_BAD_TIPS_DURATION);
    }
}