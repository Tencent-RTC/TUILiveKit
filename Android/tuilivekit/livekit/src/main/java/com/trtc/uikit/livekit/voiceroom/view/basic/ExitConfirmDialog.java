package com.trtc.uikit.livekit.voiceroom.view.basic;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

public class ExitConfirmDialog extends PopupDialog {

    private final OnConfirmListener mOnConfirmListener;

    public ExitConfirmDialog(@NonNull Context context, OnConfirmListener listener) {
        super(context);
        mOnConfirmListener = listener;
        initView();
    }

    protected void initView() {
        View view = View.inflate(getContext(), R.layout.livekit_voiceroom_exit_confirm, null);
        setView(view);
        TextView textCancel = view.findViewById(R.id.tv_cancel);
        textCancel.setText(getContext().getString(R.string.livekit_cancel));
        textCancel.setOnClickListener(v -> dismiss());

        TextView confirmText = view.findViewById(R.id.confirm_text);
        confirmText.setText(getContext().getString(R.string.livekit_end_live));
        confirmText.setOnClickListener(v -> {
            dismiss();
            mOnConfirmListener.onConfirm();
        });
    }

    public interface OnConfirmListener {
        void onConfirm();
    }
}
