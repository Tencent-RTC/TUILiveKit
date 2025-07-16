package com.trtc.uikit.livekit.voiceroom.view.basic;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

public class ExitSeatDialog  extends PopupDialog {

    private final OnConfirmListener mOnConfirmListener;

    public ExitSeatDialog(@NonNull Context context, OnConfirmListener listener) {
        super(context);
        mOnConfirmListener = listener;
        initView();
    }

    protected void initView() {
        View view = View.inflate(getContext(), R.layout.livekit_voiceroom_exit_seat_dialog, null);
        setView(view);
        TextView textCancel = view.findViewById(R.id.tv_cancel);
        textCancel.setOnClickListener(v -> dismiss());

        TextView textExitLive = view.findViewById(R.id.tv_exit_room);
        textExitLive.setOnClickListener(v -> {
            dismiss();
            mOnConfirmListener.onExitRoom();
        });

        TextView textExitSeat = view.findViewById(R.id.tv_exit_seat);
        textExitSeat.setOnClickListener(v -> {
            dismiss();
            mOnConfirmListener.onExitSeat();
        });
    }

    public interface OnConfirmListener {
        void onExitRoom();

        void onExitSeat();
    }
}

