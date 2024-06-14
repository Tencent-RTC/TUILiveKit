package com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.core.content.ContextCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

@SuppressLint("ViewConstructor")
public class ActionConfirmationPanel extends LinearLayout {
    private PopupDialog.DialogActionListener mDialogActionListener;

    public ActionConfirmationPanel(Context context, String actionText, OnClickListener listener) {
        super(context);
        inflate(context, R.layout.livekit_layout_action_confirmation_panel, this);
        setOrientation(VERTICAL);
        setBackground(ContextCompat.getDrawable(context, R.drawable.livekit_dialog_background));

        TextView textAction = findViewById(R.id.text_action);
        textAction.setText(actionText);
        textAction.setOnClickListener(listener);

        findViewById(R.id.text_cancel).setOnClickListener(view -> {
            if (mDialogActionListener != null) {
                mDialogActionListener.dismiss();
            }
        });
    }

    public void setDialogActionListener(PopupDialog.DialogActionListener listener) {
        mDialogActionListener = listener;
    }
}
