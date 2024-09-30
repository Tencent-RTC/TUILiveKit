package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.battle;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.TextView;

import com.trtc.tuikit.common.ui.PopupDialog.DialogActionListener;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.BattleState;

@SuppressLint("ViewConstructor")
public class BattleCountdownView extends BasicView {

    private       TextView             mCountdownView;
    private       int                  mCountdownValue = BattleState.BATTLE_REQUEST_TIMEOUT;
    private final DialogActionListener mListener;

    public BattleCountdownView(Context context, LiveController controller, DialogActionListener listener) {
        super(context, controller);
        mListener = listener;
    }

    @Override
    protected void initView() {
        inflate(getContext(), R.layout.livekit_battle_count_down_view, this);
        mCountdownView = findViewById(R.id.tv_countdown);
        findViewById(R.id.tv_cancel).setOnClickListener(view -> onDismiss());
        startCountdown();
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    private void startCountdown() {
        post(new Runnable() {
            @Override
            public void run() {
                mCountdownView.setText(String.valueOf(mCountdownValue));
                mCountdownValue--;
                if (mCountdownValue < 0) {
                    onDismiss();
                    return;
                }
                postDelayed(this, 1000);
            }
        });
    }

    private void onDismiss() {
        if (mListener != null) {
            mListener.dismiss();
        }
    }
}
