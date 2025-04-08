package com.trtc.uikit.livekit.livestream.view.anchor.floatwindow;

import android.annotation.SuppressLint;
import android.content.Context;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;

import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.state.BattleState.BattleUser;
import com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionUser;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class FloatViewAnchorView extends BasicView {

    private final Observer<ConnectionUser> mReceivedConnectRequestObserver = this::onReceivedConnectRequestChange;
    private final Observer<BattleUser>     mReceivedBattleRequestObserver  = this::onReceivedBattleRequestChange;

    public FloatViewAnchorView(@NonNull Context context, LiveCoreView coreView) {
        super(context);
        addView(coreView);
    }

    @Override
    protected void initView() {

    }

    @Override
    protected void refreshView() {

    }

    @Override
    protected void addObserver() {
        mCoHostState.receivedConnectionRequest.observeForever(mReceivedConnectRequestObserver);
        mBattleState.mReceivedBattleRequest.observeForever(mReceivedBattleRequestObserver);
    }

    @Override
    protected void removeObserver() {
        mCoHostState.receivedConnectionRequest.removeObserver(mReceivedConnectRequestObserver);
        mBattleState.mReceivedBattleRequest.removeObserver(mReceivedBattleRequestObserver);
    }

    private void onReceivedConnectRequestChange(ConnectionUser receivedConnectionRequest) {
        if (receivedConnectionRequest != null) {
            String resId = getResources().getString(R.string.common_float_window_received_connection_invitation);
            ToastUtil.toastShortMessage(resId);
        }
    }

    private void onReceivedBattleRequestChange(BattleUser user) {
        if (user != null) {
            String resId = getResources().getString(R.string.common_float_window_received_battle_invitation);
            ToastUtil.toastShortMessage(resId);
        }
    }
}
