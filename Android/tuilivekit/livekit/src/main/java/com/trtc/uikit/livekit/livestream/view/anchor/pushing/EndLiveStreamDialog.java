package com.trtc.uikit.livekit.livestream.view.anchor.pushing;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class EndLiveStreamDialog extends PopupDialog {

    protected     Context           mContext;
    private       TextView          mTextTips;
    private       TextView          mTextDisconnect;
    private final LiveCoreView      mLiveStream;
    private final LiveStreamManager mLiveStreamManager;
    private       LinearLayout      mLayoutPanel;
    private       TextView          mTextEndLive;
    private       TextView          mTextCancel;

    public EndLiveStreamDialog(@NonNull Context context, LiveCoreView liveStream,
                               LiveStreamManager liveStreamManager) {
        super(context);
        mContext = context;
        mLiveStream = liveStream;
        mLiveStreamManager = liveStreamManager;
        initView();
    }

    @SuppressLint("InflateParams")
    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_end_live_panel, null);

        bindViewId(view);
        initEndLiveButton();
        initCancelButton();
        initEndLiveExpandedView();
        setView(view);
    }

    private void bindViewId(View view) {
        mTextTips = view.findViewById(R.id.tv_tips);
        mTextDisconnect = view.findViewById(R.id.tv_disconnect);
        mLayoutPanel = view.findViewById(R.id.ll_options_panel);
        mTextEndLive = view.findViewById(R.id.tv_end_live);
        mTextCancel = view.findViewById(R.id.tv_cancel);
    }

    public void initEndLiveExpandedView() {
        boolean isConnected = !mLiveStreamManager.getCoHostState().connectedUsers.get().isEmpty();
        boolean isBattled = Boolean.TRUE.equals(mLiveStreamManager.getBattleState().mIsBattleRunning.get());
        setExpandedViewVisibility(isConnected);

        if (isBattled) {
            String tips = getContext().getString(R.string.livekit_end_pk_tips);
            String disconnectText = getContext().getString(R.string.livekit_end_pk);
            mTextTips.setText(tips);
            mTextDisconnect.setText(disconnectText);
            mTextDisconnect.setOnClickListener(v -> {
                String battleId = mLiveStreamManager.getBattleState().mBattleId;
                mLiveStream.terminateBattle(battleId, new TUIRoomDefine.ActionCallback() {
                    @Override
                    public void onSuccess() {
                        mLiveStreamManager.getBattleManager().onExitBattle();
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String s) {

                    }
                });
                dismiss();
            });
        } else if (isConnected) {
            String tips = getContext().getString(R.string.livekit_end_connection_tips);
            String disconnectText = getContext().getString(R.string.livekit_end_connection);
            mTextTips.setText(tips);
            mTextDisconnect.setText(disconnectText);
            mTextDisconnect.setOnClickListener(v -> {
                mLiveStream.terminateCrossRoomConnection();
                mLiveStreamManager.getCoHostManager().cleanConnectedUsers();
                dismiss();
            });
        }
    }

    private void setExpandedViewVisibility(boolean isConnected) {
        if (isConnected) {
            mLayoutPanel.setVisibility(VISIBLE);
        } else {
            mLayoutPanel.setVisibility(GONE);
        }
    }

    private void initEndLiveButton() {
        mTextEndLive.setText(getContext().getString(R.string.livekit_end_live));
        mTextEndLive.setOnClickListener(v -> {
            dismiss();
            endLiveRoom();
        });
    }

    private void initCancelButton() {
        mTextCancel.setText(getContext().getString(R.string.livekit_cancel));
        mTextCancel.setOnClickListener(v -> dismiss());
    }

    private void endLiveRoom() {
        mLiveStream.stopLiveStream(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mLiveStreamManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.DASHBOARD);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {

            }
        });
    }
}
