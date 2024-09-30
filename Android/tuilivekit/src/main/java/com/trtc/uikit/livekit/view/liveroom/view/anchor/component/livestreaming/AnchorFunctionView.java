package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.RelativeLayout;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageButton;
import com.trtc.uikit.livekit.common.uicomponent.music.view.MusicPanelView;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.BattleState;
import com.trtc.uikit.livekit.state.operation.ConnectionState.ConnectionUser;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common.SettingsPanel;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.battle.AnchorEndBattlePanel;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.battle.BattleCountdownView;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.connection.AnchorConnectionManagePanel;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.link.AnchorLinkMicManagePanel;

import java.util.ArrayList;
import java.util.List;

@SuppressLint("ViewConstructor")
public class AnchorFunctionView extends BasicView {

    private AnchorLinkMicManagePanel    mLinkMicManagePanel;
    private PopupDialog                 mAnchorLinkMicDialog;
    private AnchorConnectionManagePanel mConnectionPanel;
    private SettingsPanel               mSettingsPanel;
    private PopupDialog                 mSettingsPanelDialog;
    private PopupDialog                 mMusicPanel;
    private PopupDialog                 mAnchorConnectionDialog;
    private Dialog                      mBattleCountdownDialog;
    private View                        mViewLinkMic;
    private View                        mViewBattle;

    private final Observer<List<ConnectionUser>> mConnectedObserver          = this::onConnectedUserChange;
    private final Observer<Boolean>              mInWaitingObserver          = this::onInWaitingChange;
    private final Observer<Boolean>              mStartBattleObserver        = this::onBattleStarted;
    private final Observer<Boolean>              mOnDisplayResultObserver    = this::onDisplayResultChange;

    public AnchorFunctionView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_function, this, true);

        initBarrageSendContainer();
        initConnectionView();
        initBattleView();
        initLinkView();
        initSettingsView();
        initMusicView();
    }

    private void initBarrageSendContainer() {
        RelativeLayout mLayoutBarrageSendContainer = findViewById(R.id.rl_barrage);
        TUIBarrageButton barrageButton = new TUIBarrageButton(mContext, mRoomState.roomId, mRoomState.ownerInfo.userId);
        mLayoutBarrageSendContainer.addView(barrageButton);
    }

    @Override
    protected void addObserver() {
        mConnectionState.connectedUsers.observe(mConnectedObserver);
        mBattleState.mIsInWaiting.observe(mInWaitingObserver);
        mBattleState.mIsBattleRunning.observe(mStartBattleObserver);
        mBattleState.mIsOnDisplayResult.observe(mOnDisplayResultObserver);
    }

    @Override
    protected void removeObserver() {
        mConnectionState.connectedUsers.removeObserver(mConnectedObserver);
        mBattleState.mIsInWaiting.removeObserver(mInWaitingObserver);
        mBattleState.mIsBattleRunning.removeObserver(mStartBattleObserver);
        mBattleState.mIsOnDisplayResult.removeObserver(mOnDisplayResultObserver);
    }

    private void initConnectionView() {
        findViewById(R.id.v_connection).setOnClickListener((view) -> {
            if (mAnchorConnectionDialog == null) {
                mAnchorConnectionDialog = new PopupDialog(mContext,
                        com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
                mAnchorConnectionDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mConnectionPanel == null) {
                mConnectionPanel = new AnchorConnectionManagePanel(mContext, mLiveController,
                        () -> mAnchorConnectionDialog.dismiss());
            }
            mAnchorConnectionDialog.setView(mConnectionPanel);
            mAnchorConnectionDialog.show();
        });
    }

    private void initBattleView() {
        mViewBattle = findViewById(R.id.v_battle);
        enableView(mViewBattle, false);
        mViewBattle.setOnClickListener(view -> {
            if (Boolean.TRUE.equals(mBattleState.mIsBattleRunning.get())) {
                PopupDialog dialog = new PopupDialog(mContext,
                        com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
                AnchorEndBattlePanel panel = new AnchorEndBattlePanel(mContext, mLiveController);
                panel.setEndBattleListener(v -> dialog.dismiss());
                panel.setCancelListener(v -> dialog.dismiss());
                dialog.setView(panel);
                dialog.show();
            } else {
                List<String> list = new ArrayList<>();
                for (ConnectionUser user : mLiveController.getConnectionState().connectedUsers.get()) {
                    if (!user.userId.equals(mUserState.selfInfo.userId)) {
                        list.add(user.userId);
                    }
                }
                mLiveController.getBattleController().requestBattle(list, BattleState.BATTLE_REQUEST_TIMEOUT);
            }
        });
    }

    private void initLinkView() {
        mViewLinkMic = findViewById(R.id.v_link);
        mViewLinkMic.setOnClickListener((view) -> {
            if (mAnchorLinkMicDialog == null) {
                mAnchorLinkMicDialog = new PopupDialog(mContext);
                mAnchorLinkMicDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mLinkMicManagePanel == null) {
                mLinkMicManagePanel = new AnchorLinkMicManagePanel(mContext, mLiveController,
                        () -> mAnchorLinkMicDialog.dismiss());
            }
            mAnchorLinkMicDialog.setView(mLinkMicManagePanel);
            mAnchorLinkMicDialog.show();
        });
    }

    private void initSettingsView() {
        findViewById(R.id.v_settings).setOnClickListener((view) -> {
            if (mSettingsPanelDialog == null) {
                mSettingsPanelDialog = new PopupDialog(mContext,
                        com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
                mSettingsPanelDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mSettingsPanel == null) {
                mSettingsPanel = new SettingsPanel(mContext, mLiveController);
            }
            mSettingsPanelDialog.setView(mSettingsPanel);
            mSettingsPanelDialog.show();
        });
    }

    private void initMusicView() {
        findViewById(R.id.v_music).setOnClickListener((view) -> {
            if (mMusicPanel == null) {
                mMusicPanel = new PopupDialog(mContext);
                MusicPanelView musicListPanelView = new MusicPanelView(mContext, mRoomState.roomId,
                        mLiveController.getLiveService().getTRTCCloud());
                mMusicPanel.setView(musicListPanelView);
            }
            mMusicPanel.show();
        });
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onConnectedUserChange(List<ConnectionUser> connectedList) {
        post(() -> {
            if (mConnectionState.connectedUsers.get().isEmpty()) {
                enableView(mViewLinkMic, true);
                enableView(mViewBattle, false);
            } else {
                enableView(mViewLinkMic, false);
                enableView(mViewBattle, true);
            }
        });
    }

    private void enableView(View view, boolean enable) {
        view.setEnabled(enable);
        view.setAlpha(enable ? 1.0f : 0.5f);
    }

    private void showBattleCountdownDialog() {
        if (mBattleCountdownDialog == null) {
            mBattleCountdownDialog = new Dialog(mContext, android.R.style.Theme_Translucent_NoTitleBar);
            mBattleCountdownDialog.setCancelable(false);
            mBattleCountdownDialog.setCanceledOnTouchOutside(false);
            BattleCountdownView view = new BattleCountdownView(mContext, mLiveController,
                    () -> mLiveController.getBattleController().cancelBattleRequest());
            mBattleCountdownDialog.setContentView(view);
        }
        mBattleCountdownDialog.show();
    }

    private void dismissBattleCountdownDialog() {
        if (mBattleCountdownDialog != null) {
            mBattleCountdownDialog.dismiss();
            mBattleCountdownDialog = null;
        }
    }

    private void onInWaitingChange(Boolean inWaiting) {
        if (Boolean.TRUE.equals(inWaiting)) {
            showBattleCountdownDialog();
        } else if (Boolean.FALSE.equals(inWaiting)) {
            dismissBattleCountdownDialog();
        }
    }

    private void onBattleStarted(Boolean started) {
        if (Boolean.TRUE.equals(started)) {
            mViewBattle.setBackgroundResource(R.drawable.livekit_function_battle_exit);
            for (BattleState.BattleUser user : mBattleState.mBattledUsers.get()) {
                if (TextUtils.equals(mUserState.selfInfo.userId, user.userId)) {
                    enableView(findViewById(R.id.v_connection), false);
                    break;
                }
            }
        } else if (Boolean.FALSE.equals(started)) {
            mViewBattle.setBackgroundResource(R.drawable.livekit_function_battle);
            enableView(findViewById(R.id.v_connection), true);
        }
    }

    private void onDisplayResultChange(Boolean onDisplay) {
        if (Boolean.TRUE.equals(onDisplay)) {
            enableView(mViewBattle, false);
        } else if (Boolean.FALSE.equals(onDisplay)) {
            enableView(mViewBattle, true);
        }
    }
}

