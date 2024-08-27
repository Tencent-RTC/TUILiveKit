package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.Window;
import android.widget.RelativeLayout;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageButton;
import com.trtc.uikit.livekit.common.uicomponent.music.view.MusicPanelView;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common.SettingsPanel;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.connection.AnchorConnectionManagePanel;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.link.AnchorLinkMicManagePanel;

import java.util.List;

@SuppressLint("ViewConstructor")
public class AnchorFunctionView extends BasicView {

    private       AnchorLinkMicManagePanel                       mLinkMicManagePanel;
    private       PopupDialog                                    mAnchorLinkMicDialog;
    private       AnchorConnectionManagePanel                    mConnectionPanel;
    private       SettingsPanel                                  mSettingsPanel;
    private       PopupDialog                                    mSettingsPanelDialog;
    private       PopupDialog                                    mMusicPanel;
    private       PopupDialog                                    mAnchorConnectionDialog;
    private       View                                           mViewLinkMic;
    private final Observer<List<ConnectionState.ConnectionUser>> mConnectedObserver
            = this::onConnectedUserChange;

    public AnchorFunctionView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_function, this, true);

        initBarrageSendContainer();
        initConnectionView();
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
    }

    @Override
    protected void removeObserver() {
        mConnectionState.connectedUsers.removeObserver(mConnectedObserver);
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

    private void initLinkView() {
        mViewLinkMic = findViewById(R.id.v_link);
        setLinkMicClickListener();
    }

    private void setLinkMicClickListener() {
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
    private void onConnectedUserChange(List<ConnectionState.ConnectionUser> connectedList) {
        post(() -> {
            if (mConnectionState.connectedUsers.get().isEmpty()) {
                setLinkMicClickListener();
                mViewLinkMic.setAlpha(1.0f);
            } else {
                mViewLinkMic.setOnClickListener(null);
                mViewLinkMic.setAlpha(0.5f);
            }
        });
    }

}

