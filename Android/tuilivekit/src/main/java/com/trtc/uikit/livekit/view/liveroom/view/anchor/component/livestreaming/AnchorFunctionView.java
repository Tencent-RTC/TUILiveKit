package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.RelativeLayout;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageButton;
import com.trtc.uikit.livekit.common.uicomponent.music.view.MusicPanelView;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common.SettingsPanel;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.link.AnchorLinkMicManagePanel;

@SuppressLint("ViewConstructor")
public class AnchorFunctionView extends BasicView {

    private AnchorLinkMicManagePanel mLinkMicManagePanel;
    private PopupDialog              mAnchorLinkMicDialog;
    private SettingsPanel            mSettingsPanel;
    private PopupDialog              mSettingsPanelDialog;
    private PopupDialog              mMusicPanel;

    public AnchorFunctionView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_function, this, true);

        initBarrageSendContainer();
        initLinkButton();
        initSettingsButton();
        initMusicButton();
    }

    private void initBarrageSendContainer() {
        RelativeLayout mLayoutBarrageSendContainer = findViewById(R.id.rl_barrage);

        TUIBarrageButton barrageButton = new TUIBarrageButton(mContext, mRoomState.roomId, mRoomState.ownerInfo.userId);
        mLayoutBarrageSendContainer.addView(barrageButton);
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    private void initLinkButton() {
        findViewById(R.id.btn_link).setOnClickListener((view) -> {
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


    private void initSettingsButton() {
        findViewById(R.id.btn_settings).setOnClickListener((view) -> {
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

    private void initMusicButton() {
        findViewById(R.id.btn_music).setOnClickListener((view) -> {
            if (mMusicPanel == null) {
                mMusicPanel = new PopupDialog(mContext);
                MusicPanelView musicListPanelView = new MusicPanelView(mContext, mRoomState.roomId,
                        mLiveController.getLiveService().getTRTCCloud());
                mMusicPanel.setView(musicListPanelView);
            }
            mMusicPanel.show();
        });
    }
}

