package com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageButton;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.anchor.component.common.MusicPanel;
import com.trtc.uikit.livekit.liveroom.view.anchor.component.common.SettingsPanel;

@SuppressLint("ViewConstructor")
public class FunctionView extends LinearLayout {

    private       AnchorLinkMicPanel mAnchorLinkMicPanel;
    private       PopupDialog        mAnchorLinkMicDialog;
    private       SettingsPanel      mSettingsPanel;
    private       PopupDialog        mSettingsPanelDialog;
    private       MusicPanel         mMusicPanel;
    private       PopupDialog        mMusicPanelDialog;
    private       RelativeLayout     mLayoutBarrageSendContainer;
    private final Context            mContext;
    private final LiveRoomInfo       mLiveRoomInfo;
    private final RoomEngineService  mRoomEngineService;

    public FunctionView(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;

        initView();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {

    }

    private void removeObserver() {

    }

    private void initView() {
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_function, this,
                true);

        initBarrageSendContainer(rootView);
        initLinkButton(rootView);
        initSettingsButton(rootView);
        initMusicButton(rootView);
    }

    private void initBarrageSendContainer(View rootView) {
        mLayoutBarrageSendContainer = rootView.findViewById(R.id.rl_barrage);

        TUIBarrageButton barrageButton = new TUIBarrageButton(mContext, mLiveRoomInfo.roomId);
        mLayoutBarrageSendContainer.addView(barrageButton);
    }

    private void initLinkButton(View rootView) {
        Button linkButton = rootView.findViewById(R.id.btn_link);
        linkButton.setOnClickListener((view) -> {
            if (mAnchorLinkMicDialog == null) {
                mAnchorLinkMicDialog = new PopupDialog(mContext);
                mAnchorLinkMicDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mAnchorLinkMicPanel == null) {
                mAnchorLinkMicPanel = new AnchorLinkMicPanel(mContext, mLiveRoomInfo, mRoomEngineService, () -> {
                    mAnchorLinkMicDialog.dismiss();
                });
            }
            mAnchorLinkMicDialog.setView(mAnchorLinkMicPanel);
            mAnchorLinkMicDialog.show();
        });
    }


    private void initSettingsButton(View rootView) {
        Button settingsButton = rootView.findViewById(R.id.btn_settings);
        settingsButton.setOnClickListener((view) -> {
            if (mSettingsPanelDialog == null) {
                mSettingsPanelDialog = new PopupDialog(mContext,
                        com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
                mSettingsPanelDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mSettingsPanel == null) {
                mSettingsPanel = new SettingsPanel(mContext, mLiveRoomInfo, mRoomEngineService);
            }
            mSettingsPanelDialog.setView(mSettingsPanel);
            mSettingsPanelDialog.show();
        });
    }

    private void initMusicButton(View rootView) {
        Button musicButton = rootView.findViewById(R.id.btn_music);
        musicButton.setOnClickListener((view) -> {
            if (mMusicPanelDialog == null) {
                mMusicPanelDialog = new PopupDialog(mContext);
                mMusicPanelDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mMusicPanel == null) {
                mMusicPanel = new MusicPanel(mContext, mLiveRoomInfo, mRoomEngineService);
            }
            mMusicPanelDialog.setView(mMusicPanel);
            mMusicPanel.refresh();
            mMusicPanelDialog.show();
        });
    }
}

