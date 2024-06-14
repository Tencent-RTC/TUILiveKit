package com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming.link;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.MediaController;

@SuppressLint("ViewConstructor")
public class VideoLinkSettingsPanel extends BasicView {
    private static final int SEAT_INDEX = -1;

    private       TextView                         mTextTitleSettingsPanel;
    private       ImageView                        mImageBeautySettingsBack;
    private       Button                           mButtonApplyLinkMic;
    private       TextView                         mTextTipsApplyLinkMic;
    private       SeekBar                          mBeautySeekBar;
    private       TextView                         mTextBeautyType;
    private       TextView                         mTextBeautyLevel;
    private       VideoSettingsAdapter             mAdapter;
    private       PopupDialog.DialogActionListener mListener;
    private final Observer<Integer>                mItemTypeListener = this::onSettingsPanelTypeChanged;
    private final Observer<Integer>                mBeautyListener   = this::onBeautyTypeChanged;


    public VideoLinkSettingsPanel(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_dialog_link_video_settings, this, true);
        bindViewId();

        initRecycleSettingsOption();
        initTextTitleSettingsPanel();
        initBeautySeekBar();
        initPreviewVideoView();
        initApplyLinkMicButton();
    }

    @Override
    protected void addObserver() {
        mAdapter.mItemType.observe(mItemTypeListener);
    }

    @Override
    protected void removeObserver() {
        mAdapter.mItemType.removeObserver(mItemTypeListener);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mLiveController.getMediaController().closeLocalCamera();
    }

    private void initApplyLinkMicButton() {
        mButtonApplyLinkMic.setOnClickListener(view -> {
            ToastUtil.toastShortMessageCenter(mContext.getString(R.string.livekit_toast_apply_link_mic));
            mLiveController.getSeatController().takeSeat(SEAT_INDEX);
            mListener.dismiss();
        });
    }

    private void initPreviewVideoView() {
        TUIVideoView previewVideoView = findViewById(R.id.preview_audience_video);
        MediaController mediaController = mLiveController.getMediaController();
        mediaController.setLocalVideoView(previewVideoView);
        mediaController.openLocalCamera();
    }

    private void initTextTitleSettingsPanel() {
        mImageBeautySettingsBack.setOnClickListener(view -> {
            mAdapter.backToVideoSettings();
            exitBeautySettings();
        });
    }

    private void initRecycleSettingsOption() {
        RecyclerView mRecycleSettingsOption = findViewById(R.id.video_settings_options);
        mRecycleSettingsOption.setLayoutManager(new GridLayoutManager(mContext, 5));
        mAdapter = new VideoSettingsAdapter(mContext, mLiveController);
        mRecycleSettingsOption.setAdapter(mAdapter);
    }

    private void bindViewId() {
        mTextTitleSettingsPanel = findViewById(R.id.title_link_audio_settings);
        mImageBeautySettingsBack = findViewById(R.id.beauty_settings_back);
        mButtonApplyLinkMic = findViewById(R.id.btn_apply_link_mic);
        mTextTipsApplyLinkMic = findViewById(R.id.video_settings_apply_tips);
    }

    public void initBeautySeekBar() {
        mTextBeautyLevel = findViewById(R.id.beauty_tv_seek_bar_level);
        mBeautySeekBar = findViewById(R.id.beauty_seek_bar);
        mTextBeautyType = findViewById(R.id.beauty_tv_seek_bar_type);

        mBeautySeekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                MediaController mediaController = mLiveController.getMediaController();
                mTextBeautyLevel.setText(String.valueOf(progress));
                switch (mAdapter.getCurrentBeautytype()) {
                    case VideoSettingsAdapter.ITEM_BEAUTY_SMOOTH:
                        mediaController.setBeautyLevel(progress);
                        break;
                    case VideoSettingsAdapter.ITEM_BEAUTY_WHITENESS:
                        mediaController.setWhitenessLevel(progress);
                        break;
                    case VideoSettingsAdapter.ITEM_BEAUTY_RUDDY:
                        mediaController.setRuddyLevel(progress);
                        break;
                    default:
                        break;
                }
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {

            }
        });
    }

    public void enableBeautySettings() {
        mImageBeautySettingsBack.setVisibility(VISIBLE);
        mTextTitleSettingsPanel.setText(mContext.getString(R.string.livekit_beauty_panel_title));
        mButtonApplyLinkMic.setVisibility(GONE);
        mTextTipsApplyLinkMic.setVisibility(GONE);
        mAdapter.mCurrentBeautyType.observe(mBeautyListener);
    }

    private void exitBeautySettings() {
        mImageBeautySettingsBack.setVisibility(GONE);
        mTextTitleSettingsPanel.setText(mContext.getString(R.string.livekit_title_link_video_settings));
        mButtonApplyLinkMic.setVisibility(VISIBLE);
        mTextTipsApplyLinkMic.setVisibility(VISIBLE);
        mAdapter.mCurrentBeautyType.removeObserver(mBeautyListener);
    }

    public void closeBeauty() {
        setSeekBarVisible(GONE);
        mLiveController.getMediaController().closeBeauty();
    }

    public void enableSmooth() {
        final int currentProgress = mBeautyState.smoothLevel.get();
        setSeekBarVisible(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_smooth);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableWhiteness() {
        final int currentProgress = mBeautyState.whitenessLevel.get();
        setSeekBarVisible(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_whiteness);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableRuddy() {
        final int currentProgress = mBeautyState.ruddyLevel.get();
        setSeekBarVisible(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_ruddy);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void setSeekBarVisible(int visible) {
        mTextBeautyType.setVisibility(visible);
        mBeautySeekBar.setVisibility(visible);
        mTextBeautyLevel.setVisibility(visible);
    }

    public void setDialogActionListener(PopupDialog.DialogActionListener listener) {
        mListener = listener;
    }

    private void onBeautyTypeChanged(Integer type) {
        switch (type) {
            case VideoSettingsAdapter.ITEM_BEAUTY_CLOSE:
                closeBeauty();
                break;
            case VideoSettingsAdapter.ITEM_BEAUTY_SMOOTH:
                enableSmooth();
                break;
            case VideoSettingsAdapter.ITEM_BEAUTY_WHITENESS:
                enableWhiteness();
                break;
            case VideoSettingsAdapter.ITEM_BEAUTY_RUDDY:
                enableRuddy();
                break;
            default:
                break;
        }
    }

    private void onSettingsPanelTypeChanged(Integer type) {
        if (type == VideoSettingsAdapter.ITEM_TYPE_SETTINGS) {
            exitBeautySettings();
        } else {
            enableBeautySettings();
        }
    }
}
