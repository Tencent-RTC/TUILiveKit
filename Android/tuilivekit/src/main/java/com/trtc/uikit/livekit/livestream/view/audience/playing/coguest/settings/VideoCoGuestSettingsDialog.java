package com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.settings;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class VideoCoGuestSettingsDialog extends PopupDialog {

    private       TUIVideoView                mPreviewVideoView;
    private       TextView                    mTextTitleSettingsPanel;
    private       ImageView                   mImageBeautySettingsBack;
    private       Button                      mButtonApplyLinkMic;
    private       TextView                    mTextTipsApplyLinkMic;
    private       SeekBar                     mBeautySeekBar;
    private       TextView                    mTextBeautyType;
    private       TextView                    mTextBeautyLevel;
    private       RecyclerView                mRecycleSettingsOption;
    private       VideoCoGuestSettingsAdapter mAdapter;
    private final LiveCoreView                mLiveStream;
    private final LiveStreamManager           mLiveManager;
    private final Observer<Integer>           mItemTypeListener = this::onSettingsPanelTypeChanged;
    private final Observer<Integer>           mBeautyListener   = this::onBeautyTypeChanged;


    public VideoCoGuestSettingsDialog(@NonNull Context context, LiveStreamManager manager,
                                      LiveCoreView liveStream) {
        super(context);
        mLiveManager = manager;
        mLiveStream = liveStream;
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_dialog_link_video_settings, null);
        bindViewId(view);

        initRecycleSettingsOption();
        initTextTitleSettingsPanel();
        initBeautySeekBar();
        initPreviewVideoView();
        initApplyLinkMicButton();

        addObserver();
        setOnDismissListener(dialog -> removeObserver());
        setView(view);
    }

    private void bindViewId(View view) {
        mPreviewVideoView = view.findViewById(R.id.preview_audience_video);
        mTextTitleSettingsPanel = view.findViewById(R.id.title_link_audio_settings);
        mImageBeautySettingsBack = view.findViewById(R.id.beauty_settings_back);
        mButtonApplyLinkMic = view.findViewById(R.id.btn_apply_link_mic);
        mTextTipsApplyLinkMic = view.findViewById(R.id.video_settings_apply_tips);
        mRecycleSettingsOption = view.findViewById(R.id.video_settings_options);
        mTextBeautyLevel = view.findViewById(R.id.beauty_tv_seek_bar_level);
        mBeautySeekBar = view.findViewById(R.id.beauty_seek_bar);
        mTextBeautyType = view.findViewById(R.id.beauty_tv_seek_bar_type);
    }

    protected void addObserver() {
        mAdapter.mItemType.observe(mItemTypeListener);
    }

    protected void removeObserver() {
        mAdapter.mItemType.removeObserver(mItemTypeListener);
    }

    private void initApplyLinkMicButton() {
        mButtonApplyLinkMic.setOnClickListener(view -> {
            ToastUtil.toastShortMessageCenter(getContext().getString(R.string.livekit_toast_apply_link_mic));
            mLiveStream.requestIntraRoomConnection("", 60, true, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {

                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorHandler.onError(error);
                }
            });

            removeObserver();
            dismiss();
        });
    }

    private void initPreviewVideoView() {
        mLiveManager.getMediaManager().setLocalVideoView(mPreviewVideoView);
        mLiveManager.getMediaManager().openLocalCamera();
    }

    private void initTextTitleSettingsPanel() {
        mImageBeautySettingsBack.setOnClickListener(view -> {
            mAdapter.backToVideoSettings();
            exitBeautySettings();
        });
    }

    private void initRecycleSettingsOption() {
        mRecycleSettingsOption.setLayoutManager(new GridLayoutManager(getContext(), 5));
        mAdapter = new VideoCoGuestSettingsAdapter(getContext(), mLiveManager, mLiveStream);
        mRecycleSettingsOption.setAdapter(mAdapter);
    }

    public void initBeautySeekBar() {
        mBeautySeekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                mTextBeautyLevel.setText(String.valueOf(progress));
                switch (mAdapter.getCurrentBeautytype()) {
                    case VideoCoGuestSettingsAdapter.ITEM_BEAUTY_SMOOTH:
                        mLiveManager.getMediaManager().setBeautyLevel(progress);
                        break;
                    case VideoCoGuestSettingsAdapter.ITEM_BEAUTY_WHITENESS:
                        mLiveManager.getMediaManager().setWhitenessLevel(progress);
                        break;
                    case VideoCoGuestSettingsAdapter.ITEM_BEAUTY_RUDDY:
                        mLiveManager.getMediaManager().setRuddyLevel(progress);
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
        mTextTitleSettingsPanel.setText(getContext().getString(R.string.livekit_beauty_panel_title));
        mButtonApplyLinkMic.setVisibility(GONE);
        mTextTipsApplyLinkMic.setVisibility(GONE);
        mAdapter.mCurrentBeautyType.observe(mBeautyListener);
    }

    private void exitBeautySettings() {
        mImageBeautySettingsBack.setVisibility(GONE);
        mTextTitleSettingsPanel.setText(getContext().getString(R.string.livekit_title_link_video_settings));
        mButtonApplyLinkMic.setVisibility(VISIBLE);
        mTextTipsApplyLinkMic.setVisibility(VISIBLE);
        mAdapter.mCurrentBeautyType.removeObserver(mBeautyListener);
    }

    public void closeBeauty() {
        setSeekBarVisible(GONE);
        mLiveManager.getMediaManager().closeBeauty();
    }

    public void enableSmooth() {
        final int currentProgress = mLiveManager.getBeautyState().smoothLevel.get();
        setSeekBarVisible(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_smooth);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableWhiteness() {
        final int currentProgress = mLiveManager.getBeautyState().whitenessLevel.get();
        setSeekBarVisible(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_whiteness);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableRuddy() {
        final int currentProgress = mLiveManager.getBeautyState().ruddyLevel.get();
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

    private void onBeautyTypeChanged(Integer type) {
        switch (type) {
            case VideoCoGuestSettingsAdapter.ITEM_BEAUTY_CLOSE:
                closeBeauty();
                break;
            case VideoCoGuestSettingsAdapter.ITEM_BEAUTY_SMOOTH:
                enableSmooth();
                break;
            case VideoCoGuestSettingsAdapter.ITEM_BEAUTY_WHITENESS:
                enableWhiteness();
                break;
            case VideoCoGuestSettingsAdapter.ITEM_BEAUTY_RUDDY:
                enableRuddy();
                break;
            default:
                break;
        }
    }

    private void onSettingsPanelTypeChanged(Integer type) {
        if (type == VideoCoGuestSettingsAdapter.ITEM_TYPE_SETTINGS) {
            exitBeautySettings();
        } else {
            enableBeautySettings();
        }
    }
}
