package com.trtc.uikit.livekit.liveroom.view.audience.component;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.PermissionRequest;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;

@SuppressLint("ViewConstructor")
public class VideoLinkSettingsPanel extends ConstraintLayout {
    private static final int SEAT_INDEX   = -1;
    private static final int REQ_TIME_OUT = 0;

    private final Context                          mContext;
    private final RoomEngineService                mRoomEngineService;
    private final LiveKitStore                     mStore;
    private       TextView                         mTextTitleSettingsPanel;
    private       ImageView                        mImageBeautySettingsBack;
    private       Button                           mButtonApplyLinkMic;
    private       TextView                         mTextTipsApplyLinkMic;
    private       SeekBar                          mBeautySeekBar;
    private       TextView                         mTextBeautyType;
    private       TextView                         mTextBeautyLevel;
    private       VideoSettingsAdapter             mAdapter;
    private       PopupDialog.DialogActionListener mListener;
    private final Observer<Integer>                mItemTypeListener = (type) -> {
        if (type == VideoSettingsAdapter.ITEM_TYPE_SETTINGS) {
            exitBeautySettings();
        } else {
            enableBeautySettings();
        }
    };
    private final Observer<Integer>                mBeautyListener   = (type) -> {
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
    };

    public VideoLinkSettingsPanel(@NonNull Context context, RoomEngineService service) {
        super(context);
        mContext = context;
        mRoomEngineService = service;
        mStore = LiveKitStore.sharedInstance();
        initView();
    }

    private void initView() {
        inflate(mContext, R.layout.livekit_dialog_link_video_settings, this);
        setBackground(ContextCompat.getDrawable(mContext, R.drawable.livekit_dialog_background));

        RecyclerView mRecycleSettingsOption = findViewById(R.id.video_settings_options);
        mRecycleSettingsOption.setLayoutManager(new GridLayoutManager(mContext, 5));
        mAdapter = new VideoSettingsAdapter(mContext, mRoomEngineService);
        mRecycleSettingsOption.setAdapter(mAdapter);

        mTextTitleSettingsPanel = findViewById(R.id.title_link_audio_settings);
        mImageBeautySettingsBack = findViewById(R.id.beauty_settings_back);
        mImageBeautySettingsBack.setOnClickListener(view -> {
            mAdapter.backToVideoSettings();
            exitBeautySettings();
        });

        initBeautySeekBar();

        TUIVideoView previewVideoView = findViewById(R.id.preview_audience_video);
        mButtonApplyLinkMic = findViewById(R.id.btn_apply_link_mic);
        mTextTipsApplyLinkMic = findViewById(R.id.video_settings_apply_tips);
        mRoomEngineService.setLocalVideoView(previewVideoView);
        openSelfCamera();

        mButtonApplyLinkMic.setOnClickListener(view -> {
            mStore.setSelfStatus(TUILiveDefine.UserInteractionStatus.APPLYING);
            ToastUtil.toastShortMessageCenter(mContext.getString(R.string.livekit_toast_apply_link_mic));
            TUIRoomDefine.Request request =
                    mRoomEngineService.takeSeat(SEAT_INDEX, REQ_TIME_OUT,
                            new TUIRoomDefine.RequestCallback() {
                                @Override
                                public void onAccepted(String requestId, String userId) {
                                    mStore.setSelfStatus(TUILiveDefine.UserInteractionStatus.LINKING);
                                    ToastUtil.toastShortMessageCenter(
                                            mContext.getString(R.string.livekit_toast_link_mic_success));
                                    openSelfCamera();
                                    mRoomEngineService.openLocalMicrophone();
                                }

                                @Override
                                public void onRejected(String requestId, String userId, String message) {
                                    mStore.setSelfStatus(TUILiveDefine.UserInteractionStatus.NONE);
                                    ToastUtil.toastShortMessageCenter(
                                            mContext.getString(R.string.livekit_link_apply_rejected));
                                }

                                @Override
                                public void onCancelled(String requestId, String userId) {
                                    mStore.setSelfStatus(TUILiveDefine.UserInteractionStatus.NONE);
                                }

                                @Override
                                public void onTimeout(String requestId, String userId) {
                                    mStore.setSelfStatus(TUILiveDefine.UserInteractionStatus.NONE);
                                    ToastUtil.toastShortMessageCenter(
                                            mContext.getString(R.string.livekit_link_apply_timeout));
                                }

                                @Override
                                public void onError(String requestId, String userId,
                                                    TUICommonDefine.Error error, String message) {
                                    mStore.setSelfStatus(TUILiveDefine.UserInteractionStatus.NONE);
                                }
                            });
            mStore.selfInfo.requestId = request.requestId;
            mListener.dismiss();
        });
    }

    private void openSelfCamera() {
        PermissionRequest.requestPermissions(mContext.getApplicationContext(),
                new PermissionCallback() {
                    @Override
                    public void onGranted() {
                        mRoomEngineService.openLocalCamera(mStore.selfInfo.videoInfo.isFrontCamera.get(),
                                mStore.selfInfo.videoInfo.videoQuality.get(), null);
                    }
                });
    }

    public void initBeautySeekBar() {
        mTextBeautyLevel = findViewById(R.id.beauty_tv_seek_bar_level);
        mBeautySeekBar = findViewById(R.id.beauty_seek_bar);
        mTextBeautyType = findViewById(R.id.beauty_tv_seek_bar_type);

        mBeautySeekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                mTextBeautyLevel.setText(String.valueOf(progress));
                switch (mAdapter.getCurrentBeautytype()) {
                    case VideoSettingsAdapter.ITEM_BEAUTY_SMOOTH:
                        mStore.selfInfo.beautyInfo.smoothLevel.set(progress);
                        mRoomEngineService.setBeautyLevel(progress);
                        break;
                    case VideoSettingsAdapter.ITEM_BEAUTY_WHITENESS:
                        mStore.selfInfo.beautyInfo.whitenessLevel.set(progress);
                        mRoomEngineService.setWhitenessLevel(progress);
                        break;
                    case VideoSettingsAdapter.ITEM_BEAUTY_RUDDY:
                        mStore.selfInfo.beautyInfo.ruddyLevel.set(progress);
                        mRoomEngineService.setRuddyLevel(progress);
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
        mStore.selfInfo.beautyInfo.smoothLevel.set(0);
        mRoomEngineService.setBeautyLevel(0);
        mStore.selfInfo.beautyInfo.whitenessLevel.set(0);
        mRoomEngineService.setWhitenessLevel(0);
        mStore.selfInfo.beautyInfo.ruddyLevel.set(0);
        mRoomEngineService.setRuddyLevel(0);
    }

    public void enableSmooth() {
        final int currentProgress = mStore.selfInfo.beautyInfo.smoothLevel.get();
        setSeekBarVisible(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_smooth);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableWhiteness() {
        final int currentProgress = mStore.selfInfo.beautyInfo.whitenessLevel.get();
        setSeekBarVisible(VISIBLE);
        mTextBeautyType.setText(R.string.livekit_beauty_item_whiteness);
        mBeautySeekBar.setMax(9);
        mBeautySeekBar.setProgress(currentProgress);
        mTextBeautyLevel.setText(String.valueOf(mBeautySeekBar.getProgress()));
    }

    public void enableRuddy() {
        final int currentProgress = mStore.selfInfo.beautyInfo.ruddyLevel.get();
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
        mAdapter.mItemType.observe(mItemTypeListener);
    }

    private void removeObserver() {
        mAdapter.mItemType.removeObserver(mItemTypeListener);
    }
}
