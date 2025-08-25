package com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.panel;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.content.Context;
import android.text.TextUtils;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.features.anchorboardcast.view.usermanage.ConfirmDialog;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.List;
import java.util.Set;

public class AnchorManagerDialog extends PopupDialog {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorManagerDialog");

    private static final float BUTTON_DISABLE_ALPHA = 0.24f;
    private static final float BUTTON_ENABLE_ALPHA  = 1.0f;

    private final Context                    mContext;
    private final AnchorManager              mAnchorManager;
    private final LiveCoreView               mLiveCoreView;
    private       TUIRoomDefine.SeatFullInfo mUserInfo;
    private       ImageFilterView            mImageHeadView;
    private       TextView                   mUserIdText;
    private       TextView                   mUserNameText;
    private       View                       mFlipCameraContainer;
    private       View                       mFollowContainer;
    private       View                       mHandUpContainer;
    private       View                       mAudioContainer;
    private       View                       mVideoContainer;
    private       ImageView                  mIvAudio;
    private       TextView                   mTvAudio;
    private       ImageView                  mIvVideo;
    private       TextView                   mTvVideo;
    private       TextView                   mTextUnfollow;
    private       ImageView                  mImageFollowIcon;
    private       ConfirmDialog              mConfirmDialog;

    private final Observer<Boolean>                      mMicrophoneMutedObserver  = value -> updateMicrophoneButton();
    private final Observer<Boolean>                      mCameraOpenedObserver     = value -> updateCameraButton();
    private final Observer<Set<String>>                  lockAudioUserListObserver = value -> updateMicrophoneButton();
    private final Observer<Set<String>>                  lockVideoUserListObserver = value -> updateCameraButton();
    private final Observer<List<TUIRoomDefine.UserInfo>> mConnectUserListObserver  = this::onConnectUserListChanged;
    private final Observer<Set<String>>                  mFollowingUserObserver    = this::onFollowingUserChanged;

    public AnchorManagerDialog(@NonNull Context context, AnchorManager liveStreamManager,
                               LiveCoreView liveCoreView) {
        super(context);
        mContext = context;
        mAnchorManager = liveStreamManager;
        mLiveCoreView = liveCoreView;
        initView();
    }

    public void init(TUIRoomDefine.SeatFullInfo userInfo) {
        mUserInfo = userInfo;
        mAnchorManager.getUserManager().checkFollowUser(userInfo.userId);
        updateView();
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void initView() {
        View rootView = View.inflate(mContext, R.layout.livekit_anchor_manager_panel, null);
        setView(rootView);
        bindViewId(rootView);
        initFollowButtonView(rootView);
    }

    private void bindViewId(View rootView) {
        mUserIdText = rootView.findViewById(R.id.user_id);
        mUserNameText = rootView.findViewById(R.id.user_name);
        mImageHeadView = rootView.findViewById(R.id.iv_head);
        mHandUpContainer = rootView.findViewById(R.id.hand_up_container);
        mFlipCameraContainer = rootView.findViewById(R.id.flip_camera_container);
        mFollowContainer = rootView.findViewById(R.id.fl_follow_panel);
        mIvAudio = rootView.findViewById(R.id.iv_audio);
        mAudioContainer = rootView.findViewById(R.id.audio_container);
        mTvAudio = rootView.findViewById(R.id.tv_audio);
        mVideoContainer = rootView.findViewById(R.id.video_container);
        mIvVideo = rootView.findViewById(R.id.iv_video);
        mTvVideo = rootView.findViewById(R.id.tv_video);
        mTextUnfollow = rootView.findViewById(R.id.tv_unfollow);
        mImageFollowIcon = rootView.findViewById(R.id.iv_follow);

        mHandUpContainer.setOnClickListener(v -> onHangupButtonClicked());
        mFlipCameraContainer.setOnClickListener(v -> onSwitchCameraButtonClicked());
        mAudioContainer.setOnClickListener(v -> onMicrophoneButtonClicked());
        mVideoContainer.setOnClickListener(v -> onCameraButtonClicked());
    }

    private void updateView() {
        if (mUserInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(mUserInfo.userId)) {
            return;
        }
        String avatarUrl = mUserInfo.userAvatar;
        if (TextUtils.isEmpty(avatarUrl)) {
            mImageHeadView.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImageHeadView, avatarUrl, R.drawable.livekit_ic_avatar);
        }
        mUserNameText.setText(mUserInfo.userName);
        mUserIdText.setText(mContext.getString(R.string.common_user_id, mUserInfo.userId));
        updateMediaDeviceButton();
    }

    private void updateMediaDeviceButton() {
        if (isAdmin()) {
            updateAdminDeviceButton();
        } else {
            updateGeneralUserDeviceButton();
        }
    }

    private void updateAdminDeviceButton() {
        if (isSelfUser()) {
            mFlipCameraContainer.setVisibility(VISIBLE);
            mHandUpContainer.setVisibility(GONE);
            mFollowContainer.setVisibility(GONE);
        } else {
            mFlipCameraContainer.setVisibility(GONE);
            mHandUpContainer.setVisibility(VISIBLE);
            mFollowContainer.setVisibility(VISIBLE);
        }
    }

    private void updateGeneralUserDeviceButton() {
        if (isSelfUser()) {
            mFlipCameraContainer.setVisibility(VISIBLE);
            mHandUpContainer.setVisibility(VISIBLE);
            mFollowContainer.setVisibility(GONE);
            updateCameraButton();
            updateMicrophoneButton();
        }
    }

    private boolean isSelfUser() {
        if (mUserInfo == null) {
            return false;
        }
        if (TextUtils.isEmpty(mUserInfo.userId)) {
            return false;
        }
        return mUserInfo.userId.equals(mAnchorManager.getCoreState().userState.selfInfo.getValue().userId);
    }

    private boolean isAdmin() {
        String selfUserId = mAnchorManager.getCoreState().userState.selfInfo.getValue().userId;
        if (TextUtils.isEmpty(selfUserId)) {
            return false;
        }
        return selfUserId.equals(mAnchorManager.getCoreState().roomState.ownerInfo.getValue().userId);
    }

    private void addObserver() {
        mAnchorManager.getCoGuestState().lockAudioUserList.observeForever(lockAudioUserListObserver);
        mAnchorManager.getCoGuestState().lockVideoUserList.observeForever(lockVideoUserListObserver);
        mAnchorManager.getUserState().followingUserList.observeForever(mFollowingUserObserver);
        mLiveCoreView.getCoreState().mediaState.isMicrophoneMuted.observeForever(mMicrophoneMutedObserver);
        mLiveCoreView.getCoreState().mediaState.isCameraOpened.observeForever(mCameraOpenedObserver);
        mLiveCoreView.getCoreState().coGuestState.connectedUserList.observeForever(mConnectUserListObserver);
        TUIRoomEngine.sharedInstance().addObserver(mTUIRoomObserver);
    }

    private void removeObserver() {
        mAnchorManager.getCoGuestState().lockAudioUserList.removeObserver(lockAudioUserListObserver);
        mAnchorManager.getCoGuestState().lockVideoUserList.removeObserver(lockVideoUserListObserver);
        mAnchorManager.getUserState().followingUserList.removeObserver(mFollowingUserObserver);
        mLiveCoreView.getCoreState().mediaState.isMicrophoneMuted.removeObserver(mMicrophoneMutedObserver);
        mLiveCoreView.getCoreState().mediaState.isCameraOpened.removeObserver(mCameraOpenedObserver);
        mLiveCoreView.getCoreState().coGuestState.connectedUserList.removeObserver(mConnectUserListObserver);
        TUIRoomEngine.sharedInstance().removeObserver(mTUIRoomObserver);
    }

    private void initFollowButtonView(View rootView) {
        rootView.findViewById(R.id.fl_follow_panel).setOnClickListener(view -> {
            if (mUserInfo == null) {
                return;
            }
            if (mAnchorManager.getUserState().followingUserList.getValue().contains(mUserInfo.userId)) {
                mAnchorManager.getUserManager().unfollowUser(mUserInfo.userId);
            } else {
                mAnchorManager.getUserManager().followUser(mUserInfo.userId);
            }
        });
    }

    private void onFollowingUserChanged(Set<String> followUsers) {
        if (mUserInfo == null) {
            return;
        }
        if (followUsers.contains(mUserInfo.userId)) {
            mTextUnfollow.setVisibility(GONE);
            mImageFollowIcon.setVisibility(VISIBLE);
        } else {
            mImageFollowIcon.setVisibility(GONE);
            mTextUnfollow.setVisibility(VISIBLE);
        }
    }

    private void onHangupButtonClicked() {
        if (mUserInfo == null) {
            return;
        }

        if (mConfirmDialog == null) {
            mConfirmDialog = new ConfirmDialog(mContext);
        }


        if (isAdmin()) {
            mConfirmDialog.setContent(mContext.getString(R.string.common_disconnect_tips));
            mConfirmDialog.setPositiveText(mContext.getString(R.string.common_disconnection));
            mConfirmDialog.setPositiveListener(v -> {
                mLiveCoreView.disconnectUser(mUserInfo.userId, new TUIRoomDefine.ActionCallback() {
                    @Override
                    public void onSuccess() {
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LOGGER.error("disconnectUser failed:error:" + error + "," +
                                "errorCode:" + error.getValue() + "message:" + message);
                        ErrorLocalized.onError(error);
                    }
                });
                dismiss();
            });
            mConfirmDialog.show();
            return;
        }

        if (isSelfUser()) {
            mConfirmDialog.setContent(mContext.getString(R.string.common_terminate_room_connection_message));
            mConfirmDialog.setPositiveText(mContext.getString(R.string.common_disconnection));
            mConfirmDialog.setPositiveListener(v -> {
                mLiveCoreView.terminateIntraRoomConnection();
                dismiss();
            });
            mConfirmDialog.show();
        }
    }

    private void onMicrophoneButtonClicked() {
        if (mUserInfo == null) {
            return;
        }

        if (isSelfUser()) {
            if (Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isMicrophoneMuted.getValue())) {
                unMuteMicrophone();
            } else {
                mLiveCoreView.muteMicrophone();
            }
            dismiss();
            return;
        }

        if (isAdmin()) {
            mAnchorManager.getCoGuestManager().disableUserMediaDevice(mUserInfo.userId,
                    CoGuestManager.MediaDevice.MICROPHONE);
            dismiss();
        }
    }

    private void onCameraButtonClicked() {
        if (mUserInfo == null) {
            return;
        }

        if (isSelfUser()) {
            if (Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isCameraOpened.getValue())) {
                mLiveCoreView.stopCamera();
                mFlipCameraContainer.setVisibility(GONE);
            } else {
                startCamera();
            }
            dismiss();
            return;
        }

        if (isAdmin()) {
            mAnchorManager.getCoGuestManager().disableUserMediaDevice(mUserInfo.userId,
                    CoGuestManager.MediaDevice.CAMERA);
            dismiss();
        }
    }

    private void onSwitchCameraButtonClicked() {
        boolean isFront = Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isFrontCamera.getValue());
        mLiveCoreView.switchCamera(!isFront);
        dismiss();
    }

    private void unMuteMicrophone() {
        mLiveCoreView.unmuteMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("unMuteMicrophone failed:error:" + error + "," +
                        "errorCode:" + error.getValue() + "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void startCamera() {
        boolean isFrontCamera = Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isFrontCamera.getValue());
        mLiveCoreView.startCamera(isFrontCamera, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("startCamera failed:error:" + error + "," +
                        "errorCode:" + error.getValue() + "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void updateMicrophoneButton() {
        boolean isAudioLocked =
                mAnchorManager.getCoGuestState().lockAudioUserList.getValue().contains(mUserInfo.userId);
        if (!isSelfUser()) {
            if (isAdmin()) {
                mAudioContainer.setEnabled(true);
                mIvAudio.setAlpha(BUTTON_ENABLE_ALPHA);
                mIvAudio.setImageResource(isAudioLocked ? R.drawable.livekit_ic_disable_audio :
                        R.drawable.livekit_ic_unmute_audio);
                mTvAudio.setText(isAudioLocked ? R.string.common_enable_audio : R.string.common_disable_audio);
            }
            return;
        }

        if (isAudioLocked) {
            mAudioContainer.setEnabled(false);
            mIvAudio.setAlpha(BUTTON_DISABLE_ALPHA);
            mIvAudio.setImageResource(R.drawable.livekit_ic_mute_audio);
            mTvAudio.setText(R.string.common_unmute_audio);
            return;
        }

        mAudioContainer.setEnabled(true);
        mIvAudio.setAlpha(BUTTON_ENABLE_ALPHA);
        boolean isMicrophoneMuted =
                Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isMicrophoneMuted.getValue());
        if (isMicrophoneMuted) {
            mIvAudio.setImageResource(R.drawable.livekit_ic_mute_audio);
            mTvAudio.setText(R.string.common_unmute_audio);
        } else {
            mIvAudio.setImageResource(R.drawable.livekit_ic_unmute_audio);
            mTvAudio.setText(R.string.common_mute_audio);
        }
    }

    private void updateCameraButton() {
        boolean isVideoLocked =
                mAnchorManager.getCoGuestState().lockVideoUserList.getValue().contains(mUserInfo.userId);
        if (!isSelfUser()) {
            if (isAdmin()) {
                mVideoContainer.setEnabled(true);
                mIvVideo.setAlpha(BUTTON_ENABLE_ALPHA);
                mIvVideo.setImageResource(isVideoLocked ? R.drawable.livekit_ic_disable_video :
                        R.drawable.livekit_ic_start_video);
                mTvVideo.setText(isVideoLocked ? R.string.common_enable_video : R.string.common_disable_video);
            }
            return;
        }

        boolean isCameraOpened = Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isCameraOpened.getValue());
        if (isVideoLocked) {
            mVideoContainer.setEnabled(false);
            mIvVideo.setAlpha(BUTTON_DISABLE_ALPHA);
            mIvVideo.setImageResource(R.drawable.livekit_ic_stop_video);
            mTvVideo.setText(R.string.common_start_video);
            mFlipCameraContainer.setVisibility(GONE);
            return;
        }

        mVideoContainer.setEnabled(true);
        mIvVideo.setAlpha(BUTTON_ENABLE_ALPHA);
        if (isCameraOpened) {
            mIvVideo.setImageResource(R.drawable.livekit_ic_start_video);
            mTvVideo.setText(R.string.common_stop_video);
            mFlipCameraContainer.setVisibility(VISIBLE);
        } else {
            mIvVideo.setImageResource(R.drawable.livekit_ic_stop_video);
            mTvVideo.setText(R.string.common_start_video);
            mFlipCameraContainer.setVisibility(GONE);
        }
    }

    private void onConnectUserListChanged(List<TUIRoomDefine.UserInfo> connectedUserList) {
        if (mUserInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(mUserInfo.userId)) {
            return;
        }
        boolean isConnected = false;
        for (TUIRoomDefine.UserInfo userInfo : connectedUserList) {
            if (mUserInfo.userId.equals(userInfo.userId)) {
                isConnected = true;
                break;
            }
        }
        if (!isConnected) {
            dismiss();
            if (mConfirmDialog != null) {
                mConfirmDialog.dismiss();
            }
        }
    }

    private final TUIRoomObserver mTUIRoomObserver = new TUIRoomObserver() {
        @Override
        public void onKickedOffSeat(int seatIndex, TUIRoomDefine.UserInfo operateUser) {
            dismiss();
            if (mConfirmDialog != null) {
                mConfirmDialog.dismiss();
            }
        }

        @Override
        public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
            if (mUserInfo == null) {
                return;
            }
            if (TextUtils.isEmpty(mUserInfo.userId)) {
                return;
            }
            if (userInfo.userId.equals(mUserInfo.userId)) {
                dismiss();
                if (mConfirmDialog != null) {
                    mConfirmDialog.dismiss();
                }
            }
        }
    };
}