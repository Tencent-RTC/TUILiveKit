package com.trtc.uikit.livekit.features.audiencecontainer.view.cohost.widgets;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.DeviceStatus.OPENED;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.SeatFullInfo;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.view.BasicView;

import java.util.List;


public class CoHostForegroundWidgetsView extends BasicView {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("CoHostWidgetsView");

    private       LinearLayout                           mLayoutUserInfo;
    private       TextView                               mTextName;
    private       ImageView                              mImageMuteAudio;
    private       SeatFullInfo                           mState                    = new SeatFullInfo();
    private final Observer<List<TUIRoomDefine.UserInfo>> mCoGuestObserver          = this::onCoGuestChange;
    private final Observer<List<ConnectionUser>>         mCoHostObserver           = this::onCoHostChange;
    private final Observer<Boolean>                      mPictureInPictureObserver = this::onPictureInPictureObserver;

    public CoHostForegroundWidgetsView(@NonNull Context context) {
        this(context, null);
    }

    public CoHostForegroundWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CoHostForegroundWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(AudienceManager manager, TUIRoomDefine.SeatFullInfo seatInfo) {
        LOGGER.info("init userId:" + seatInfo.userId + ",roomId:" + seatInfo.roomId);
        mState = seatInfo;
        super.init(manager);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_co_guest_foreground_widgets_view, this, true);
        mLayoutUserInfo = findViewById(R.id.ll_user_info);
        mImageMuteAudio = findViewById(R.id.iv_mute_audio);
        mTextName = findViewById(R.id.tv_name);
    }

    @Override
    protected void refreshView() {
        initUserNameView();
        initMuteAudioView();
    }

    private void initMuteAudioView() {
        mImageMuteAudio.setVisibility(mState.userMicrophoneStatus == OPENED ? GONE : VISIBLE);
    }

    private void initUserNameView() {
        if (isShowUserInfo()) {
            mLayoutUserInfo.setVisibility(VISIBLE);
        } else {
            mLayoutUserInfo.setVisibility(GONE);
        }
        mTextName.setText(TextUtils.isEmpty(mState.userName) ? mState.userId : mState.userName);
    }

    @Override
    protected void addObserver() {
        mAudienceManager.getCoreState().coGuestState.connectedUserList.observeForever(mCoGuestObserver);
        mAudienceManager.getCoreState().coHostState.connectedUserList.observeForever(mCoHostObserver);
        mMediaState.isPictureInPictureMode.observeForever(mPictureInPictureObserver);
    }

    @Override
    protected void removeObserver() {
        mAudienceManager.getCoreState().coGuestState.connectedUserList.removeObserver(mCoGuestObserver);
        mAudienceManager.getCoreState().coHostState.connectedUserList.removeObserver(mCoHostObserver);
        mMediaState.isPictureInPictureMode.removeObserver(mPictureInPictureObserver);
    }

    private void onCoGuestChange(List<TUIRoomDefine.UserInfo> coGuestList) {
        initUserNameView();
        updateVisibility();
    }

    private void onCoHostChange(List<ConnectionUser> connectionUsers) {
        initUserNameView();
        updateVisibility();
    }

    private void updateVisibility() {
        if (Boolean.TRUE.equals(mMediaState.isPictureInPictureMode.getValue())) {
            setVisibility(GONE);
        } else {
            setVisibility(VISIBLE);
        }
    }

    private boolean isShowUserInfo() {
        if (mAudienceManager.getCoreState() != null && mAudienceManager.getCoreState().coHostState != null
                && mAudienceManager.getCoreState().coHostState.connectedUserList != null
                && mAudienceManager.getCoreState().coHostState.connectedUserList.getValue() != null
                && mAudienceManager.getCoreState().coHostState.connectedUserList.getValue().size() > 1) {
            return true;
        }
        if (mAudienceManager.getCoreState() != null && mAudienceManager.getCoreState().coGuestState != null
                && mAudienceManager.getCoreState().coGuestState.connectedUserList != null
                && mAudienceManager.getCoreState().coGuestState.connectedUserList.getValue() != null
                && mAudienceManager.getCoreState().coGuestState.connectedUserList.getValue().size() > 1) {
            return true;
        }
        return false;
    }

    private void onPictureInPictureObserver(Boolean isPipMode) {
        updateVisibility();
    }
}
