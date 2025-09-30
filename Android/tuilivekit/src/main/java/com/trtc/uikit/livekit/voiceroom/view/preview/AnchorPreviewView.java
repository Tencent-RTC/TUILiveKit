package com.trtc.uikit.livekit.voiceroom.view.preview;

import static com.trtc.uikit.livekit.common.ConstantsKt.TEMPLATE_ID_VOICE_ROOM;

import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUIThemeManager;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

public class AnchorPreviewView extends BasicView {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("AnchorPreviewView");
    private boolean mIsExit = false;
    private ImageView mImageKaraokeView;
    private static final String EVENT_KEY_TIME_LIMIT = "RTCRoomTimeLimitService";
    private static final String EVENT_SUB_KEY_COUNTDOWN_START = "CountdownStart";
    private static final String EVENT_SUB_KEY_COUNTDOWN_END = "CountdownEnd";
    private final Observer<RoomState.LayoutType> mVoiceRoomLayoutObserver = this::onVoiceRoomLayoutChanged;


    public AnchorPreviewView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorPreviewView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorPreviewView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void addObserver() {
        mRoomState.layoutType.observeForever(mVoiceRoomLayoutObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.layoutType.removeObserver(mVoiceRoomLayoutObserver);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_preview, this, true);
        findViewById(R.id.iv_back).setOnClickListener((view -> {
            mIsExit = true;
            ((Activity) mContext).finish();
        }));
        findViewById(R.id.btn_start_live).setOnClickListener((view) -> {
            createRoom(view);

        });
    }

    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        super.init(voiceRoomManager);
        initLiveInfoEditView();
        initFunctionView();
        initKTVView();
    }

    private void initLiveInfoEditView() {
        LiveInfoEditView liveStreamSettingsCard = findViewById(R.id.rl_live_info_edit_view);
        liveStreamSettingsCard.init(mVoiceRoomManager);
    }

    private void initFunctionView() {
        AnchorPreviewFunctionView functionView = findViewById(R.id.rl_function);
        functionView.init(mVoiceRoomManager, mSeatGridView);
    }

    private void initKTVView() {
        mImageKaraokeView = findViewById(R.id.iv_ktv);
        if (Locale.SIMPLIFIED_CHINESE.getLanguage().equals(TUIThemeManager.getInstance().getCurrentLanguage())) {
            mImageKaraokeView.setImageResource(R.drawable.karaoke_preview_song_request_zh);
        } else if (Locale.TRADITIONAL_CHINESE.getLanguage().equals(TUIThemeManager.getInstance().getCurrentLanguage())) {
            mImageKaraokeView.setImageResource(R.drawable.karaoke_preview_song_request_tw);
        } else {
            mImageKaraokeView.setImageResource(R.drawable.karaoke_preview_song_request_en);
        }
    }

    private void createRoom(View view) {
        if (!view.isEnabled()) {
            return;
        }
        view.setEnabled(false);
        RoomState roomState = mVoiceRoomManager.getRoomState();
        TUILiveListManager.LiveInfo liveInfo = new TUILiveListManager.LiveInfo();
        liveInfo.isSeatEnabled = true;
        liveInfo.keepOwnerOnSeat = true;
        liveInfo.seatLayoutTemplateId = TEMPLATE_ID_VOICE_ROOM;
        liveInfo.roomId = roomState.roomId;
        liveInfo.name = roomState.roomName.getValue();
        liveInfo.maxSeatCount = roomState.maxSeatCount.getValue() == null ? 9 : roomState.maxSeatCount.getValue();
        liveInfo.seatMode = roomState.seatMode.getValue();
        liveInfo.backgroundUrl = roomState.backgroundURL.getValue();
        liveInfo.coverUrl = roomState.coverURL.getValue();
        liveInfo.isPublicVisible = roomState.liveExtraInfo.liveMode.getValue() == RoomState.LiveStreamPrivacyStatus.PUBLIC;
        mSeatGridView.startVoiceRoom(liveInfo, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfo liveInfo) {
                if (mIsExit) {
                    TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_END, null);
                    mSeatGridView.stopVoiceRoom((TUILiveListManager.StopLiveCallback) null);
                    return;
                }
                LOGGER.info("create room success");
                mVoiceRoomManager.getRoomManager().updateRoomState(liveInfo);
                mVoiceRoomManager.getUserManager().getAudienceList();
                mVoiceRoomManager.getUserManager().updateOwnerUserInfo();
                mVoiceRoomManager.getSeatManager().getSeatList();
                mVoiceRoomManager.getRoomManager().updateLiveStatus(RoomState.LiveStatus.PUSHING);
                showAlertUserLiveTips();
                TUICore.notifyEvent(EVENT_KEY_TIME_LIMIT, EVENT_SUB_KEY_COUNTDOWN_START, null);
                view.setEnabled(true);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error(" create room failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
                view.setEnabled(true);
            }
        });
    }

    private void showAlertUserLiveTips() {
        try {
            Map<String, Object> map = new HashMap<>();
            map.put(TUIConstants.Privacy.PARAM_DIALOG_CONTEXT, Objects.requireNonNull(getContext()));
            TUICore.notifyEvent(TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED,
                    TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_START, map);
        } catch (Exception e) {
            LOGGER.error("showAlertUserLiveTips exception:" + e.getMessage());
        }
    }

    private void onVoiceRoomLayoutChanged(RoomState.LayoutType layoutType) {
        if (mImageKaraokeView != null) {
            mImageKaraokeView.setVisibility(layoutType == RoomState.LayoutType.VoiceRoom ? GONE : VISIBLE);
        }
    }
}
