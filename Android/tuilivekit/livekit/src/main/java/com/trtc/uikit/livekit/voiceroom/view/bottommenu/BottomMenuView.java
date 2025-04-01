package com.trtc.uikit.livekit.voiceroom.view.bottommenu;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.component.barrage.BarrageInputView;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;

public class BottomMenuView extends BasicView {
    private BarrageInputView mBarrageInputView;
    private View             mMicrophoneContainer;
    private ImageView        mMicrophoneButton;

    private final Observer<SeatState.LinkStatus> mLinkStateObserver = this::onLinkStateChanged;

    private final Observer<Boolean> mMicrophoneMutedObserver = this::updateMicrophoneButton;

    public BottomMenuView(@NonNull Context context) {
        this(context, null);
    }

    public BottomMenuView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BottomMenuView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_layout_bottom_menu, this, true);
        mBarrageInputView = findViewById(R.id.rl_barrage_button);
        mMicrophoneContainer = findViewById(R.id.microphone_container);
        mMicrophoneButton = findViewById(R.id.iv_microphone);
        mMicrophoneButton.setOnClickListener(v -> onMicrophoneButtonClick());
    }

    @Override
    protected void addObserver() {
        mSeatState.linkStatus.observeForever(mLinkStateObserver);
        mSeatGridView.getCoreState().mediaState.isMicrophoneMuted.observeForever(mMicrophoneMutedObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.linkStatus.removeObserver(mLinkStateObserver);
        mSeatGridView.getCoreState().mediaState.isMicrophoneMuted.removeObserver(mMicrophoneMutedObserver);
    }

    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager, SeatGridView seatGridView) {
        super.init(voiceRoomManager, seatGridView);
        BasicView functionView;
        if (mUserState.selfInfo.role.getValue() == TUIRoomDefine.Role.ROOM_OWNER) {
            functionView = new AnchorFunctionView(mContext);
        } else {
            functionView = new AudienceFunctionView(mContext);
        }
        functionView.init(mVoiceRoomManager, mSeatGridView);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        RelativeLayout functionContainer = findViewById(R.id.function_container);
        functionContainer.removeAllViews();
        functionContainer.addView(functionView, layoutParams);
        mBarrageInputView.init(mRoomState.roomId);
    }

    private void onLinkStateChanged(SeatState.LinkStatus linkStatus) {
        mMicrophoneContainer.setVisibility(linkStatus == SeatState.LinkStatus.LINKING ? VISIBLE : INVISIBLE);
    }

    private void onMicrophoneButtonClick() {
        boolean isMicrophoneOpened = Boolean.TRUE.equals(mSeatGridView.getCoreState().mediaState.isMicrophoneOpened.getValue());
        if (!isMicrophoneOpened) {
            openLocalMicrophone();
            return;
        }
        if (Boolean.TRUE.equals(mSeatGridView.getCoreState().mediaState.isMicrophoneMuted.getValue())) {
            unMuteMicrophone();
        } else {
            muteMicrophone();
        }
    }

    private void openLocalMicrophone() {
        mSeatGridView.startMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorLocalized.onError(error);
            }
        });
    }

    private void muteMicrophone() {
        mSeatGridView.muteMicrophone();
    }

    private void unMuteMicrophone() {
        mSeatGridView.unmuteMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorLocalized.onError(error);
            }
        });
    }

    private void updateMicrophoneButton(boolean isMicrophoneMuted) {
        mMicrophoneButton.setImageResource(isMicrophoneMuted ? R.drawable.livekit_ic_mic_closed :
                R.drawable.livekit_ic_mic_opened);
    }
}

