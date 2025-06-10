package com.trtc.uikit.livekit.voiceroom.view.preview;

import android.content.Context;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.api.Constants;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

import java.nio.charset.Charset;
import java.util.Arrays;

public class LiveInfoEditView extends BasicView {

    private static final int MAX_INPUT_BYTE_LENGTH = 100;

    private       EditText                                    mEditRoomName;
    private       TextView                                    mTextStreamPrivacyStatus;
    private       ImageView                                   mImageStreamCover;
    private       StreamPresetImagePicker                     mStreamPresetImagePicker;
    private final Observer<String>                            mLiveCoverObserver         = this::onLiveCoverChanged;
    private final Observer<RoomState.LiveStreamPrivacyStatus> mLivePrivacyStatusObserver =
            this::onLivePrivacyStatusChange;

    public LiveInfoEditView(@NonNull Context context) {
        this(context, null);
    }

    public LiveInfoEditView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LiveInfoEditView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_preview_live_info_edit,
                this, true);
        initLivePrivacyStatusPicker();
        mImageStreamCover = findViewById(R.id.iv_cover);
        mTextStreamPrivacyStatus = findViewById(R.id.tv_stream_privacy_status);
        mEditRoomName = findViewById(R.id.et_stream_name);
        mEditRoomName.addTextChangedListener(new TextWatcher() {

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable editable) {
                if (TextUtils.isEmpty(editable)) {
                    return;
                }
                String newString = editable.toString();
                if (!checkLength(editable.toString())) {
                    for (int i = editable.length(); i > 0; i--) {
                        String s = editable.subSequence(0, i).toString();
                        if (checkLength(s)) {
                            newString = s;
                            mEditRoomName.setText(s);
                            mEditRoomName.setSelection(s.length());
                            break;
                        }
                    }
                }
                mVoiceRoomManager.getRoomManager().setRoomName(newString);
            }

            private boolean checkLength(String s) {
                return s.getBytes(Charset.defaultCharset()).length <= MAX_INPUT_BYTE_LENGTH;
            }
        });
    }

    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        super.init(voiceRoomManager);
        initLiveNameEditText();
        initLiveCoverPicker();
    }

    @Override
    protected void addObserver() {
        mRoomState.coverURL.observeForever(mLiveCoverObserver);
        mRoomState.liveExtraInfo.liveMode.observeForever(mLivePrivacyStatusObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.coverURL.removeObserver(mLiveCoverObserver);
        mRoomState.liveExtraInfo.liveMode.removeObserver(mLivePrivacyStatusObserver);
    }

    private void initLiveCoverPicker() {
        View coverSettingsLayout = findViewById(R.id.fl_cover_edit);
        ImageLoader.load(mContext, mImageStreamCover, mRoomState.coverURL.getValue(),
                R.drawable.anchor_prepare_live_stream_default_cover);
        coverSettingsLayout.setOnClickListener(view -> {
            if (mStreamPresetImagePicker == null) {
                StreamPresetImagePicker.Config config = new StreamPresetImagePicker.Config();
                config.title = mContext.getString(R.string.common_title_preset_cover);
                config.confirmButtonText = mContext.getString(R.string.common_set_as_cover);
                config.data = Arrays.asList(Constants.COVER_URL_LIST);
                config.currentImageUrl = mRoomState.coverURL.getValue();
                mStreamPresetImagePicker = new StreamPresetImagePicker(mContext, config);
                mStreamPresetImagePicker.setOnConfirmListener(imageUrl
                        -> mVoiceRoomManager.getRoomManager().setCoverURL(imageUrl));
            }
            mStreamPresetImagePicker.show();
        });
    }

    private void initLiveNameEditText() {
        String roomName = mVoiceRoomManager.getRoomManager().getDefaultRoomName();
        mEditRoomName.setText(roomName);
        mVoiceRoomManager.getRoomManager().setRoomName(roomName);
    }

    private void initLivePrivacyStatusPicker() {
        findViewById(R.id.ll_stream_privacy_status).setOnClickListener(view -> {
            StreamPrivacyStatusPicker picker = new StreamPrivacyStatusPicker(mContext, mVoiceRoomManager);
            picker.show();
        });
    }

    private void onLiveCoverChanged(String coverURL) {
        ImageLoader.load(mContext, mImageStreamCover, coverURL, R.drawable.anchor_prepare_live_stream_default_cover);
    }

    private void onLivePrivacyStatusChange(RoomState.LiveStreamPrivacyStatus status) {
        mTextStreamPrivacyStatus.setText(status.resId);
    }
}
