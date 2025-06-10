package com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit;

import static com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareState.COVER_URL_LIST;
import static com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareState.MAX_INPUT_BYTE_LENGTH;

import android.content.Context;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine.LiveStreamPrivacyStatus;
import com.trtc.uikit.livekit.features.anchorprepare.manager.AnchorPrepareManager;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareState;
import com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit.livecoverpicker.LiveCoverPicker;

import java.nio.charset.Charset;
import java.util.Arrays;

public class LiveInfoEditView extends FrameLayout {

    private       AnchorPrepareManager              mManager;
    private       AnchorPrepareState                mState;
    private       EditText                          mEditRoomName;
    private       TextView                          mTextStreamPrivacyStatus;
    private       ImageView                         mImageStreamCover;
    private       LiveCoverPicker                   mLiveCoverPicker;
    private final Observer<String>                  mLiveCoverObserver         = this::onLiveCoverChange;
    private final Observer<LiveStreamPrivacyStatus> mLivePrivacyStatusObserver = this::onLivePrivacyStatusChange;

    public LiveInfoEditView(@NonNull Context context) {
        this(context, null);
    }

    public LiveInfoEditView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LiveInfoEditView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        LayoutInflater.from(getContext()).inflate(R.layout.anchor_prepare_layout_live_info_edit_view, this, true);
    }

    // Need to be called before addView to the parent layout
    public void init(AnchorPrepareManager manager) {
        initManager(manager);
        initView();
    }

    private void initManager(AnchorPrepareManager manager) {
        mManager = manager;
        mState = manager.getState();
    }

    protected void initView() {
        bindViewId();

        initLiveNameEditText();
        initLiveCoverPicker();
        initLivePrivacyStatusPicker();
    }

    protected synchronized void addObserver() {
        if (mState == null) {
            return;
        }
        mState.coverURL.observeForever(mLiveCoverObserver);
        mState.liveMode.observeForever(mLivePrivacyStatusObserver);
    }

    public synchronized void removeObserver() {
        if (mState == null) {
            return;
        }
        mState.coverURL.removeObserver(mLiveCoverObserver);
        mState.liveMode.removeObserver(mLivePrivacyStatusObserver);
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

    private void bindViewId() {
        mImageStreamCover = findViewById(R.id.iv_cover);
        mEditRoomName = findViewById(R.id.et_stream_name);
        mTextStreamPrivacyStatus = findViewById(R.id.tv_stream_privacy_status);
    }

    private void initLiveCoverPicker() {
        View coverSettingsLayout = findViewById(R.id.fl_cover_edit);
        ImageLoader.load(getContext(), mImageStreamCover, mState.coverURL.getValue(),
                R.drawable.anchor_prepare_live_stream_default_cover);
        coverSettingsLayout.setOnClickListener(view -> {
            if (mLiveCoverPicker == null) {
                LiveCoverPicker.Config config = new LiveCoverPicker.Config();
                config.title = getContext().getString(R.string.common_title_preset_cover);
                config.confirmButtonText = getContext().getString(R.string.common_set_as_cover);
                config.data = Arrays.asList(COVER_URL_LIST);
                config.currentImageUrl = mState.coverURL.getValue();
                mLiveCoverPicker = new LiveCoverPicker(getContext(), config);
                mLiveCoverPicker.setOnItemClickListener(imageUrl
                        -> mManager.setCoverURL(imageUrl));
            }
            mLiveCoverPicker.show();
        });
    }

    private void initLiveNameEditText() {
        String roomName = mManager.getDefaultRoomName();
        mEditRoomName.setText(roomName);
        mManager.setRoomName(roomName);
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
                mManager.setRoomName(newString);
            }

            private boolean checkLength(String s) {
                return s.getBytes(Charset.defaultCharset()).length <= MAX_INPUT_BYTE_LENGTH;
            }
        });
    }

    private void initLivePrivacyStatusPicker() {
        findViewById(R.id.ll_stream_privacy_status).setOnClickListener(view -> {
            LivePrivacyStatusPicker picker = new LivePrivacyStatusPicker(getContext(), mManager);
            picker.show();
        });
    }

    private void onLiveCoverChange(String coverURL) {
        ImageLoader.load(getContext(), mImageStreamCover, coverURL,
                R.drawable.anchor_prepare_live_stream_default_cover);
    }

    private void onLivePrivacyStatusChange(LiveStreamPrivacyStatus status) {
        mTextStreamPrivacyStatus.setText(status.resId);
    }
}
