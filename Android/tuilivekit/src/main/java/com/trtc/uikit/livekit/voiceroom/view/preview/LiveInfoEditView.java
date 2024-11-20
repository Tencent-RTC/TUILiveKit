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

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.api.Constants;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

import java.util.Arrays;
import java.util.List;

public class LiveInfoEditView extends BasicView {

    private       EditText                                    mEditRoomName;
    private       TextView                                    mTextStreamCategory;
    private       TextView                                    mTextStreamPrivacyStatus;
    private       ImageView                                   mImageStreamCover;
    private       StreamPresetImagePicker                     mStreamPresetImagePicker;
    private final Observer<String>                            mLiveCoverObserver         = this::onLiveCoverChanged;
    private final Observer<String>                            mLiveCategoryObserver      = this::onLiveCategoryChanged;
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
        initLiveTypePicker();
        mImageStreamCover = findViewById(R.id.iv_cover);
        mTextStreamCategory = findViewById(R.id.tv_stream_category);
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
            public void afterTextChanged(Editable s) {
                mVoiceRoomManager.getRoomManager().setRoomName(s.toString());
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
        mRoomState.coverURL.observe(mLiveCoverObserver);
        mRoomState.liveExtraInfo.category.observe(mLiveCategoryObserver);
        mRoomState.liveExtraInfo.liveMode.observe(mLivePrivacyStatusObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.coverURL.removeObserver(mLiveCoverObserver);
        mRoomState.liveExtraInfo.category.removeObserver(mLiveCategoryObserver);
        mRoomState.liveExtraInfo.liveMode.removeObserver(mLivePrivacyStatusObserver);
    }

    private void initLiveCoverPicker() {
        List<String> dataList = getDataList(getContext());
        mVoiceRoomManager.getRoomManager().setLiveCategoryList(dataList);
        mVoiceRoomManager.getRoomManager().setLiveCategory(dataList.get(0));
        mTextStreamCategory.setText(mRoomState.liveExtraInfo.category.get());
        View coverSettingsLayout = findViewById(R.id.fl_cover_edit);
        ImageLoader.load(mContext, mImageStreamCover, mRoomState.coverURL.get(),
                R.drawable.livekit_live_stream_default_cover);
        coverSettingsLayout.setOnClickListener(view -> {
            if (mStreamPresetImagePicker == null) {
                StreamPresetImagePicker.Config config = new StreamPresetImagePicker.Config();
                config.title = mContext.getString(R.string.livekit_titile_preset_cover);
                config.confirmButtonText = mContext.getString(R.string.livekit_set_as_cover);
                config.data = Arrays.asList(Constants.COVER_URL_LIST);
                config.currentImageUrl = mRoomState.coverURL.get();
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

    private void initLiveTypePicker() {
        findViewById(R.id.ll_stream_category).setOnClickListener(view -> {
            StreamCategoryPicker picker = new StreamCategoryPicker(mContext, mVoiceRoomManager);
            picker.show();
        });
    }

    private void initLivePrivacyStatusPicker() {
        findViewById(R.id.ll_stream_privacy_status).setOnClickListener(view -> {
            StreamPrivacyStatusPicker picker = new StreamPrivacyStatusPicker(mContext, mVoiceRoomManager);
            picker.show();
        });
    }

    private void onLiveCoverChanged(String coverURL) {
        ImageLoader.load(mContext, mImageStreamCover, coverURL, R.drawable.livekit_live_stream_default_cover);
    }

    private void onLiveCategoryChanged(String category) {
        if (TextUtils.isEmpty(category)) {
            mTextStreamCategory.setText(mContext.getString(R.string.livekit_stream_categories_default));
        } else {
            mTextStreamCategory.setText(category);
        }
    }

    private void onLivePrivacyStatusChange(RoomState.LiveStreamPrivacyStatus status) {
        mTextStreamPrivacyStatus.setText(status.resId);
    }

    private List<String> getDataList(Context context) {
        return Arrays.asList(context.getResources().getStringArray(R.array.livekit_stream_category_list));
    }
}
