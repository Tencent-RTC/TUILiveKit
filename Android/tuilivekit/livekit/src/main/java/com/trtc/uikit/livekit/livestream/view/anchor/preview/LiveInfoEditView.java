package com.trtc.uikit.livekit.livestream.view.anchor.preview;

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

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.RoomState.LiveStreamPrivacyStatus;
import com.trtc.uikit.livekit.livestream.view.anchor.preview.dialog.StreamCategoryPicker;
import com.trtc.uikit.livekit.livestream.view.anchor.preview.dialog.StreamPresetImagePicker;
import com.trtc.uikit.livekit.livestream.view.anchor.preview.dialog.StreamPrivacyStatusPicker;

import java.util.Arrays;
import java.util.List;

public class LiveInfoEditView extends FrameLayout {
    public static final String[] COVER_URL_LIST = {
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover2.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover3.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover4.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover5.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover6.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover7.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover8.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover9.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover10.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover11.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover12.png",};

    private       EditText                          mEditRoomName;
    private       TextView                          mTextStreamCategory;
    private       TextView                          mTextStreamPrivacyStatus;
    private       ImageView                         mImageStreamCover;
    private       StreamPresetImagePicker           mStreamPresetImagePicker;
    private       LiveStreamManager                 mLiveManager;
    private       RoomState                         mRoomState;
    private final Observer<String>                  mLiveCoverObserver         = this::onLiveCoverChange;
    private final Observer<String>                  mLiveCategoryObserver      = this::onLiveCategoryChange;
    private final Observer<LiveStreamPrivacyStatus> mLivePrivacyStatusObserver = this::onLivePrivacyStatusChange;

    public LiveInfoEditView(@NonNull Context context) {
        this(context, null);
    }

    public LiveInfoEditView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LiveInfoEditView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_preview_live_info_edit, this, true);
    }


    public void init(LiveStreamManager manager) {
        mLiveManager = manager;
        mRoomState = manager.getRoomState();

        initView();
        addObserver();
    }

    protected void initView() {
        bindViewId();

        initLiveNameEditText();
        initLiveCoverPicker();
        initLiveTypePicker();
        initLivePrivacyStatusPicker();
    }

    protected void addObserver() {
        mRoomState.coverURL.observe(mLiveCoverObserver);
        mRoomState.category.observe(mLiveCategoryObserver);
        mRoomState.liveMode.observe(mLivePrivacyStatusObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    protected void removeObserver() {
        if (mRoomState == null) {
            return;
        }
        mRoomState.coverURL.removeObserver(mLiveCoverObserver);
        mRoomState.category.removeObserver(mLiveCategoryObserver);
        mRoomState.liveMode.removeObserver(mLivePrivacyStatusObserver);
    }

    private void bindViewId() {
        mImageStreamCover = findViewById(R.id.iv_cover);
        mEditRoomName = findViewById(R.id.et_stream_name);
        mTextStreamCategory = findViewById(R.id.tv_stream_category);
        mTextStreamPrivacyStatus = findViewById(R.id.tv_stream_privacy_status);
    }

    private void initLiveCoverPicker() {
        List<String> dataList = getDataList(getContext());
        mLiveManager.getRoomManager().setLiveCategoryList(dataList);
        mLiveManager.getRoomManager().setLiveCategory(dataList.get(0));
        mTextStreamCategory.setText(mRoomState.category.get());
        View coverSettingsLayout = findViewById(R.id.fl_cover_edit);
        ImageLoader.load(getContext(), mImageStreamCover, mRoomState.coverURL.get(),
                R.drawable.livekit_live_stream_default_cover);
        coverSettingsLayout.setOnClickListener(view -> {
            if (mStreamPresetImagePicker == null) {
                StreamPresetImagePicker.Config config = new StreamPresetImagePicker.Config();
                config.title = getContext().getString(R.string.livekit_titile_preset_cover);
                config.confirmButtonText = getContext().getString(R.string.livekit_set_as_cover);
                config.data = Arrays.asList(COVER_URL_LIST);
                config.currentImageUrl = mRoomState.coverURL.get();
                mStreamPresetImagePicker = new StreamPresetImagePicker(getContext(), config);
                mStreamPresetImagePicker.setOnItemClickListener(imageUrl
                        -> mLiveManager.getRoomManager().setCoverURL(imageUrl));
            }
            mStreamPresetImagePicker.show();
        });
    }

    private void initLiveNameEditText() {
        String roomName = mLiveManager.getRoomManager().getDefaultRoomName();
        mEditRoomName.setText(roomName);
        mLiveManager.getRoomManager().setRoomName(roomName);
        mEditRoomName.addTextChangedListener(new TextWatcher() {

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mLiveManager.getRoomManager().setRoomName(s.toString());
            }
        });
    }

    private void initLiveTypePicker() {
        findViewById(R.id.ll_stream_category).setOnClickListener(view -> {
            StreamCategoryPicker picker = new StreamCategoryPicker(getContext(), mLiveManager);
            picker.show();
        });
    }

    private void initLivePrivacyStatusPicker() {
        findViewById(R.id.ll_stream_privacy_status).setOnClickListener(view -> {
            StreamPrivacyStatusPicker picker = new StreamPrivacyStatusPicker(getContext(), mLiveManager);
            picker.show();
        });
    }

    private void onLiveCoverChange(String coverURL) {
        ImageLoader.load(getContext(), mImageStreamCover, coverURL, R.drawable.livekit_live_stream_default_cover);
    }

    private void onLiveCategoryChange(String category) {
        if (TextUtils.isEmpty(category)) {
            mTextStreamCategory.setText(getContext().getString(R.string.livekit_stream_categories_default));
        } else {
            mTextStreamCategory.setText(category);
        }
    }

    private void onLivePrivacyStatusChange(LiveStreamPrivacyStatus status) {
        mTextStreamPrivacyStatus.setText(status.resId);
    }

    private List<String> getDataList(Context context) {
        return Arrays.asList(context.getResources().getStringArray(R.array.livekit_stream_category_list));
    }
}
