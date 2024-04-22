package com.trtc.uikit.livekit.voiceroom.view.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.view.BasicView;

@SuppressLint("ViewConstructor")
public class LiveStreamSettingsCard extends BasicView {

    private EditText  mEditRoomName;
    private TextView  mTextStreamCategory;
    private TextView  mTextStreamPrivacyStatus;
    private ImageView mImageStreamCover;

    private final Observer<String> mStreamCoverObserver = coverURL -> ImageLoader.load(mContext, mImageStreamCover,
            coverURL, R.drawable.livekit_live_stream_default_cover);

    private final Observer<String> mStreamCategoryObserver = category -> mTextStreamCategory.setText(category);

    private final Observer<LiveDefine.LiveStreamPrivacyStatus> mStreamPrivacyStatusObserver =
            status -> mTextStreamPrivacyStatus.setText(status.resId);

    public LiveStreamSettingsCard(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
        mContext = context;
        View rootView = LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_preview_settings_card,
                this, true);
        initStreamNameEditText(rootView);
        initStreamCoverPicker(rootView);
        initStreamCategoryPicker(rootView);
        initStreamPrivacyStatusPicker(rootView);

    }

    @Override
    protected void addObserver() {
        mRoomState.coverURL.observe(mStreamCoverObserver);
        mRoomState.category.observe(mStreamCategoryObserver);
        mRoomState.liveMode.observe(mStreamPrivacyStatusObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.coverURL.removeObserver(mStreamCoverObserver);
        mRoomState.category.removeObserver(mStreamCategoryObserver);
        mRoomState.liveMode.removeObserver(mStreamPrivacyStatusObserver);
    }

    @Override
    protected void initView() {

    }

    private void initStreamCoverPicker(View rootView) {
        View coverSettingsLayout = rootView.findViewById(R.id.cover_settings);
        mImageStreamCover = rootView.findViewById(R.id.iv_cover);
        ImageLoader.load(mContext, mImageStreamCover, mRoomState.coverURL.get(),
                R.drawable.livekit_live_stream_default_cover);
        coverSettingsLayout.setOnClickListener(view -> {
            StreamPresetCoverPicker picker = new StreamPresetCoverPicker(mContext, mLiveController);
            picker.show();
        });
    }

    private void initStreamNameEditText(View rootView) {
        mEditRoomName = rootView.findViewById(R.id.et_stream_name);
        mEditRoomName.setText(!TextUtils.isEmpty(mRoomState.roomName.get()) ? mRoomState.roomName.get() :
                TextUtils.isEmpty(mUserState.selfInfo.name.get()) ? mUserState.selfInfo.userId :
                        mUserState.selfInfo.name.get());
    }

    private void initStreamCategoryPicker(View rootView) {
        mTextStreamCategory = rootView.findViewById(R.id.tv_stream_category);
        findViewById(R.id.ll_stream_category).setOnClickListener(view -> {
            StreamCategoryPicker picker = new StreamCategoryPicker(mContext, mLiveController);
            picker.show();
        });
    }

    private void initStreamPrivacyStatusPicker(View rootView) {
        mTextStreamPrivacyStatus = rootView.findViewById(R.id.tv_stream_privacy_status);
        findViewById(R.id.ll_stream_privacy_status).setOnClickListener(view -> {
            StreamPrivacyStatusPicker picker = new StreamPrivacyStatusPicker(mContext, mLiveController);
            picker.show();
        });
    }

    public String getRoomName() {
        return mEditRoomName.getText().toString().trim();
    }
}
