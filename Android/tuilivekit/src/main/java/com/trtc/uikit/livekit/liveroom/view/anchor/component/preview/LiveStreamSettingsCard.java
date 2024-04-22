package com.trtc.uikit.livekit.liveroom.view.anchor.component.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

@SuppressLint("ViewConstructor")
public class LiveStreamSettingsCard extends ConstraintLayout {

    private       Context                                      mContext;
    private       EditText                                     mEditStreamName;
    private       TextView                                     mTextStreamCategory;
    private       TextView                                     mTextStreamPrivacyStatus;
    private       ImageView                                    mImageStreamCover;
    private final LiveRoomInfo                                 mLiveRoomInfo;
    private final Observer<String>                             mStreamCoverObserver         =
            coverURL -> ImageLoader.load(mContext, mImageStreamCover, coverURL,
                    R.drawable.livekit_live_stream_default_cover);
    private final Observer<String>                             mStreamCategoryObserver      =
            category -> mTextStreamCategory.setText(category);
    private final Observer<LiveDefine.LiveStreamPrivacyStatus> mStreamPrivacyStatusObserver =
            status -> mTextStreamPrivacyStatus.setText(status.resId);


    public LiveStreamSettingsCard(@NonNull Context context, LiveRoomInfo roomInfo) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;

        View rootView = LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_preview_settings_card,
                this, true);
        initStreamNameEditText(rootView);
        initStreamCoverPicker(rootView);
        initStreamCategoryPicker(rootView);
        initStreamPrivacyStatusPicker(rootView);

    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mLiveRoomInfo.coverURL.observe(mStreamCoverObserver);
        mLiveRoomInfo.category.observe(mStreamCategoryObserver);
        mLiveRoomInfo.liveMode.observe(mStreamPrivacyStatusObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mLiveRoomInfo.coverURL.removeObserver(mStreamCoverObserver);
        mLiveRoomInfo.category.removeObserver(mStreamCategoryObserver);
        mLiveRoomInfo.liveMode.removeObserver(mStreamPrivacyStatusObserver);
    }

    private void initStreamCoverPicker(View rootView) {
        View coverSettingsLayout = rootView.findViewById(R.id.cover_settings);
        mImageStreamCover = rootView.findViewById(R.id.iv_cover);
        ImageLoader.load(mContext, mImageStreamCover, mLiveRoomInfo.coverURL.get(),
                R.drawable.livekit_live_stream_default_cover);
        coverSettingsLayout.setOnClickListener(view -> {
            StreamPresetCoverPicker picker = new StreamPresetCoverPicker(mContext, mLiveRoomInfo);
            picker.show();
        });
    }

    private void initStreamNameEditText(View rootView) {
        mEditStreamName = rootView.findViewById(R.id.et_stream_name);
        mEditStreamName.setText(!TextUtils.isEmpty(mLiveRoomInfo.name.get()) ? mLiveRoomInfo.name.get() :
                TextUtils.isEmpty(mLiveRoomInfo.anchorInfo.name.get()) ? mLiveRoomInfo.anchorInfo.userId :
                        mLiveRoomInfo.anchorInfo.name.get());
    }

    private void initStreamCategoryPicker(View rootView) {
        mTextStreamCategory = rootView.findViewById(R.id.tv_stream_category);
        findViewById(R.id.ll_stream_category).setOnClickListener(view -> {
            StreamCategoryPicker picker = new StreamCategoryPicker(mContext, mLiveRoomInfo);
            picker.show();
        });
    }

    private void initStreamPrivacyStatusPicker(View rootView) {
        mTextStreamPrivacyStatus = rootView.findViewById(R.id.tv_stream_privacy_status);
        findViewById(R.id.ll_stream_privacy_status).setOnClickListener(view -> {
            StreamPrivacyStatusPicker picker = new StreamPrivacyStatusPicker(mContext, mLiveRoomInfo);
            picker.show();
        });
    }

    public String getStreamId() {
        return mEditStreamName.getText().toString().trim();
    }
}
