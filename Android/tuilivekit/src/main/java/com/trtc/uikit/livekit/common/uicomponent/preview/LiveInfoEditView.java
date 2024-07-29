package com.trtc.uikit.livekit.common.uicomponent.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;

import java.util.Arrays;
import java.util.List;

@SuppressLint("ViewConstructor")
public class LiveInfoEditView extends BasicView {

    private       EditText                                     mEditRoomName;
    private       TextView                                     mTextStreamCategory;
    private       TextView                                     mTextStreamPrivacyStatus;
    private       ImageView               mImageStreamCover;
    private       StreamPresetImagePicker mStreamPresetImagePicker;
    private final Observer<String>        mLiveCoverObserver         = this::onLiveCoverChange;
    private final Observer<String>                             mLiveCategoryObserver      = this::onLiveCategoryChange;
    private final Observer<LiveDefine.LiveStreamPrivacyStatus> mLivePrivacyStatusObserver =
            this::onLivePrivacyStatusChange;


    public LiveInfoEditView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_preview_live_info_edit,
                this, true);
        bindViewId();

        initLiveNameEditText();
        initLiveCoverPicker();
        initLiveTypePicker();
        initLivePrivacyStatusPicker();
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

    private void bindViewId() {
        mImageStreamCover = findViewById(R.id.iv_cover);
        mEditRoomName = findViewById(R.id.et_stream_name);
        mTextStreamCategory = findViewById(R.id.tv_stream_category);
        mTextStreamPrivacyStatus = findViewById(R.id.tv_stream_privacy_status);
    }

    private void initLiveCoverPicker() {
        List<String> dataList = getDataList(getContext());
        mLiveController.getRoomController().setLiveCategoryList(dataList);
        mLiveController.getRoomController().setLiveCategory(dataList.get(0));
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
                mStreamPresetImagePicker.setOnItemClickListener(imageUrl
                        -> mLiveController.getRoomController().setCoverURL(imageUrl));
            }
            mStreamPresetImagePicker.show();
        });
    }

    private void initLiveNameEditText() {
        String roomName = mLiveController.getRoomController().getDefaultRoomName();
        mEditRoomName.setText(roomName);
        mLiveController.getRoomController().setRoomName(roomName);
        mEditRoomName.addTextChangedListener(new TextWatcher() {

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mLiveController.getRoomController().setRoomName(s.toString());
            }
        });
    }

    private void initLiveTypePicker() {
        findViewById(R.id.ll_stream_category).setOnClickListener(view -> {
            StreamCategoryPicker picker = new StreamCategoryPicker(mContext, mLiveController);
            picker.show();
        });
    }

    private void initLivePrivacyStatusPicker() {
        findViewById(R.id.ll_stream_privacy_status).setOnClickListener(view -> {
            StreamPrivacyStatusPicker picker = new StreamPrivacyStatusPicker(mContext, mLiveController);
            picker.show();
        });
    }

    private void onLiveCoverChange(String coverURL) {
        ImageLoader.load(mContext, mImageStreamCover, coverURL, R.drawable.livekit_live_stream_default_cover);
    }

    private void onLiveCategoryChange(String category) {
        if (TextUtils.isEmpty(category)) {
            mTextStreamCategory.setText(mContext.getString(R.string.livekit_stream_categories_default));
        } else {
            mTextStreamCategory.setText(category);
        }
    }

    private void onLivePrivacyStatusChange(LiveDefine.LiveStreamPrivacyStatus status) {
        mTextStreamPrivacyStatus.setText(status.resId);
    }

    private List<String> getDataList(Context context) {
        return Arrays.asList(context.getResources().getStringArray(R.array.livekit_stream_category_list));
    }
}
