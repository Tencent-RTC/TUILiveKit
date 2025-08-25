package com.trtc.uikit.livekit.features.endstatistics;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.lifecycle.Observer;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.endstatistics.manager.EndStatisticsManager;
import com.trtc.uikit.livekit.features.endstatistics.state.EndStatisticsState;

public class AudienceEndStatisticsView extends FrameLayout {
    private final LiveKitLogger        LOGGER   = LiveKitLogger.getFeaturesLogger("AudienceEndStatisticsView");
    private final EndStatisticsManager mManager = new EndStatisticsManager();
    private final EndStatisticsState   mState   = mManager.getState();

    private final Observer<String> mOwnerNameObserver      = this::onOwnerNameChange;
    private final Observer<String> mOwnerAvatarUrlObserver = this::onOwnerAvatarUrlChange;

    private TextView        mTextName;
    private ImageFilterView mImageHead;

    private EndStatisticsDefine.AudienceEndStatisticsViewListener mListener;

    public AudienceEndStatisticsView(@NonNull Context context) {
        this(context, null);
    }

    public AudienceEndStatisticsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        initView();
    }

    private void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_audience_dashboard_view, this, true);
        mTextName = findViewById(R.id.tv_name);
        mImageHead = findViewById(R.id.iv_head);
        findViewById(R.id.iv_back).setOnClickListener(v -> onExitClick());
    }

    public void init(String roomId, String ownerName, String ownerAvatarUrl) {
        mManager.setRoomId(TextUtils.isEmpty(roomId) ? "" : roomId);
        mManager.setOwnerName(TextUtils.isEmpty(ownerName) ? "" : ownerName);
        mManager.setOwnerAvatarUrl(TextUtils.isEmpty(ownerAvatarUrl) ? "" : ownerAvatarUrl);
        LOGGER.info("init, " + mState.toString());
    }

    public void setListener(EndStatisticsDefine.AudienceEndStatisticsViewListener listener) {
        mListener = listener;
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

    private void addObserver() {
        mState.ownerName.observeForever(mOwnerNameObserver);
        mState.ownerAvatarUrl.observeForever(mOwnerAvatarUrlObserver);
    }

    private void removeObserver() {
        mState.ownerName.removeObserver(mOwnerNameObserver);
        mState.ownerAvatarUrl.removeObserver(mOwnerAvatarUrlObserver);
    }

    private void onExitClick() {
        if (mListener != null) {
            mListener.onCloseButtonClick();
        }
    }

    private void onOwnerNameChange(String name) {
        mTextName.setText(name);
    }

    private void onOwnerAvatarUrlChange(String url) {
        String avatarUrl = TextUtils.isEmpty(url) ? null : url;
        ImageLoader.load(getContext(), mImageHead, avatarUrl, R.drawable.livekit_ic_avatar);
    }
}
