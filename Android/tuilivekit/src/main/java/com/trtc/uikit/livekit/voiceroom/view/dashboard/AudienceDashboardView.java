package com.trtc.uikit.livekit.voiceroom.view.dashboard;

import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

public class AudienceDashboardView extends BasicView {

    private TextView        mTextName;
    private ImageFilterView mImageHead;

    private final Observer<String> mOwnerNameObserver = (name) -> mTextName.setText(name);

    private final Observer<String> mOwnerAvatarObserver = (avatar) -> {
        if (TextUtils.isEmpty(avatar)) {
            mImageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImageHead, avatar, R.drawable.livekit_ic_avatar);
        }
    };

    public AudienceDashboardView(@NonNull Context context) {
        this(context, null);
    }

    public AudienceDashboardView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceDashboardView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_audience_dashboard_view, this,
                true);
        mTextName = findViewById(R.id.tv_name);
        mImageHead = findViewById(R.id.iv_head);
        findViewById(R.id.iv_back).setOnClickListener(view -> {
            mVoiceRoomManager.getRoomManager().clearLiveState();
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        });
    }

    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        super.init(voiceRoomManager);
        UserState.UserInfo ownerInfo = mRoomState.ownerInfo;
        mTextName.setText(TextUtils.isEmpty(ownerInfo.name.get()) ? ownerInfo.userId : ownerInfo.name.get());
        if (TextUtils.isEmpty(ownerInfo.avatarUrl.get())) {
            mImageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImageHead, ownerInfo.avatarUrl.get(), R.drawable.livekit_ic_avatar);
        }
    }

    @Override
    protected void addObserver() {
        mRoomState.ownerInfo.name.observe(mOwnerNameObserver);
        mRoomState.ownerInfo.avatarUrl.observe(mOwnerAvatarObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.ownerInfo.name.removeObserver(mOwnerNameObserver);
        mRoomState.ownerInfo.avatarUrl.removeObserver(mOwnerAvatarObserver);
    }
}
