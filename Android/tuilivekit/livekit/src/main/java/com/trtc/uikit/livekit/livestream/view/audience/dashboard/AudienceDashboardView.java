package com.trtc.uikit.livekit.livestream.view.audience.dashboard;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.view.BasicView;

@SuppressLint("ViewConstructor")
public class AudienceDashboardView extends BasicView {

    private ImageView       mImageBack;
    private ImageFilterView mImageHead;
    private TextView        mTextName;

    public AudienceDashboardView(@NonNull Context context) {
        this(context, null);
    }

    public AudienceDashboardView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceDashboardView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @SuppressLint("DefaultLocale")
    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_audience_dashboard_view, this, true);
        mImageBack = findViewById(R.id.iv_back);
        mImageHead = findViewById(R.id.iv_head);
        mTextName = findViewById(R.id.tv_name);
    }

    @Override
    protected void refreshView() {
        initUserNameView();
        initUserAvatarView();
        initBackView();
    }

    private void initBackView() {
        mImageBack.setOnClickListener(view -> {
            mRoomManager.updateLiveStatus(RoomState.LiveStatus.NONE);
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        });
    }

    private void initUserAvatarView() {

        if (TextUtils.isEmpty(mRoomState.ownerInfo.avatarUrl.getValue())) {
            mImageHead.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImageHead, mRoomState.ownerInfo.avatarUrl.getValue(), R.drawable.livekit_ic_avatar);
        }
    }

    private void initUserNameView() {
        mTextName.setText(TextUtils.isEmpty(mRoomState.ownerInfo.name.getValue()) ? mRoomState.ownerInfo.userId :
                mRoomState.ownerInfo.name.getValue());
    }


    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }
}
