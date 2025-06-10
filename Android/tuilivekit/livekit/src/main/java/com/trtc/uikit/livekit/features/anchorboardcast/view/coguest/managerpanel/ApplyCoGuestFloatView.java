package com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.managerpanel;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class ApplyCoGuestFloatView extends FrameLayout {

    private       LinearLayout                          mLayoutRoot;
    private       ImageView                             mImageFirstApplyLinkAudience;
    private       ImageView                             mImageSecondApplyLinkAudience;
    private       RelativeLayout                        mLayoutSecondApplyLinkAudience;
    private       RelativeLayout                        mLayoutEllipsis;
    private       TextView                              mTextApplyLinkAudienceCount;
    private       LiveCoreView                          mCoreView;
    private       AnchorManager                         mManager;
    private final Observer<Set<TUIRoomDefine.UserInfo>> mApplyLinkAudienceListObserver =
            this::onApplyLinkAudienceListChange;

    public ApplyCoGuestFloatView(@NonNull Context context) {
        this(context, null);
    }

    public ApplyCoGuestFloatView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public ApplyCoGuestFloatView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_apply_link_audience, this, true);
        mLayoutRoot = findViewById(R.id.ll_root);
        mTextApplyLinkAudienceCount = findViewById(R.id.tv_apply_link_audience_count);
        mLayoutSecondApplyLinkAudience = findViewById(R.id.rl_second_apply_link_audience);
        mLayoutEllipsis = findViewById(R.id.rl_ellipsis);
        mImageFirstApplyLinkAudience = findViewById(R.id.iv_first_apply_link_audience);
        mImageSecondApplyLinkAudience = findViewById(R.id.iv_second_apply_link_audience);
    }

    public void init(AnchorManager manager) {
        mManager = manager;
        mCoreView = mManager.getCoreView();

        initApplyLinkAudienceListView();
        initRootView();
        addObserver();
    }

    private void initRootView() {
        mLayoutRoot.setOnClickListener((view) -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            CoGuestManageDialog dialog = new CoGuestManageDialog(getContext(), mManager);
            dialog.setOnDismissListener(dialog1 -> view.setEnabled(true));
            dialog.show();
        });
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mCoreView.getCoreState().coGuestState.applicantList.observeForever(mApplyLinkAudienceListObserver);
    }

    private void removeObserver() {
        mCoreView.getCoreState().coGuestState.applicantList.removeObserver(mApplyLinkAudienceListObserver);
    }

    private void initApplyLinkAudienceListView() {
        Set<TUIRoomDefine.UserInfo> seatApplicationList =
                mCoreView.getCoreState().coGuestState.applicantList.getValue();
        if (!seatApplicationList.isEmpty()) {
            setVisibility(VISIBLE);
        } else {
            setVisibility(GONE);
        }

        final CopyOnWriteArrayList<TUIRoomDefine.UserInfo> applyLinkAudienceList =
                new CopyOnWriteArrayList<>(seatApplicationList);
        if (seatApplicationList.size() == 1) {
            mLayoutSecondApplyLinkAudience.setVisibility(GONE);
            mLayoutEllipsis.setVisibility(GONE);
            ImageLoader.load(getContext(), mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl, R.drawable.livekit_ic_avatar);
        } else if (seatApplicationList.size() == 2) {
            mLayoutSecondApplyLinkAudience.setVisibility(VISIBLE);
            mLayoutEllipsis.setVisibility(GONE);
            ImageLoader.load(getContext(), mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl, R.drawable.livekit_ic_avatar);
            ImageLoader.load(getContext(), mImageSecondApplyLinkAudience,
                    applyLinkAudienceList.get(1).avatarUrl, R.drawable.livekit_ic_avatar);
        } else if (seatApplicationList.size() > 2) {
            mLayoutSecondApplyLinkAudience.setVisibility(VISIBLE);
            mLayoutEllipsis.setVisibility(VISIBLE);
            ImageLoader.load(getContext(), mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl, R.drawable.livekit_ic_avatar);
            ImageLoader.load(getContext(), mImageSecondApplyLinkAudience,
                    applyLinkAudienceList.get(1).avatarUrl, R.drawable.livekit_ic_avatar);
        } else {
            setVisibility(GONE);
        }
        mTextApplyLinkAudienceCount.setText(getContext().getString(R.string.common_seat_application_title,
                seatApplicationList.size()));
    }

    private void onApplyLinkAudienceListChange(Set<TUIRoomDefine.UserInfo> applicantList) {
        initApplyLinkAudienceListView();
    }
}
