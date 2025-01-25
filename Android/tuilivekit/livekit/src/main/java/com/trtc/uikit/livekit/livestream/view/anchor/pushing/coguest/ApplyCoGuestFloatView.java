package com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.LinkedHashSet;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class ApplyCoGuestFloatView extends BasicView {

    private       LinearLayout                                          mLayoutRoot;
    private       ImageView                                             mImageFirstApplyLinkAudience;
    private       ImageView                                             mImageSecondApplyLinkAudience;
    private       RelativeLayout                                        mLayoutSecondApplyLinkAudience;
    private       RelativeLayout                                        mLayoutEllipsis;
    private       TextView                                              mTextApplyLinkAudienceCount;
    private       LiveCoreView                                          mLiveStream;
    private final Observer<LinkedHashSet<CoGuestState.SeatApplication>> mApplyLinkAudienceListObserver =
            this::onApplyLinkAudienceListChange;

    public ApplyCoGuestFloatView(@NonNull Context context) {
        this(context, null);
    }

    public ApplyCoGuestFloatView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public ApplyCoGuestFloatView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(LiveStreamManager manager, LiveCoreView liveStream) {
        super.init(manager);
        mLiveStream = liveStream;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_apply_link_audience, this, true);
        mLayoutRoot = findViewById(R.id.ll_root);
        mTextApplyLinkAudienceCount = findViewById(R.id.tv_apply_link_audience_count);
        mLayoutSecondApplyLinkAudience = findViewById(R.id.rl_second_apply_link_audience);
        mLayoutEllipsis = findViewById(R.id.rl_ellipsis);
        mImageFirstApplyLinkAudience = findViewById(R.id.iv_first_apply_link_audience);
        mImageSecondApplyLinkAudience = findViewById(R.id.iv_second_apply_link_audience);
    }

    @Override
    protected void refreshView() {
        initApplyLinkAudienceListView();
        initRootView();
    }

    private void initRootView() {
        mLayoutRoot.setOnClickListener((view) -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            AnchorCoGuestManageDialog dialog = new AnchorCoGuestManageDialog(mContext, mLiveManager, mLiveStream);
            dialog.setOnDismissListener(dialog1 -> view.setEnabled(true));
            dialog.show();
        });
    }

    @Override
    protected void addObserver() {
        mCoGuestState.requestCoGuestList.observe(mApplyLinkAudienceListObserver);
    }

    @Override
    protected void removeObserver() {
        mCoGuestState.requestCoGuestList.removeObserver(mApplyLinkAudienceListObserver);
    }

    private void initApplyLinkAudienceListView() {
        LinkedHashSet<CoGuestState.SeatApplication> seatApplicationList = mCoGuestState.requestCoGuestList.get();
        if (!seatApplicationList.isEmpty()) {
            setVisibility(VISIBLE);
        } else {
            setVisibility(GONE);
        }

        final CopyOnWriteArrayList<CoGuestState.SeatApplication> applyLinkAudienceList =
                new CopyOnWriteArrayList<>(seatApplicationList);
        if (seatApplicationList.size() == 1) {
            mLayoutSecondApplyLinkAudience.setVisibility(GONE);
            mLayoutEllipsis.setVisibility(GONE);
            ImageLoader.load(mContext, mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl, R.drawable.livekit_ic_avatar);
        } else if (seatApplicationList.size() == 2) {
            mLayoutSecondApplyLinkAudience.setVisibility(VISIBLE);
            mLayoutEllipsis.setVisibility(GONE);
            ImageLoader.load(mContext, mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl, R.drawable.livekit_ic_avatar);
            ImageLoader.load(mContext, mImageSecondApplyLinkAudience,
                    applyLinkAudienceList.get(1).avatarUrl, R.drawable.livekit_ic_avatar);
        } else if (seatApplicationList.size() > 2) {
            mLayoutSecondApplyLinkAudience.setVisibility(VISIBLE);
            mLayoutEllipsis.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl, R.drawable.livekit_ic_avatar);
            ImageLoader.load(mContext, mImageSecondApplyLinkAudience,
                    applyLinkAudienceList.get(1).avatarUrl, R.drawable.livekit_ic_avatar);
        } else {
            setVisibility(GONE);
        }
        mTextApplyLinkAudienceCount.setText(mContext.getString(R.string.livekit_link_mic_down_title_popup,
                seatApplicationList.size()));
    }

    private void onApplyLinkAudienceListChange(LinkedHashSet<CoGuestState.SeatApplication> seatApplications) {
        initApplyLinkAudienceListView();
    }
}
