package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.link.AnchorLinkMicManagePanel;

import java.util.LinkedHashSet;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class ApplyLinkMicFloatView extends BasicView {

    private       LinearLayout                                       mLayoutRoot;
    private       ImageView                                          mImageFirstApplyLinkAudience;
    private       ImageView                                          mImageSecondApplyLinkAudience;
    private       RelativeLayout                                     mLayoutSecondApplyLinkAudience;
    private       RelativeLayout                                     mLayoutEllipsis;
    private       TextView                                           mTextApplyLinkAudienceCount;
    private       AnchorLinkMicManagePanel                           mAnchorLinkMicPanel;
    private       PopupDialog                                        mAnchorLinkMicDialog;
    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mApplyLinkAudienceListObserver =
            this::onApplyLinkAudienceListChange;

    public ApplyLinkMicFloatView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_apply_link_audience, this, true);
        bindViewId();

        initApplyLinkAudienceListView();
        initRootView();
    }

    private void initRootView() {
        mLayoutRoot.setOnClickListener((view) -> {
            if (mAnchorLinkMicDialog == null) {
                mAnchorLinkMicDialog = new PopupDialog(mContext);
                mAnchorLinkMicDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mAnchorLinkMicPanel == null) {
                mAnchorLinkMicPanel = new AnchorLinkMicManagePanel(mContext, mLiveController,
                        () -> mAnchorLinkMicDialog.dismiss());
            }
            mAnchorLinkMicDialog.setView(mAnchorLinkMicPanel);
            mAnchorLinkMicDialog.show();
        });
    }

    private void bindViewId() {
        mLayoutRoot = findViewById(R.id.ll_root);
        mTextApplyLinkAudienceCount = findViewById(R.id.tv_apply_link_audience_count);
        mLayoutSecondApplyLinkAudience = findViewById(R.id.rl_second_apply_link_audience);
        mLayoutEllipsis = findViewById(R.id.rl_ellipsis);
        mImageFirstApplyLinkAudience = findViewById(R.id.iv_first_apply_link_audience);
        mImageSecondApplyLinkAudience = findViewById(R.id.iv_second_apply_link_audience);
    }

    @Override
    protected void addObserver() {
        mSeatState.seatApplicationList.observe(mApplyLinkAudienceListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatApplicationList.removeObserver(mApplyLinkAudienceListObserver);
    }

    private void initApplyLinkAudienceListView() {
        LinkedHashSet<SeatState.SeatApplication> seatApplicationList = mSeatState.seatApplicationList.get();
        if (!seatApplicationList.isEmpty()) {
            setVisibility(VISIBLE);
        } else {
            setVisibility(GONE);
        }

        final CopyOnWriteArrayList<SeatState.SeatApplication> applyLinkAudienceList =
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

    private void onApplyLinkAudienceListChange(LinkedHashSet<SeatState.SeatApplication> seatApplications) {
        initApplyLinkAudienceListView();
    }
}
