package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.link;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;

import java.util.LinkedHashSet;
import java.util.List;

@SuppressLint("ViewConstructor")
public class AnchorLinkMicManagePanel extends BasicView {

    private       TextView                                           mTextMicUpTitle;
    private       TextView                                           mTextMicDownTitle;
    private       View                                               mViewSeparation;
    private       RecyclerView                                       mRecyclerLinkAudienceView;
    private       RecyclerView                                       mRecyclerApplyLinkAudienceView;
    private       AnchorLinkMicAdapter                               mAnchorLinkMicAdapter;
    private       AnchorApplyLinkMicAdapter                          mAnchorApplyLinkMicAdapter;
    private final PopupDialog.DialogActionListener                   mDismissListener;
    private final Observer<List<SeatState.SeatInfo>>                 mLinkAudienceListObserver      =
            this::onLinkAudienceListChange;
    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mApplyLinkAudienceListObserver =
            this::onApplyLinkAudienceListChange;

    public AnchorLinkMicManagePanel(Context context, LiveController liveController,
                                    PopupDialog.DialogActionListener listener) {
        super(context, liveController);
        mDismissListener = listener;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_link_manage_panel, this, true);
        bindViewId();

        initBackView();
        initMicUpTitleView();
        initMicDownTitleView();
        initViewSeparation();
        initLinkAudienceListView();
        initApplyLinkAudienceListView();
    }

    private void initViewSeparation() {
        if (!mSeatState.seatApplicationList.get().isEmpty() && mSeatState.seatList.get().size() > 1) {
            mViewSeparation.setVisibility(VISIBLE);
        } else {
            mViewSeparation.setVisibility(GONE);
        }
    }

    @Override
    protected void addObserver() {
        mSeatState.seatList.observe(mLinkAudienceListObserver);
        mSeatState.seatApplicationList.observe(mApplyLinkAudienceListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatList.removeObserver(mLinkAudienceListObserver);
        mSeatState.seatApplicationList.removeObserver(mApplyLinkAudienceListObserver);
    }

    private void bindViewId() {
        mTextMicUpTitle = findViewById(R.id.tv_mic_up_title);
        mTextMicDownTitle = findViewById(R.id.tv_mic_down_title);
        mViewSeparation = findViewById(R.id.view_separation);
        mRecyclerLinkAudienceView = findViewById(R.id.rv_link_user_list);
        mRecyclerApplyLinkAudienceView = findViewById(R.id.rv_apply_link_user_list);
    }

    private void initBackView() {
        findViewById(R.id.iv_back).setOnClickListener(view -> {
            if (mDismissListener != null) {
                mDismissListener.dismiss();
            }
        });
    }

    private void initApplyLinkAudienceListView() {
        mRecyclerApplyLinkAudienceView.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));

        mAnchorApplyLinkMicAdapter = new AnchorApplyLinkMicAdapter(mContext, mLiveController);
        mRecyclerApplyLinkAudienceView.setAdapter(mAnchorApplyLinkMicAdapter);
    }

    private void initLinkAudienceListView() {
        mRecyclerLinkAudienceView.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));

        mAnchorLinkMicAdapter = new AnchorLinkMicAdapter(mContext, mLiveController);
        mRecyclerLinkAudienceView.setAdapter(mAnchorLinkMicAdapter);
    }

    private void initMicDownTitleView() {
        if (!mLiveController.getSeatState().seatApplicationList.get().isEmpty()) {
            mTextMicDownTitle.setVisibility(VISIBLE);
        } else {
            mTextMicDownTitle.setVisibility(GONE);
        }
        mTextMicDownTitle.setText(mContext.getString(R.string.livekit_seat_application_title,
                mLiveController.getSeatState().seatApplicationList.get().size()));
    }

    @SuppressLint("StringFormatMatches")
    private void initMicUpTitleView() {
        if (mLiveController.getSeatState().seatList.get().size() > 1) {
            mTextMicUpTitle.setVisibility(VISIBLE);
            mTextMicUpTitle.setText(mContext.getString(R.string.livekit_seat_list_title,
                    mLiveController.getSeatState().seatList.get().size() - 1,
                    mLiveController.getRoomSate().maxSeatCount.get()));
        } else {
            mTextMicUpTitle.setVisibility(GONE);
        }
    }

    private void onLinkAudienceListChange(List<SeatState.SeatInfo> seatInfoList) {
        initMicUpTitleView();
        initViewSeparation();
        mAnchorLinkMicAdapter.updateData();
    }

    private void onApplyLinkAudienceListChange(LinkedHashSet<SeatState.SeatApplication> seatApplications) {
        initMicDownTitleView();
        initViewSeparation();
        mAnchorApplyLinkMicAdapter.updateData();
    }
}