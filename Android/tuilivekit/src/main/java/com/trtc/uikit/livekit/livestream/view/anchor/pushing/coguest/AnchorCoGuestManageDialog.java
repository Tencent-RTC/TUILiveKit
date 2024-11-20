package com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.LinkedHashSet;
import java.util.List;

@SuppressLint("ViewConstructor")
public class AnchorCoGuestManageDialog extends PopupDialog {

    private       TextView                                              mTextMicUpTitle;
    private       TextView                                              mTextMicDownTitle;
    private       ImageView                                             mImageBack;
    private       View                                                  mViewSeparation;
    private       RecyclerView                                          mRecyclerLinkAudienceView;
    private       RecyclerView                                          mRecyclerApplyLinkAudienceView;
    private       AnchorCoGuestAdapter                                  mAnchorLinkMicAdapter;
    private       AnchorApplyCoGuestAdapter                             mAnchorApplyLinkMicAdapter;
    private final LiveCoreView                                          mLiveStream;
    private final LiveStreamManager                                     mLiveManager;
    private final Observer<List<CoGuestState.SeatInfo>>                 mLinkAudienceListObserver      =
            this::onLinkAudienceListChange;
    private final Observer<LinkedHashSet<CoGuestState.SeatApplication>> mApplyLinkAudienceListObserver =
            this::onApplyLinkAudienceListChange;

    public AnchorCoGuestManageDialog(Context context, LiveStreamManager manager,
                                     LiveCoreView liveStream) {
        super(context);
        mLiveManager = manager;
        mLiveStream = liveStream;
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_link_manage_panel, null);
        bindViewId(view);

        initBackView();
        initMicUpTitleView();
        initMicDownTitleView();
        initViewSeparation();
        initLinkAudienceListView();
        initApplyLinkAudienceListView();

        setView(view);
    }

    private void initViewSeparation() {
        if (!mLiveManager.getCoGuestState().requestCoGuestList.get().isEmpty()
                && mLiveManager.getCoGuestState().connectedUserList.get().size() > 1) {
            mViewSeparation.setVisibility(VISIBLE);
        } else {
            mViewSeparation.setVisibility(GONE);
        }
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();

        addObserver();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    protected void addObserver() {
        mLiveManager.getCoGuestState().connectedUserList.observe(mLinkAudienceListObserver);
        mLiveManager.getCoGuestState().requestCoGuestList.observe(mApplyLinkAudienceListObserver);
    }

    protected void removeObserver() {
        mLiveManager.getCoGuestState().connectedUserList.removeObserver(mLinkAudienceListObserver);
        mLiveManager.getCoGuestState().requestCoGuestList.removeObserver(mApplyLinkAudienceListObserver);
    }

    private void bindViewId(View view) {
        mTextMicUpTitle = view.findViewById(R.id.tv_mic_up_title);
        mTextMicDownTitle = view.findViewById(R.id.tv_mic_down_title);
        mViewSeparation = view.findViewById(R.id.view_separation);
        mRecyclerLinkAudienceView = view.findViewById(R.id.rv_link_user_list);
        mRecyclerApplyLinkAudienceView = view.findViewById(R.id.rv_apply_link_user_list);
        mImageBack = view.findViewById(R.id.iv_back);
    }

    private void initBackView() {
        mImageBack.setOnClickListener(view -> {
            dismiss();
        });
    }

    private void initApplyLinkAudienceListView() {
        mRecyclerApplyLinkAudienceView.setLayoutManager(new LinearLayoutManager(getContext(),
                LinearLayoutManager.VERTICAL, false));
        mAnchorApplyLinkMicAdapter = new AnchorApplyCoGuestAdapter(getContext(), mLiveManager, mLiveStream);
        mRecyclerApplyLinkAudienceView.setAdapter(mAnchorApplyLinkMicAdapter);
    }

    private void initLinkAudienceListView() {
        mRecyclerLinkAudienceView.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL,
                false));

        mAnchorLinkMicAdapter = new AnchorCoGuestAdapter(getContext(), mLiveManager, mLiveStream);
        mRecyclerLinkAudienceView.setAdapter(mAnchorLinkMicAdapter);
    }

    private void initMicDownTitleView() {
        if (!mLiveManager.getCoGuestState().requestCoGuestList.get().isEmpty()) {
            mTextMicDownTitle.setVisibility(VISIBLE);
        } else {
            mTextMicDownTitle.setVisibility(GONE);
        }
        mTextMicDownTitle.setText(getContext().getString(R.string.livekit_seat_application_title,
                mLiveManager.getCoGuestState().requestCoGuestList.get().size()));
    }

    @SuppressLint("StringFormatMatches")
    private void initMicUpTitleView() {
        if (mLiveManager.getCoGuestState().connectedUserList.get().size() > 1) {
            mTextMicUpTitle.setVisibility(VISIBLE);
            mTextMicUpTitle.setText(getContext().getString(R.string.livekit_seat_list_title,
                    mLiveManager.getCoGuestState().connectedUserList.get().size() - 1,
                    mLiveManager.getRoomState().maxSeatCount.get()));
        } else {
            mTextMicUpTitle.setVisibility(GONE);
        }
    }

    private void onLinkAudienceListChange(List<CoGuestState.SeatInfo> seatInfoList) {
        initMicUpTitleView();
        initViewSeparation();
        mAnchorLinkMicAdapter.updateData();
    }

    private void onApplyLinkAudienceListChange(LinkedHashSet<CoGuestState.SeatApplication> seatApplications) {
        initMicDownTitleView();
        initViewSeparation();
        mAnchorApplyLinkMicAdapter.updateData();
    }
}