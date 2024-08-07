package com.trtc.uikit.livekit.view.voiceroom.view.panel.seatmanager;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanel;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.voiceroom.view.panel.invite.SeatInvitationView;

import java.util.LinkedHashSet;
import java.util.List;

@SuppressLint("ViewConstructor")
public class SeatManagerView extends BottomPanelView {

    private TextView               mSeatListTitle;
    private TextView               mSeatApplicationTitle;
    private RecyclerView           mSeatListView;
    private RecyclerView           mSeatApplicationListView;
    private View                   mEmptyView;
    private SeatApplicationAdapter mSeatApplicationAdapter;
    private SeatListPanelAdapter   mSeatListPanelAdapter;
    private BottomPanel            mSeatInvitationPanel;

    private final Observer<List<SeatState.SeatInfo>> mSeatListObserver = this::onSeatListChange;

    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mSeatApplicationListObserver =
            this::onSeatApplicationListChange;

    public SeatManagerView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_seat_manager_panel, this, true);
        setTitle(mContext.getString(R.string.livekit_link_mic_manager));
        showEndButton(R.drawable.livekit_ic_invite_user, v -> showSeatInvitationPanel());
        findViewById(R.id.invite_button).setOnClickListener(v -> showSeatInvitationPanel());
        bindViewId();
        initSeatListView();
        initSeatApplicationListView();
    }

    @Override
    protected void addObserver() {
        mSeatState.seatList.observe(mSeatListObserver);
        mSeatState.seatApplicationList.observe(mSeatApplicationListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatList.removeObserver(mSeatListObserver);
        mSeatState.seatApplicationList.removeObserver(mSeatApplicationListObserver);
    }

    private void bindViewId() {
        mSeatListTitle = findViewById(R.id.seat_list_title);
        mSeatApplicationTitle = findViewById(R.id.seat_application_title);
        mSeatListView = findViewById(R.id.rv_seat_list);
        mSeatApplicationListView = findViewById(R.id.rv_apply_link_user_list);
        mEmptyView = findViewById(R.id.empty_view_container);
    }

    private void initSeatListView() {
        mSeatListView = findViewById(R.id.rv_seat_list);
        mSeatListView.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL, false));
        mSeatListPanelAdapter = new SeatListPanelAdapter(mContext, mLiveController);
        mSeatListView.setAdapter(mSeatListPanelAdapter);
    }

    private void initSeatApplicationListView() {
        mSeatApplicationListView = findViewById(R.id.rv_seat_application);
        mSeatApplicationListView.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mSeatApplicationAdapter = new SeatApplicationAdapter(mContext, mLiveController);
        mSeatApplicationListView.setAdapter(mSeatApplicationAdapter);
    }

    private void initSeatApplicationTitleView() {
        if (!mLiveController.getSeatState().seatApplicationList.get().isEmpty()) {
            mSeatApplicationTitle.setVisibility(VISIBLE);
        } else {
            mSeatApplicationTitle.setVisibility(GONE);
        }
        mSeatApplicationTitle.setText(mContext.getString(R.string.livekit_seat_application_title,
                mLiveController.getSeatState().seatApplicationList.get().size()));
    }

    @SuppressLint("StringFormatMatches")
    private void initSeatListViewTitle() {
        List<SeatState.SeatInfo> seatList = mSeatListPanelAdapter.getData();
        if (seatList.isEmpty()) {
            mSeatListTitle.setVisibility(GONE);
        } else {
            mSeatListTitle.setVisibility(VISIBLE);
            mSeatListTitle.setText(mContext.getString(R.string.livekit_seat_list_title, seatList.size()
                    , mLiveController.getRoomSate().maxSeatCount.get() - 1));
        }
    }

    private void onSeatListChange(List<SeatState.SeatInfo> seatInfoList) {
        mSeatListPanelAdapter.updateData();
        initSeatListViewTitle();
        updateEmptyView();
    }

    private void onSeatApplicationListChange(LinkedHashSet<SeatState.SeatApplication> seatApplications) {
        mSeatApplicationAdapter.updateData();
        initSeatApplicationTitleView();
        updateEmptyView();
    }

    private void showSeatInvitationPanel() {
        if (mSeatInvitationPanel == null) {
            SeatInvitationView panelView = new SeatInvitationView(mContext, mLiveController);
            mSeatInvitationPanel = BottomPanel.create(panelView);
        }
        mSeatInvitationPanel.show();
    }

    private void updateEmptyView() {
        List<SeatState.SeatInfo> seatList = mSeatListPanelAdapter.getData();
        LinkedHashSet<SeatState.SeatApplication> seatApplicationList = mSeatState.seatApplicationList.get();
        if (seatList.isEmpty() && seatApplicationList.isEmpty()) {
            mEmptyView.setVisibility(VISIBLE);
        } else {
            mEmptyView.setVisibility(GONE);
        }
    }
}

