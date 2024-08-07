package com.trtc.uikit.livekit.view.voiceroom.view.panel.invite;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.LiveMapObserver;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class SeatInvitationView extends BottomPanelView {
    private SeatInvitationAdapter mSeatInvitationAdapter;
    private int                   mInvitationIndex = -1;

    private final Observer<LinkedHashSet<UserState.UserInfo>> mAudienceListObserver = this::onAudienceListChange;
    private final Observer<List<SeatState.SeatInfo>>          mSeatListObserver     = this::updateSeatListView;

    public SeatInvitationView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_seat_invite_panel, this, true);
        setTitle(mContext.getString(R.string.livekit_voiceroom_invite));
        showBackButton();
        initSeatListView();
    }

    @Override
    protected void addObserver() {
        mUserState.userList.observe(mAudienceListObserver);
        mSeatState.sentSeatInvitationMap.observe(mSentSeatInvitationMap);
        mSeatState.seatList.observe(mSeatListObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.userList.removeObserver(mAudienceListObserver);
        mSeatState.sentSeatInvitationMap.removeObserver(mSentSeatInvitationMap);
        mSeatState.seatList.removeObserver(mSeatListObserver);
    }

    public void setInviteSeatIndex(int seatIndex) {
        mInvitationIndex = seatIndex;
    }

    private void initSeatListView() {
        RecyclerView seatInvitationListView = findViewById(R.id.rv_seat_invitation);
        seatInvitationListView.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mSeatInvitationAdapter = new SeatInvitationAdapter(mContext, mLiveController);
        mSeatInvitationAdapter.setOnInviteButtonClickListener(this::onInviteButtonClicked);
        seatInvitationListView.setAdapter(mSeatInvitationAdapter);
    }

    private void onAudienceListChange(LinkedHashSet<UserState.UserInfo> userInfoList) {
        mSeatInvitationAdapter.updateData();
    }

    private void updateSeatListView(List<SeatState.SeatInfo> seatList) {
        mSeatInvitationAdapter.updateData();
    }

    private void onInviteButtonClicked(TextView inviteButton, UserState.UserInfo userInfo) {
        if (inviteButton.isSelected()) {
            mSeatController.cancelSeatInvitation(userInfo.userId);
            return;
        }
        mSeatController.takeUserOnSeatByAdmin(mInvitationIndex, userInfo);
        if (mInvitationIndex != -1) {
            dismiss();
        }
    }

    private final LiveMapObserver<String, SeatState.SeatInvitation> mSentSeatInvitationMap =
            new LiveMapObserver<String, SeatState.SeatInvitation>() {
                @SuppressLint("NotifyDataSetChanged")
                @Override
                public void onDataChanged(Map<String, SeatState.SeatInvitation> map) {
                    mSeatInvitationAdapter.notifyDataSetChanged();
                }

                @Override
                public void onItemChanged(String key, SeatState.SeatInvitation seatInvitation) {
                    mSeatInvitationAdapter.updateSentSeatInvitationState(seatInvitation.userId);
                }

                @Override
                public void onItemRemoved(String key, SeatState.SeatInvitation seatInvitation) {
                    mSeatInvitationAdapter.updateSentSeatInvitationState(seatInvitation.userId);
                }
            };
}
