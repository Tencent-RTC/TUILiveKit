package com.trtc.uikit.livekit.voiceroom.view.seatmanager;

import static android.view.View.VISIBLE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.livedata.LiveMapObserver;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.error.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

public class SeatInvitationDialog extends PopupDialog {
    private static final String FILE = "SeatInvitationDialog";

    private final Context          mContext;
    private final VoiceRoomManager mVoiceRoomManager;
    private final SeatGridView     mSeatGridView;

    private ImageView    mImageBack;
    private TextView     mTvTitle;
    private RecyclerView mSeatInvitationListView;

    private SeatInvitationAdapter mSeatInvitationAdapter;
    private int                   mInvitationIndex = -1;

    private final Observer<LinkedHashSet<UserState.UserInfo>> mAudienceListObserver = this::onAudienceListChanged;
    private final Observer<List<SeatState.SeatInfo>>          mSeatListObserver     = this::updateSeatListView;

    public SeatInvitationDialog(@NonNull Context context, VoiceRoomManager voiceRoomManager,
                                SeatGridView seatGridView) {
        super(context);
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        mSeatGridView = seatGridView;
        initView();
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

    private void initView() {
        View rootView = View.inflate(mContext, R.layout.livekit_voiceroom_seat_invite_panel, null);
        setView(rootView);
        bindViewId(rootView);
        mTvTitle.setText(R.string.livekit_voiceroom_invite);
        setTitle(mContext.getString(R.string.livekit_voiceroom_invite));
        showBackButton();
        initSeatListView();
    }

    private void addObserver() {
        mVoiceRoomManager.getUserState().userList.observe(mAudienceListObserver);
        mVoiceRoomManager.getSeatState().sentSeatInvitationMap.observe(mSentSeatInvitationMap);
        mVoiceRoomManager.getSeatState().seatList.observe(mSeatListObserver);
    }

    private void removeObserver() {
        mVoiceRoomManager.getUserState().userList.removeObserver(mAudienceListObserver);
        mVoiceRoomManager.getSeatState().sentSeatInvitationMap.removeObserver(mSentSeatInvitationMap);
        mVoiceRoomManager.getSeatState().seatList.removeObserver(mSeatListObserver);
    }

    private void bindViewId(View rootView) {
        mImageBack = rootView.findViewById(R.id.iv_back);
        mTvTitle = rootView.findViewById(R.id.tv_title);
        mSeatInvitationListView = rootView.findViewById(R.id.rv_seat_invitation);
    }

    private void showBackButton() {
        mImageBack.setOnClickListener(view -> dismiss());
        mImageBack.setVisibility(VISIBLE);
    }

    public void setInviteSeatIndex(int seatIndex) {
        mInvitationIndex = seatIndex;
    }

    private void initSeatListView() {
        mSeatInvitationListView.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mSeatInvitationAdapter = new SeatInvitationAdapter(mContext, mVoiceRoomManager);
        mSeatInvitationAdapter.setOnInviteButtonClickListener(this::onInviteButtonClicked);
        mSeatInvitationListView.setAdapter(mSeatInvitationAdapter);
    }

    private void onAudienceListChanged(LinkedHashSet<UserState.UserInfo> userInfoList) {
        mSeatInvitationAdapter.updateData();
    }

    private void updateSeatListView(List<SeatState.SeatInfo> seatList) {
        mSeatInvitationAdapter.updateData();
    }

    private void onInviteButtonClicked(TextView inviteButton, UserState.UserInfo userInfo) {
        String userId = userInfo.userId;
        if (inviteButton.isSelected()) {
            mSeatGridView.cancelRequest(userId, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mVoiceRoomManager.getSeatManager().removeSentSeatInvitation(userId);
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    Logger.error(FILE, "cancelRequest failed,error:" + error + ",message:" + message);
                    ErrorLocalized.onError(error);
                }
            });
            return;
        }
        mSeatGridView.takeUserOnSeatByAdmin(mInvitationIndex, userId, 60, new VoiceRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(TUIRoomDefine.UserInfo userInfo) {
                mVoiceRoomManager.getSeatManager().removeSentSeatInvitation(userId);
            }

            @Override
            public void onRejected(TUIRoomDefine.UserInfo userInfo) {
                mVoiceRoomManager.getSeatManager().removeSentSeatInvitation(userId);
                ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(
                        R.string.livekit_voiceroom_invite_seat_canceled));

            }

            @Override
            public void onCancelled(TUIRoomDefine.UserInfo userInfo) {
                mVoiceRoomManager.getSeatManager().removeSentSeatInvitation(userId);
            }

            @Override
            public void onTimeout(TUIRoomDefine.UserInfo userInfo) {
                mVoiceRoomManager.getSeatManager().removeSentSeatInvitation(userId);
                ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(
                        R.string.livekit_voiceroom_invite_seat_canceled));
            }

            @Override
            public void onError(TUIRoomDefine.UserInfo userInfo, TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "takeUserOnSeatByAdmin failed,error:" + error + ",message:" + message);
                ErrorLocalized.onError(error);
                mVoiceRoomManager.getSeatManager().removeSentSeatInvitation(userId);
            }
        });
        mVoiceRoomManager.getSeatManager().addSentSeatInvitation(userInfo);
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
