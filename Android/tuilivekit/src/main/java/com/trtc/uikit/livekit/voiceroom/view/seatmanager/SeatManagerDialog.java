package com.trtc.uikit.livekit.voiceroom.view.seatmanager;

import static android.view.View.VISIBLE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.SwitchCompat;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;

import java.util.LinkedHashSet;
import java.util.List;

public class SeatManagerDialog extends PopupDialog {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("SeatManagerDialog");

    private final Context          mContext;
    private final VoiceRoomManager mVoiceRoomManager;
    private final SeatGridView     mSeatGridView;

    private ImageView              mImageBack;
    private TextView               mTvTitle;
    private TextView               mSeatListTitle;
    private TextView               mSeatApplicationTitle;
    private Button                 mInviteButton;
    private ImageView              mEndButton;
    private View                   mEndButtonContainer;
    private RecyclerView           mSeatListView;
    private RecyclerView           mSeatApplicationListView;
    private View                   mEmptyView;
    private SeatApplicationAdapter mSeatApplicationAdapter;
    private SeatListPanelAdapter   mSeatListPanelAdapter;
    private SeatInvitationDialog   mSeatInvitationDialog;
    private SwitchCompat           mSwitchNeedRequest;


    private final Observer<List<SeatState.SeatInfo>> mSeatListObserver = this::onSeatListChanged;
    private final Observer<TUIRoomDefine.SeatMode>   mSeatModeObserver = this::onSeatModeChanged;

    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mSeatApplicationListObserver =
            this::onSeatApplicationListChanged;

    public SeatManagerDialog(@NonNull Context context, VoiceRoomManager voiceRoomManager, SeatGridView seatGridView) {
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
        View rootView = View.inflate(mContext, R.layout.livekit_voiceroom_seat_manager_panel, null);
        setView(rootView);
        bindViewId(rootView);
        mTvTitle.setText(R.string.common_link_mic_manager);
        mInviteButton.setOnClickListener(v -> showSeatInvitationPanel());
        showBackButton();
        showEndButton();
        initSeatListView();
        initSeatApplicationListView();
        initNeedRequest();
    }

    private void showEndButton() {
        mEndButton.setImageResource(R.drawable.livekit_ic_invite_user);
        mEndButtonContainer.setVisibility(VISIBLE);
        mEndButtonContainer.setOnClickListener(v -> showSeatInvitationPanel());
    }

    private void showBackButton() {
        mImageBack.setOnClickListener(view -> dismiss());
        mImageBack.setVisibility(VISIBLE);
    }

    private void addObserver() {
        mVoiceRoomManager.getSeatState().seatList.observeForever(mSeatListObserver);
        mVoiceRoomManager.getSeatState().seatApplicationList.observeForever(mSeatApplicationListObserver);
        mVoiceRoomManager.getRoomState().seatMode.observeForever(mSeatModeObserver);
    }

    private void removeObserver() {
        mVoiceRoomManager.getSeatState().seatList.removeObserver(mSeatListObserver);
        mVoiceRoomManager.getSeatState().seatApplicationList.removeObserver(mSeatApplicationListObserver);
        mVoiceRoomManager.getRoomState().seatMode.removeObserver(mSeatModeObserver);
    }

    private void bindViewId(View rootView) {
        mTvTitle = rootView.findViewById(R.id.tv_title);
        mImageBack = rootView.findViewById(R.id.iv_back);
        mSeatListTitle = rootView.findViewById(R.id.seat_list_title);
        mSeatApplicationTitle = rootView.findViewById(R.id.seat_application_title);
        mSeatListView = rootView.findViewById(R.id.rv_seat_list);
        mSeatApplicationListView = rootView.findViewById(R.id.rv_apply_link_user_list);
        mEmptyView = rootView.findViewById(R.id.empty_view_container);
        mSeatListView = rootView.findViewById(R.id.rv_seat_list);
        mInviteButton = rootView.findViewById(R.id.invite_button);
        mEndButton = rootView.findViewById(R.id.end_button);
        mEndButton = rootView.findViewById(R.id.end_button);
        mEndButtonContainer = rootView.findViewById(R.id.end_button_container);
        mSeatApplicationListView = rootView.findViewById(R.id.rv_seat_application);
        mSwitchNeedRequest = rootView.findViewById(R.id.need_request);
    }

    private void initSeatListView() {
        mSeatListView.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL, false));
        mSeatListPanelAdapter = new SeatListPanelAdapter(mContext, mVoiceRoomManager, mSeatGridView);
        mSeatListView.setAdapter(mSeatListPanelAdapter);
    }

    private void initSeatApplicationListView() {
        mSeatApplicationListView.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mSeatApplicationAdapter = new SeatApplicationAdapter(mContext, mVoiceRoomManager, mSeatGridView);
        mSeatApplicationListView.setAdapter(mSeatApplicationAdapter);
    }

    private void initSeatApplicationTitleView() {
        if (!mVoiceRoomManager.getSeatState().seatApplicationList.getValue().isEmpty()) {
            mSeatApplicationTitle.setVisibility(VISIBLE);
        } else {
            mSeatApplicationTitle.setVisibility(View.GONE);
        }
        mSeatApplicationTitle.setText(mContext.getString(R.string.common_seat_application_title,
                mVoiceRoomManager.getSeatState().seatApplicationList.getValue().size()));
    }


    private void initNeedRequest() {
        boolean needRequest =
                mVoiceRoomManager.getRoomState().seatMode.getValue() == TUIRoomDefine.SeatMode.APPLY_TO_TAKE;
        mSwitchNeedRequest.setChecked(needRequest);
        mSwitchNeedRequest.setOnCheckedChangeListener((compoundButton, enable) -> onSeatModeClicked(enable));
    }

    @SuppressLint("StringFormatMatches")
    private void initSeatListViewTitle() {
        List<SeatState.SeatInfo> seatList = mSeatListPanelAdapter.getData();
        if (seatList.isEmpty()) {
            mSeatListTitle.setVisibility(View.GONE);
        } else {
            mSeatListTitle.setVisibility(VISIBLE);
            mSeatListTitle.setText(mContext.getString(R.string.common_seat_list_title, seatList.size()
                    , mVoiceRoomManager.getRoomState().maxSeatCount.getValue() - 1));
        }
    }

    private void onSeatListChanged(List<SeatState.SeatInfo> seatInfoList) {
        mSeatListPanelAdapter.updateData();
        initSeatListViewTitle();
        updateEmptyView();
    }

    private void onSeatApplicationListChanged(LinkedHashSet<SeatState.SeatApplication> seatApplications) {
        mSeatApplicationAdapter.updateData();
        initSeatApplicationTitleView();
        updateEmptyView();
    }

    private void onSeatModeChanged(TUIRoomDefine.SeatMode seatMode) {
        mSwitchNeedRequest.setChecked(seatMode == TUIRoomDefine.SeatMode.APPLY_TO_TAKE);
    }

    private void showSeatInvitationPanel() {
        if (mSeatInvitationDialog == null) {
            mSeatInvitationDialog = new SeatInvitationDialog(mContext, mVoiceRoomManager, mSeatGridView);
        }
        mSeatInvitationDialog.show();
    }

    private void updateEmptyView() {
        List<SeatState.SeatInfo> seatList = mSeatListPanelAdapter.getData();
        LinkedHashSet<SeatState.SeatApplication> seatApplicationList =
                mVoiceRoomManager.getSeatState().seatApplicationList.getValue();
        if (seatList.isEmpty() && seatApplicationList.isEmpty()) {
            mEmptyView.setVisibility(VISIBLE);
        } else {
            mEmptyView.setVisibility(View.GONE);
        }
    }

    private void onSeatModeClicked(boolean enable) {
        TUIRoomDefine.SeatMode seatMode = enable ? TUIRoomDefine.SeatMode.APPLY_TO_TAKE :
                TUIRoomDefine.SeatMode.FREE_TO_TAKE;
        mSeatGridView.updateRoomSeatMode(seatMode, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVoiceRoomManager.getRoomManager().updateSeatMode(seatMode);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("responseSeatInvitation failed, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }
}
