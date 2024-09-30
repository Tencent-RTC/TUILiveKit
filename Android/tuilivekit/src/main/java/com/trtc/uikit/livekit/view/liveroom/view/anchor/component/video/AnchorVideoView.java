package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.video;

import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_TOAST;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.gridlayout.widget.GridLayout;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LinkMicGridHelper;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.StandardDialog;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.BattleState;
import com.trtc.uikit.livekit.state.operation.BattleState.BattleUser;
import com.trtc.uikit.livekit.state.operation.ConnectionState;
import com.trtc.uikit.livekit.state.operation.ConnectionState.ConnectionUser;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.battle.BattleToastUtil;
import com.trtc.uikit.livekit.view.liveroom.view.common.battle.BattleInfoView;
import com.trtc.uikit.livekit.view.liveroom.view.common.battle.BattleMemberInfoView;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.RenderVideoViewModel;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoView;
import com.trtc.uikit.livekit.view.liveroom.view.common.video.VideoViewFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class AnchorVideoView extends BasicView {

    private static final String TAG = "AnchorVideoView";

    private       GridLayout                               mLayoutVideoList;
    private       LinkMicGridHelper                        mLinkMicGridHelper;
    private final CopyOnWriteArrayList<SeatState.SeatInfo> mLinkUserList;
    private final CopyOnWriteArrayList<ConnectionUser>     mConnectedUserList;
    private final Observer<List<SeatState.SeatInfo>>       mLinkAudienceListObserver
            = this::onLinkAudienceListChange;
    private final Observer<List<ConnectionUser>>           mConnectionListObserver
            = this::onConnectedListChange;
    private final Observer<ConnectionUser>                 mReceivedConnectRequestObserver
            = this::onReceivedRequestChange;
    private final Observer<List<BattleUser>>               mBattledListObserver
            = this::onBattleScoreChanged;
    private final Observer<Boolean>                        mBattleStartObserver
            = this::onBattleStartChange;
    private final Observer<BattleUser>                     mReceivedBattleRequestObserver
            = this::onReceivedBattleRequestChange;

    private final Runnable mBattleEndInfoRemoveTask = this::stopDisplayBattleResult;

    private StandardDialog mProcessConnectionDialog;
    private StandardDialog mProcessBattleDialog;
    private BattleInfoView mBattleInfoView;

    private final ITUINotification mToastNotification = (key, subKey, param) -> {
        BattleToastUtil.showToast(mContext, "" + param.get("Toast"));
    };

    public AnchorVideoView(Context context, LiveController liveController) {
        super(context, liveController);
        mLinkUserList = new CopyOnWriteArrayList<>(mSeatState.seatList.get());
        mConnectedUserList = new CopyOnWriteArrayList<>(mConnectionState.connectedUsers.get());
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_video_view, this, true);
        bindViewId();

        initVideoViewList();
        initBattleView();
    }

    @Override
    protected void addObserver() {
        mSeatState.seatList.observe(mLinkAudienceListObserver);
        mConnectionState.connectedUsers.observe(mConnectionListObserver);
        mConnectionState.receivedConnectionRequest.observe(mReceivedConnectRequestObserver);
        mBattleState.mBattledUsers.observe(mBattledListObserver);
        mBattleState.mIsBattleRunning.observe(mBattleStartObserver);
        mBattleState.mReceivedBattleRequest.observe(mReceivedBattleRequestObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_TOAST, mToastNotification);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatList.removeObserver(mLinkAudienceListObserver);
        mConnectionState.connectedUsers.removeObserver(mConnectionListObserver);
        mConnectionState.receivedConnectionRequest.removeObserver(mReceivedConnectRequestObserver);
        mBattleState.mBattledUsers.removeObserver(mBattledListObserver);
        mBattleState.mIsBattleRunning.removeObserver(mBattleStartObserver);
        mBattleState.mReceivedBattleRequest.removeObserver(mReceivedBattleRequestObserver);
        TUICore.unRegisterEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_TOAST, mToastNotification);
        removeCallbacks(mBattleEndInfoRemoveTask);
    }

    private void bindViewId() {
        mLayoutVideoList = findViewById(R.id.gl_video_layout);
    }

    private void initVideoViewList() {
        mLinkMicGridHelper = new LinkMicGridHelper(mLayoutVideoList);
        mLiveController.getMediaController().openLocalCamera();
        addAnchorView();
        if (mLinkUserList != null) {
            for (SeatState.SeatInfo seatInfo : mLinkUserList) {
                addAudienceView(seatInfo);
            }
        }
        if (mConnectedUserList != null) {
            for (ConnectionState.ConnectionUser connectionUser : mConnectedUserList) {
                addAudienceView(new RenderVideoViewModel(connectionUser));
            }
        }
    }

    private void initBattleView() {
        FrameLayout battleLayout = findViewById(R.id.fl_battle_layout);
        mBattleInfoView = new BattleInfoView(mContext, mLiveController);
        battleLayout.addView(mBattleInfoView);
    }

    private void addAnchorView() {
        RenderVideoViewModel videoUser = new RenderVideoViewModel();
        videoUser.userId = mUserState.selfInfo.userId;
        videoUser.avatarUrl = mUserState.selfInfo.avatarUrl.get();
        videoUser.name = mUserState.selfInfo.name.get();

        VideoView linkAudienceVideoView = mLiveController.getVideoViewFactory().createVideoView(videoUser,
                mLiveController, mContext);
        mLinkMicGridHelper.addAnchorView(linkAudienceVideoView);
    }

    private void addAudienceView(SeatState.SeatInfo seatInfo) {
        addAudienceView(new RenderVideoViewModel(seatInfo, mLiveController.getRoomState().roomId));
    }

    private void addAudienceView(RenderVideoViewModel viewModel) {
        if (mUserState.selfInfo.userId.equals(viewModel.userId)) {
            return;
        }
        VideoView linkAudienceVideoView =
                mLiveController.getVideoViewFactory().createVideoView(viewModel, mLiveController, mContext);
        mLinkMicGridHelper.addAudienceView(linkAudienceVideoView);
    }

    private void removeAudienceView(RenderVideoViewModel viewModel) {
        if (mUserState.selfInfo.userId.equals(viewModel.userId)) {
            return;
        }
        VideoView view = mLiveController.getVideoViewFactory().createVideoView(viewModel, mLiveController, mContext);
        mLinkMicGridHelper.removeAudienceView(view);
        mLiveController.getVideoViewFactory().removeVideoViewByUserId(viewModel.userId);
    }

    private void removeAudienceView(SeatState.SeatInfo seatInfo) {
        removeAudienceView(new RenderVideoViewModel(seatInfo, mLiveController.getRoomState().roomId));
    }

    private void onLinkAudienceListChange(List<SeatState.SeatInfo> seatInfoList) {
        for (SeatState.SeatInfo seatInfo : seatInfoList) {
            boolean isContainer = false;
            for (int i = 0; i < mLinkUserList.size(); i++) {
                if (mLinkUserList.get(i).userId.get().equals(seatInfo.userId.get())) {
                    isContainer = true;
                    mLinkUserList.get(i).updateState(seatInfo);
                    break;
                }
            }
            if (!isContainer) {
                mLinkUserList.add(seatInfo);
                addAudienceView(seatInfo);
            }
        }

        for (SeatState.SeatInfo userInfo : mLinkUserList) {
            if (!seatInfoList.contains(userInfo)) {
                mLinkUserList.remove(userInfo);
                removeAudienceView(userInfo);
            }
        }
    }

    private void onConnectedListChange(List<ConnectionState.ConnectionUser> connectionUsers) {
        for (ConnectionState.ConnectionUser connectionUser : connectionUsers) {
            boolean isContainer = false;
            for (int i = 0; i < mConnectedUserList.size(); i++) {
                if (mConnectedUserList.get(i).userId.equals(connectionUser.userId)) {
                    isContainer = true;
                    break;
                }
            }
            if (!isContainer) {
                mConnectedUserList.add(connectionUser);
                addAudienceView(new RenderVideoViewModel(connectionUser));
            }
        }
        for (ConnectionState.ConnectionUser userInfo : mConnectedUserList) {
            if (!connectionUsers.contains(userInfo)) {
                mConnectedUserList.remove(userInfo);
                if (!TextUtils.equals(mUserState.selfInfo.userId, userInfo.userId)) {
                    removeAudienceView(new RenderVideoViewModel(userInfo));
                }
            }
        }
        onBattleScoreChanged(mBattleState.mBattledUsers.get());
    }

    private void onReceivedRequestChange(ConnectionState.ConnectionUser receivedConnectionRequest) {
        if (receivedConnectionRequest == null) {
            mProcessConnectionDialog.dismiss();
            return;
        }
        String content = receivedConnectionRequest.userName
                + getContext().getString(R.string.livekit_connect_inviting_append);
        showConnectionRequestDialog(content, receivedConnectionRequest.avatarUrl, receivedConnectionRequest.roomId);
    }

    private void showConnectionRequestDialog(String content, String avatarUrl, String roomId) {
        mProcessConnectionDialog = new StandardDialog(getContext());
        mProcessConnectionDialog.setContent(content);
        mProcessConnectionDialog.setAvatar(avatarUrl);

        String rejectText = getContext().getString(R.string.livekit_reject);
        mProcessConnectionDialog.setNegativeText(rejectText, negativeView -> {
            mConnectionController.reject(roomId);
            mProcessConnectionDialog.dismiss();
        });

        String receiveText = getContext().getString(R.string.livekit_receive);
        mProcessConnectionDialog.setPositiveText(receiveText, positiveView -> {
            mConnectionController.accept(roomId);
            mProcessConnectionDialog.dismiss();
        });
        mProcessConnectionDialog.show();
    }

    private void onBattleScoreChanged(List<BattleUser> battleUsers) {
        if (battleUsers.isEmpty() || mConnectedUserList.isEmpty()) {
            stopDisplayBattleResult();
            return;
        }

        final Map<String, BattleUser> battleUserMap = new HashMap<>();
        for (BattleUser user : mBattleState.mBattledUsers.get()) {
            battleUserMap.put(user.userId, user);
        }
        // single battle: only 2 users in connecting and battling (1v1 battle)
        final Map<String, BattleUser> singleBattleUserMap = new HashMap<>();
        if (mConnectedUserList.size() == 2) {
            for (ConnectionState.ConnectionUser connectionUser : mConnectedUserList) {
                BattleState.BattleUser battleUser = battleUserMap.get(connectionUser.userId);
                if (battleUser != null) {
                    singleBattleUserMap.put(battleUser.userId, battleUser);
                }
            }
        }
        boolean isSingleBattle = singleBattleUserMap.size() == 2;
        LiveKitLog.info(TAG + " onBattleChanged isSingleBattle: " + isSingleBattle);
        for (ConnectionUser connectionUser : mConnectedUserList) {
            BattleUser battleUser = battleUserMap.get(connectionUser.userId);
            VideoView videoView = mLiveController.getVideoViewFactory().findVideoView(connectionUser.userId);
            if (videoView == null) {
                continue;
            }
            BattleMemberInfoView memberInfoView = videoView.findViewById(R.id.fl_battle_member_info_view);
            if (isSingleBattle) {
                memberInfoView.reset();
            } else {
                memberInfoView.setData(battleUser);
            }
        }
        if (mBattleInfoView != null) {
            mBattleInfoView.onBattleScoreChanged();
        }
    }

    private void stopDisplayBattleResult() {
        LiveKitLog.info(TAG + " stopDisplayBattleResult");
        removeCallbacks(mBattleEndInfoRemoveTask);
        if (mBattleInfoView != null) {
            mBattleInfoView.stopDisplayBattleResult();
        }
        VideoViewFactory viewFactory = mLiveController.getVideoViewFactory();
        for (ConnectionUser connectionUser : mConnectionState.connectedUsers.get()) {
            VideoView videoView = viewFactory.findVideoView(connectionUser.userId);
            if (videoView == null) {
                return;
            }
            BattleMemberInfoView memberInfoView = videoView.findViewById(R.id.fl_battle_member_info_view);
            memberInfoView.reset();
        }
        VideoView videoView = viewFactory.findVideoView(mUserState.selfInfo.userId);
        if (videoView != null) {
            BattleMemberInfoView memberInfoView = videoView.findViewById(R.id.fl_battle_member_info_view);
            if (memberInfoView != null) {
                memberInfoView.reset();
            }
        }
    }

    private void onBattleStartChange(Boolean start) {
        if (Boolean.TRUE.equals(start)) {
            onBattleStart();
        } else if (Boolean.FALSE.equals(start)) {
            onBattleEnd();
        }
    }

    private void onBattleStart() {
        if (mBattleInfoView != null) {
            FrameLayout.LayoutParams params = new LayoutParams(
                    ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
            params.topMargin = ((LayoutParams) mLayoutVideoList.getLayoutParams()).topMargin
                    - ScreenUtil.dip2px(2);
            params.height = mLayoutVideoList.getLayoutParams().height;
            mBattleInfoView.setLayoutParams(params);
            mBattleInfoView.onBattleStart();
            mBattleInfoView.setVisibility(VISIBLE);
        }
        for (ConnectionUser connectionUser : mConnectedUserList) {
            VideoView videoView = mLiveController.getVideoViewFactory().findVideoView(connectionUser.userId);
            if (videoView == null) {
                continue;
            }
            BattleMemberInfoView memberInfoView = videoView.findViewById(R.id.fl_battle_member_info_view);
            memberInfoView.reset();
        }
    }

    private void onBattleEnd() {
        final Map<String, BattleUser> battleUserMap = new HashMap<>();
        for (BattleUser user : mBattleState.mBattledUsers.get()) {
            battleUserMap.put(user.userId, user);
        }
        // single battle: only 2 users in connecting and battling (1v1 battle)
        final Map<String, BattleUser> singleBattleUserMap = new HashMap<>();
        if (mConnectedUserList.size() == 2) {
            for (ConnectionState.ConnectionUser connectionUser : mConnectedUserList) {
                BattleState.BattleUser battleUser = battleUserMap.get(connectionUser.userId);
                if (battleUser != null) {
                    singleBattleUserMap.put(battleUser.userId, battleUser);
                }
            }
        }
        boolean isSingleBattle = singleBattleUserMap.size() == 2;
        LiveKitLog.info(TAG + " onBattleEnd isSingleBattle: " + isSingleBattle);
        for (ConnectionUser connectionUser : mConnectedUserList) {
            BattleUser battleUser = battleUserMap.get(connectionUser.userId);
            VideoView videoView = mLiveController.getVideoViewFactory().findVideoView(connectionUser.userId);
            if (videoView == null) {
                continue;
            }
            BattleMemberInfoView memberInfoView = videoView.findViewById(R.id.fl_battle_member_info_view);
            if (!isSingleBattle) {
                memberInfoView.setData(battleUser);
            }
        }
        postDelayed(mBattleEndInfoRemoveTask, BattleState.BATTLE_END_INFO_DURATION * 1000);

        if (mBattleInfoView != null) {
            mBattleInfoView.onBattleEnd();
        }
    }

    private void onReceivedBattleRequestChange(BattleState.BattleUser user) {
        if (mProcessBattleDialog != null) {
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
        }
        if (user == null) {
            return;
        }
        String content = user.userName + " " + getContext().getString(R.string.livekit_battle_inviting);
        mProcessBattleDialog = new StandardDialog(getContext());
        mProcessBattleDialog.setContent(content);
        mProcessBattleDialog.setAvatar(user.avatarUrl);

        String rejectText = getContext().getString(R.string.livekit_reject);
        mProcessBattleDialog.setNegativeText(rejectText, negativeView -> {
            mBattleController.reject();
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
        });

        String receiveText = getContext().getString(R.string.livekit_receive);
        mProcessBattleDialog.setPositiveText(receiveText, positiveView -> {
            mBattleController.accept();
            mProcessBattleDialog.dismiss();
            mProcessBattleDialog = null;
        });
        mProcessBattleDialog.show();
    }
}
