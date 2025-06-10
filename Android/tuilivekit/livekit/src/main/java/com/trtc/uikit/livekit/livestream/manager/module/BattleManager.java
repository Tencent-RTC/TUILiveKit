package com.trtc.uikit.livekit.livestream.manager.module;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.rtmp.TXLiveBase;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.state.BattleState;
import com.trtc.uikit.livekit.livestream.state.LiveState;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

public final class BattleManager extends BaseManager {

    private final Handler mMainHandler = new Handler(Looper.getMainLooper());

    public BattleManager(LiveState state, ILiveService service) {
        super(state, service);
    }

    public void onRequestBattle(String battleId, List<String> requestedUserIdList) {
        mBattleState.mBattleId = battleId;
        mBattleState.mIsInWaiting.setValue(true);
        mBattleState.mSentBattleRequests.getValue().addAll(requestedUserIdList);
        mBattleState.mSentBattleRequests.setValue(mBattleState.mSentBattleRequests.getValue());
    }

    public void onCanceledBattle() {
        mBattleState.mIsInWaiting.setValue(false);
        mBattleState.mSentBattleRequests.getValue().clear();
        mBattleState.mSentBattleRequests.setValue(mBattleState.mSentBattleRequests.getValue());
    }

    public void onResponseBattle() {
        removeBattleRequestReceived();
    }

    public void onExitBattle() {
        mBattleState.reset();
    }

    public void resetOnDisplayResult() {
        mMainHandler.removeCallbacksAndMessages(null);
        if (Boolean.TRUE.equals(mBattleState.mIsOnDisplayResult.getValue())) {
            mBattleState.mIsOnDisplayResult.setValue(false);
        }
    }

    public boolean isBattleDraw() {
        List<BattleState.BattleUser> list = mBattleState.mBattledUsers.getValue();
        if (list.isEmpty()) {
            return false;
        }
        BattleState.BattleUser firstUser = list.get(0);
        BattleState.BattleUser lastUser = list.get(list.size() - 1);
        return firstUser.ranking == lastUser.ranking;
    }

    public boolean isSelfInBattle() {
        List<BattleState.BattleUser> userList = mBattleState.mBattledUsers.getValue();
        for (BattleState.BattleUser user : userList) {
            if (TextUtils.equals(mUserState.selfInfo.userId, user.userId)) {
                return true;
            }
        }
        return false;
    }

    public void onBattleStarted(TUILiveBattleManager.BattleInfo battleInfo) {
        if (Boolean.TRUE.equals(mBattleState.mIsBattleRunning.getValue())) {
            return;
        }
        mBattleState.mBattleId = battleInfo.battleId;
        mBattleState.mBattleConfig.copy(battleInfo.config);
        int duration = (int) (battleInfo.config.duration + battleInfo.startTime - getCurrentTimestamp() / 1000);
        duration = Math.min(duration, battleInfo.config.duration);
        duration = Math.max(duration, 0);
        mBattleState.mDurationCountDown.setValue(duration);
        mMainHandler.postDelayed(new Runnable() {
            @Override
            public void run() {
                int t = mBattleState.mDurationCountDown.getValue();
                if (t > 0) {
                    mBattleState.mDurationCountDown.setValue(t - 1);
                    mMainHandler.postDelayed(this, 1000);
                }
            }
        }, 1000);
        List<TUILiveBattleManager.BattleUser> users = new ArrayList<>(battleInfo.inviteeList);
        users.add(battleInfo.inviter);
        List<BattleState.BattleUser> list = mBattleState.mBattledUsers.getValue();
        for (TUILiveBattleManager.BattleUser user : users) {
            BattleState.BattleUser battleUser = new BattleState.BattleUser(user);
            battleUser.score = user.score;
            list.add(battleUser);
        }
        sortBattleUsersByScore(list);
        mBattleState.mIsInWaiting.setValue(false);
        mBattleState.mIsBattleRunning.setValue(true);
        mBattleState.mBattledUsers.setValue(list);
        mBattleState.mIsShowingStartView = true;
    }

    public void onBattleEnded(TUILiveBattleManager.BattleInfo battleInfo) {
        mMainHandler.removeCallbacksAndMessages(null);
        mBattleState.mIsShowingStartView = false;
        mBattleState.mBattleId = "";
        mBattleState.mBattleConfig.reset();
        mBattleState.mSentBattleRequests.getValue().clear();
        mBattleState.mSentBattleRequests.setValue(mBattleState.mSentBattleRequests.getValue());
        List<TUILiveBattleManager.BattleUser> users = new ArrayList<>(battleInfo.inviteeList);
        users.add(battleInfo.inviter);
        List<BattleState.BattleUser> list = mBattleState.mBattledUsers.getValue();
        for (TUILiveBattleManager.BattleUser user : users) {
            for (BattleState.BattleUser battleUser : list) {
                if (battleUser.userId.equals(user.userId)) {
                    battleUser.score = user.score;
                    break;
                }
            }
        }
        sortBattleUsersByScore(list);
        mBattleState.mBattledUsers.setValue(list);
        mBattleState.mIsBattleRunning.setValue(false);
        mMainHandler.removeCallbacksAndMessages(null);
        if (mCoHostState.connectedUsers.getValue().isEmpty()) {
            mBattleState.mIsOnDisplayResult.setValue(false);
            mBattleState.reset();
            return;
        }
        mBattleState.mIsOnDisplayResult.setValue(true);
        mMainHandler.postDelayed(
                () -> {
                    mBattleState.mIsOnDisplayResult.setValue(false);
                    mBattleState.reset();
                },
                BattleState.BATTLE_END_INFO_DURATION * 1000);
    }

    public void onBattleScoreChanged(List<TUILiveBattleManager.BattleUser> users) {
        List<BattleState.BattleUser> list = mBattleState.mBattledUsers.getValue();
        for (TUILiveBattleManager.BattleUser user : users) {
            for (BattleState.BattleUser battleUser : list) {
                if (battleUser.userId.equals(user.userId)) {
                    battleUser.score = user.score;
                    break;
                }
            }
        }
        sortBattleUsersByScore(list);
        mBattleState.mBattledUsers.setValue(list);
    }

    public void onUserExitBattle(TUILiveBattleManager.BattleUser user) {
        List<BattleState.BattleUser> users = mBattleState.mBattledUsers.getValue();
        BattleState.BattleUser exitUser = null;
        for (BattleState.BattleUser battleUser : users) {
            if (battleUser.userId.equals(user.userId)) {
                exitUser = battleUser;
                break;
            }
        }
        if (users.size() == 2) {
            return;
        }
        if (exitUser != null) {
            users.remove(exitUser);
            sortBattleUsersByScore(users);
            mBattleState.mBattledUsers.setValue(users);
        }
    }

    public void onBattleRequestReceived(String battleId, TUILiveBattleManager.BattleUser inviter) {
        mBattleState.mBattleId = battleId;
        mBattleState.mReceivedBattleRequest.setValue(new BattleState.BattleUser(inviter));
    }

    public void onBattleRequestCancelled(TUILiveBattleManager.BattleUser inviter) {
        removeBattleRequestReceived();
        Context context = ContextProvider.getApplicationContext();
        String toast = inviter.userName + " " + context.getString(R.string.common_battle_inviter_cancel);
        showToast(toast);
    }

    public void onBattleRequestAccept(TUILiveBattleManager.BattleUser invitee) {
        removeSentBattleRequest(invitee.userId);
    }

    public void onBattleRequestReject(TUILiveBattleManager.BattleUser invitee) {
        removeSentBattleRequest(invitee.userId);
        Context context = ContextProvider.getApplicationContext();
        String toast = invitee.userName + " " + context.getString(R.string.common_battle_invitee_reject);
        showToast(toast);
    }

    public void onBattleRequestTimeout(TUILiveBattleManager.BattleUser inviter,
                                       TUILiveBattleManager.BattleUser invitee) {
        if (TextUtils.equals(inviter.userId, mUserState.selfInfo.userId)) {
            mBattleState.mSentBattleRequests.getValue().clear();
            mBattleState.mSentBattleRequests.setValue(mBattleState.mSentBattleRequests.getValue());
            mBattleState.mIsInWaiting.setValue(false);
        } else {
            removeBattleRequestReceived();
            removeSentBattleRequest(invitee.userId);
        }
        Context context = ContextProvider.getApplicationContext();
        showToast(context.getString(R.string.common_battle_invitation_timeout));
    }

    @Override
    public void destroy() {
        mMainHandler.removeCallbacksAndMessages(null);
        mBattleState.reset();
    }

    private long getCurrentTimestamp() {
        long networkTimestamp = TXLiveBase.getNetworkTimestamp();
        long localTimestamp = System.currentTimeMillis();
        return networkTimestamp > 0 ? networkTimestamp : localTimestamp;
    }

    private void sortBattleUsersByScore(List<BattleState.BattleUser> users) {
        Object[] array = users.toArray();
        Arrays.sort(array, (o1, o2) -> ((BattleState.BattleUser) o2).score - ((BattleState.BattleUser) o1).score);
        ListIterator<BattleState.BattleUser> listIterator = users.listIterator();

        for (int i = 0; i < array.length; i++) {
            listIterator.next();
            BattleState.BattleUser user = (BattleState.BattleUser) array[i];
            if (i == 0) {
                user.ranking = 1;
            } else {
                BattleState.BattleUser preUser = (BattleState.BattleUser) array[i - 1];
                // same ranking for same score
                user.ranking = (preUser.score == user.score) ? preUser.ranking : preUser.ranking + 1;
            }
            listIterator.set(user);
        }
    }

    private void removeBattleRequestReceived() {
        mBattleState.mReceivedBattleRequest.setValue(null);
    }

    private void removeSentBattleRequest(String userId) {
        List<String> sendRequests = mBattleState.mSentBattleRequests.getValue();
        Iterator<String> iterator = sendRequests.iterator();
        while (iterator.hasNext()) {
            String sendUserId = iterator.next();
            if (TextUtils.equals(sendUserId, userId)) {
                iterator.remove();
                break;
            }
        }
        if (sendRequests.isEmpty()) {
            mBattleState.mIsInWaiting.setValue(false);
        }
        mBattleState.mSentBattleRequests.setValue(sendRequests);
    }


    private static void showToast(String tips) {
        Context context = ContextProvider.getApplicationContext();
        View view = LayoutInflater.from(context).inflate(R.layout.livekit_connection_toast, null, false);

        TextView text = view.findViewById(R.id.tv_toast_text);
        text.setText(tips);
        ImageView image = view.findViewById(R.id.iv_toast_image);
        image.setImageResource(R.drawable.livekit_connection_toast_icon);

        Toast toast = new Toast(view.getContext());
        toast.setDuration(Toast.LENGTH_SHORT);
        toast.setView(view);
        toast.show();
    }
}
