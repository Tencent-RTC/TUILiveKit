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
        mBattleState.mIsInWaiting.set(true);
        mBattleState.mSentBattleRequests.addAll(requestedUserIdList);
    }

    public void onCanceledBattle() {
        mBattleState.mIsInWaiting.set(false);
        mBattleState.mSentBattleRequests.clear();
    }

    public void onResponseBattle() {
        removeBattleRequestReceived();
    }

    public void onExitBattle() {
        mBattleState.reset();
    }

    public void resetOnDisplayResult() {
        mMainHandler.removeCallbacksAndMessages(null);
        if (Boolean.TRUE.equals(mBattleState.mIsOnDisplayResult.get())) {
            mBattleState.mIsOnDisplayResult.set(false);
        }
    }

    public boolean isBattleDraw() {
        List<BattleState.BattleUser> list = mBattleState.mBattledUsers.get();
        if (list.isEmpty()) {
            return false;
        }
        BattleState.BattleUser firstUser = list.get(0);
        BattleState.BattleUser lastUser = list.get(list.size() - 1);
        return firstUser.ranking == lastUser.ranking;
    }

    public void onBattleStarted(TUILiveBattleManager.BattleInfo battleInfo) {
        if (Boolean.TRUE.equals(mBattleState.mIsBattleRunning.get())) {
            return;
        }
        mBattleState.mBattleId = battleInfo.battleId;
        mBattleState.mBattleConfig.copy(battleInfo.config);
        int duration = (int) (battleInfo.config.duration + battleInfo.startTime - System.currentTimeMillis() / 1000);
        mBattleState.mDurationCountDown.set(duration);
        mMainHandler.postDelayed(new Runnable() {
            @Override
            public void run() {
                int t = mBattleState.mDurationCountDown.get();
                if (t > 0) {
                    mBattleState.mDurationCountDown.set(t - 1);
                    mMainHandler.postDelayed(this, 1000);
                }
            }
        }, 1000);
        List<TUILiveBattleManager.BattleUser> users = new ArrayList<>(battleInfo.inviteeList);
        users.add(battleInfo.inviter);
        List<BattleState.BattleUser> list = mBattleState.mBattledUsers.get();
        for (TUILiveBattleManager.BattleUser user : users) {
            BattleState.BattleUser battleUser = new BattleState.BattleUser(user);
            battleUser.score = user.score;
            list.add(battleUser);
        }
        sortBattleUsersByScore(list);
        mBattleState.mIsInWaiting.set(false);
        mBattleState.mIsBattleRunning.set(true);
        mBattleState.mBattledUsers.set(list);
        mBattleState.mIsShowingStartView = true;
    }

    public void onBattleEnded(TUILiveBattleManager.BattleInfo battleInfo) {
        mMainHandler.removeCallbacksAndMessages(null);
        mBattleState.mIsShowingStartView = false;
        mBattleState.mBattleId = "";
        mBattleState.mBattleConfig.reset();
        mBattleState.mSentBattleRequests.clear();
        List<TUILiveBattleManager.BattleUser> users = new ArrayList<>(battleInfo.inviteeList);
        users.add(battleInfo.inviter);
        List<BattleState.BattleUser> list = mBattleState.mBattledUsers.get();
        for (TUILiveBattleManager.BattleUser user : users) {
            for (BattleState.BattleUser battleUser : list) {
                if (battleUser.userId.equals(user.userId)) {
                    battleUser.score = user.score;
                    break;
                }
            }
        }
        sortBattleUsersByScore(list);
        mBattleState.mBattledUsers.set(list);
        mBattleState.mIsBattleRunning.set(false);
        mMainHandler.removeCallbacksAndMessages(null);
        if (mCoHostState.connectedUsers.get().isEmpty()) {
            mBattleState.mIsOnDisplayResult.set(false);
            mBattleState.reset();
            return;
        }
        mBattleState.mIsOnDisplayResult.set(true);
        mMainHandler.postDelayed(
                () -> {
                    mBattleState.mIsOnDisplayResult.set(false);
                    mBattleState.reset();
                },
                BattleState.BATTLE_END_INFO_DURATION * 1000);
    }

    public void onBattleScoreChanged(List<TUILiveBattleManager.BattleUser> users) {
        List<BattleState.BattleUser> list = mBattleState.mBattledUsers.get();
        for (TUILiveBattleManager.BattleUser user : users) {
            for (BattleState.BattleUser battleUser : list) {
                if (battleUser.userId.equals(user.userId)) {
                    battleUser.score = user.score;
                    break;
                }
            }
        }
        sortBattleUsersByScore(list);
        mBattleState.mBattledUsers.set(list);
    }

    public void onUserExitBattle(TUILiveBattleManager.BattleUser user) {
        List<BattleState.BattleUser> users = mBattleState.mBattledUsers.get();
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
            mBattleState.mBattledUsers.set(users);
        }
    }

    public void onBattleRequestReceived(String battleId, TUILiveBattleManager.BattleUser inviter) {
        mBattleState.mBattleId = battleId;
        mBattleState.mReceivedBattleRequest.set(new BattleState.BattleUser(inviter));
    }

    public void onBattleRequestCancelled(TUILiveBattleManager.BattleUser inviter) {
        removeBattleRequestReceived();
        Context context = ContextProvider.getApplicationContext();
        String toast = inviter.userName + " " + context.getString(R.string.livekit_battle_inviter_cancel);
        showToast(toast);
    }

    public void onBattleRequestAccept(TUILiveBattleManager.BattleUser invitee) {
        removeSentBattleRequest(invitee.userId);
    }

    public void onBattleRequestReject(TUILiveBattleManager.BattleUser invitee) {
        removeSentBattleRequest(invitee.userId);
        Context context = ContextProvider.getApplicationContext();
        String toast = invitee.userName + " " + context.getString(R.string.livekit_battle_invitee_reject);
        showToast(toast);
    }

    public void onBattleRequestTimeout(TUILiveBattleManager.BattleUser inviter,
                                       TUILiveBattleManager.BattleUser invitee) {
        if (TextUtils.equals(inviter.userId, mUserState.selfInfo.userId)) {
            mBattleState.mSentBattleRequests.clear();
            mBattleState.mIsInWaiting.set(false);
        } else {
            removeBattleRequestReceived();
            removeSentBattleRequest(invitee.userId);
        }
    }

    @Override
    public void destroy() {
        mMainHandler.removeCallbacksAndMessages(null);
        mBattleState.reset();
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
        mBattleState.mReceivedBattleRequest.set(null);
    }

    private void removeSentBattleRequest(String userId) {
        List<String> sendRequests = mBattleState.mSentBattleRequests.get();
        Iterator<String> iterator = sendRequests.iterator();
        while (iterator.hasNext()) {
            String sendUserId = iterator.next();
            if (TextUtils.equals(sendUserId, userId)) {
                iterator.remove();
                break;
            }
        }
        if (sendRequests.isEmpty()) {
            mBattleState.mIsInWaiting.set(false);
        }
        mBattleState.mSentBattleRequests.set(sendRequests);
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
