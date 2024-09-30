package com.trtc.uikit.livekit.manager.controller;

import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_TOAST;

import android.content.Context;
import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.state.LiveState;
import com.trtc.uikit.livekit.state.operation.BattleState;
import com.trtc.uikit.livekit.state.operation.ConnectionState;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

public final class BattleController extends Controller {

    private static final String TAG = "BattleController";

    public BattleController(LiveState state, ILiveService service) {
        super(state, service);
    }

    public void requestBattle(List<String> roomIdList, int timeout) {
        mBattleState.mIsInWaiting.set(true);
        TUILiveBattleManager.BattleConfig config = new TUILiveBattleManager.BattleConfig();
        config.duration = BattleState.BATTLE_DURATION;
        config.needResponse = mBattleState.mNeedResponse;
        config.extensionInfo = "";
        mLiveService.requestBattle(config, roomIdList, timeout, new TUILiveBattleManager.BattleRequestCallback() {
            @Override
            public void onSuccess(TUILiveBattleManager.BattleInfo battleInfo,
                                  Map<String, TUILiveBattleManager.BattleCode> map) {
                mBattleState.mBattleId = battleInfo.battleId;
                mBattleState.mBattleConfig.copy(config);
                List<BattleState.BattleUser> sendRequests = mBattleState.mSentBattleRequests.get();
                for (Map.Entry<String, TUILiveBattleManager.BattleCode> entry : map.entrySet()) {
                    String key = entry.getKey();
                    TUILiveBattleManager.BattleCode code = entry.getValue();
                    if (code == TUILiveBattleManager.BattleCode.SUCCESS) {
                        for (ConnectionState.ConnectionUser user : mConnectionState.connectedUsers.get()) {
                            if (TextUtils.equals(user.userId, key)) {
                                sendRequests.add(new BattleState.BattleUser(user));
                                break;
                            }
                        }
                    } else {
                        notifyToast(convertCodeToString(entry.getValue()));
                    }
                }
                mBattleState.mSentBattleRequests.set(sendRequests);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                ErrorHandler.onError(error);
                mBattleState.mSentBattleRequests.clear();
                mBattleState.mIsInWaiting.set(false);
            }
        });
    }

    public void cancelBattleRequest() {
        mBattleState.mIsInWaiting.set(false);
        List<String> userIdList = new ArrayList<>();
        for (BattleState.BattleUser user : mBattleState.mSentBattleRequests.get()) {
            userIdList.add(user.userId);
        }
        mLiveService.cancelBattleRequest(mBattleState.mBattleId, userIdList, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mBattleState.mSentBattleRequests.clear();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {

            }
        });
    }

    public void accept() {
        mLiveService.acceptBattle(mBattleState.mBattleId, null);
    }

    public void reject() {
        mLiveService.rejectBattle(mBattleState.mBattleId, null);
    }

    public void exitBattle() {
        mLiveService.exitBattle(mBattleState.mBattleId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mBattleState.mSentBattleRequests.clear();
                mBattleState.mBattledUsers.clear();
                removeBattleRequestReceived();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {

            }
        });
    }

    public void resetOnDisplayResult() {
        mBattleState.mIsOnDisplayResult.set(false);
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
        mBattleState.mBattleId = battleInfo.battleId;
        mBattleState.mBattleConfig.copy(battleInfo.config);
        mBattleState.mBattleConfig.duration =
                (int) (battleInfo.config.duration + battleInfo.startTime - System.currentTimeMillis() / 1000);
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
    }

    public void onBattleEnded(TUILiveBattleManager.BattleInfo battleInfo) {
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
        mBattleState.mIsOnDisplayResult.set(true);
        mBattleState.reset();
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

    public void onBattleRequestReceived(TUILiveBattleManager.BattleInfo battleInfo,
                                        TUILiveBattleManager.BattleUser inviter) {
        mBattleState.mBattleId = battleInfo.battleId;
        mBattleState.mReceivedBattleRequest.set(new BattleState.BattleUser(inviter));
    }

    public void onBattleRequestCancelled(TUILiveBattleManager.BattleUser inviter) {
        removeBattleRequestReceived();
        Context context = ContextProvider.getApplicationContext();
        String toast = inviter.userName + " " + context.getString(R.string.livekit_battle_inviter_cancel);
        notifyToast(toast);
    }

    public void onBattleRequestAccept(TUILiveBattleManager.BattleUser invitee) {
        removeSentBattleRequest(invitee.userId);
    }

    public void onBattleRequestReject(TUILiveBattleManager.BattleUser invitee) {
        removeSentBattleRequest(invitee.userId);
        Context context = ContextProvider.getApplicationContext();
        String toast = invitee.userName + " " + context.getString(R.string.livekit_battle_invitee_reject);
        notifyToast(toast);
    }

    public void onBattleRequestTimeout(TUILiveBattleManager.BattleUser inviter,
                                       TUILiveBattleManager.BattleUser invitee) {
        if (TextUtils.equals(inviter.userId, mUserState.selfInfo.userId)) {
            mBattleState.mSentBattleRequests.clear();
        } else {
            removeBattleRequestReceived();
            removeSentBattleRequest(invitee.userId);
        }
    }

    @Override
    public void destroy() {
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
        List<BattleState.BattleUser> sendRequests = mBattleState.mSentBattleRequests.get();
        Iterator<BattleState.BattleUser> iterator = sendRequests.iterator();
        while (iterator.hasNext()) {
            BattleState.BattleUser user = iterator.next();
            if (TextUtils.equals(user.userId, userId)) {
                iterator.remove();
                break;
            }
        }
        if (sendRequests.isEmpty()) {
            mBattleState.mIsInWaiting.set(false);
        }
        mBattleState.mSentBattleRequests.set(sendRequests);
    }

    private static void notifyToast(String toast) {
        Map<String, Object> params = new HashMap<>();
        params.put("Toast", toast);
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_TOAST, params);
    }

    private static String convertCodeToString(TUILiveBattleManager.BattleCode code) {
        Context context = ContextProvider.getApplicationContext();
        switch (code) {
            case BATTLING:
            case BATTLING_OTHER_ROOM:
                return context.getString(R.string.livekit_battle_error_conflict);
            default:
                return context.getString(R.string.livekit_battle_error_other);
        }
    }
}
