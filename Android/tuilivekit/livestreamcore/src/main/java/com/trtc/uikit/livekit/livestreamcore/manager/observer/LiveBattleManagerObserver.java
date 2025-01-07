package com.trtc.uikit.livekit.livestreamcore.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleStoppedReason;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleUser;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.BattleManager;

import java.util.List;

public final class LiveBattleManagerObserver extends TUILiveBattleManager.Observer {
    private final String mTag = "LiveBattleManagerObserver[" + hashCode() + "]";

    private final BattleManager mBattleManager;

    public LiveBattleManagerObserver(LiveStreamManager manager) {
        super();
        mBattleManager = manager.getBattleManager();
    }

    @Override
    public void onBattleStarted(BattleInfo battleInfo) {
        Logger.info(mTag + " onBattleStarted:[battleInfo:" + new Gson().toJson(battleInfo) + "]");
        mBattleManager.onBattleStarted(battleInfo);
    }

    @Override
    public void onBattleEnded(BattleInfo battleInfo, BattleStoppedReason reason) {
        Logger.info(mTag + " onBattleEnded:[battleInfo:"
                + new Gson().toJson(battleInfo) + ", reason:" + reason + "]");
        mBattleManager.onBattleEnded(battleInfo, reason);
    }

    @Override
    public void onUserJoinBattle(String battleId, BattleUser battleUser) {
        Logger.info(mTag + " onUserJoinBattle:[battleId:" + battleId
                + ", battleUser:" + new Gson().toJson(battleUser) + "]");
        mBattleManager.onUserJoinBattle(battleId, battleUser);
    }

    @Override
    public void onUserExitBattle(String battleId, BattleUser battleUser) {
        Logger.info(mTag + " onUserExitBattle:[battleId:" + battleId
                + ", battleUser:" + new Gson().toJson(battleUser) + "]");
        mBattleManager.onUserExitBattle(battleId, battleUser);
    }

    @Override
    public void onBattleScoreChanged(String battleId, List<BattleUser> battleUserList) {
        Logger.info(mTag + " onBattleScoreChanged:[battleId:" + battleId
                + ", battleUser:" + new Gson().toJson(battleUserList) + "]");
        mBattleManager.onBattleScoreChanged(battleId, battleUserList);
    }

    @Override
    public void onBattleRequestReceived(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        Logger.info(mTag + " onBattleRequestReceived:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestReceived(battleInfo, inviter, invitee);
    }

    @Override
    public void onBattleRequestCancelled(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        Logger.info(mTag + " onBattleRequestCancelled:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestCancelled(battleInfo, inviter, invitee);
    }

    @Override
    public void onBattleRequestTimeout(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        Logger.info(mTag + " onBattleRequestTimeout:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestTimeout(battleInfo, inviter, invitee);
    }

    @Override
    public void onBattleRequestAccept(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        Logger.info(mTag + " onBattleRequestAccept:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestAccept(battleInfo, inviter, invitee);
    }

    @Override
    public void onBattleRequestReject(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        Logger.info(mTag + " onBattleRequestReject:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestReject(battleInfo, inviter, invitee);
    }
}
