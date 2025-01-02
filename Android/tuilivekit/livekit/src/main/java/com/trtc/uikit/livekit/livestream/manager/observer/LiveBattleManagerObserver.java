package com.trtc.uikit.livekit.livestream.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleUser;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.manager.module.BattleManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import java.util.List;

public final class LiveBattleManagerObserver implements LiveCoreViewDefine.BattleObserver {
    private final String mTag = "LiveBattleManagerObserver[" + hashCode() + "]";

    private final BattleManager mBattleManager;

    public LiveBattleManagerObserver(LiveStreamManager manager) {
        mBattleManager = manager.getBattleManager();
    }

    @Override
    public void onBattleStarted(BattleInfo battleInfo) {
        LiveStreamLog.info(mTag + " onBattleStarted:[battleInfo:" + new Gson().toJson(battleInfo) + "]");
        mBattleManager.onBattleStarted(battleInfo);
    }

    @Override
    public void onBattleEnded(BattleInfo battleInfo) {
        LiveStreamLog.info(mTag + " onBattleEnded:[battleInfo:" + new Gson().toJson(battleInfo) + "]");
        mBattleManager.onBattleEnded(battleInfo);
    }

    @Override
    public void onUserJoinBattle(String battleId, BattleUser battleUser) {
        LiveStreamLog.info(mTag + " onUserJoinBattle:[battleId:" + battleId
                + ",battleUser:" + new Gson().toJson(battleUser) + "]");
        mBattleManager.onUserExitBattle(battleUser);
    }

    @Override
    public void onUserExitBattle(String battleId, BattleUser battleUser) {
        LiveStreamLog.info(mTag + " onUserExitBattle:[battleId:" + battleId
                + ",battleUser:" + new Gson().toJson(battleUser) + "]");
        mBattleManager.onUserExitBattle(battleUser);
    }

    @Override
    public void onBattleScoreChanged(String battleId, List<BattleUser> battleUserList) {
        LiveStreamLog.info(mTag + " onBattleScoreChanged:[battleId:" + battleId
                + ",battleUserList:" + new Gson().toJson(battleUserList) + "]");
        mBattleManager.onBattleScoreChanged(battleUserList);
    }

    @Override
    public void onBattleRequestReceived(String battleId, BattleUser inviter, BattleUser invitee) {
        LiveStreamLog.info(mTag + " onBattleRequestReceived:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestReceived(battleId, inviter);
    }

    @Override
    public void onBattleRequestCancelled(String battleId, BattleUser inviter, BattleUser invitee) {
        LiveStreamLog.info(mTag + " onBattleRequestCancelled:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestCancelled(inviter);
    }

    @Override
    public void onBattleRequestTimeout(String battleId, BattleUser inviter, BattleUser invitee) {
        LiveStreamLog.info(mTag + " onBattleRequestTimeout:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestTimeout(inviter, invitee);
    }

    @Override
    public void onBattleRequestAccept(String battleId, BattleUser inviter, BattleUser invitee) {
        LiveStreamLog.info(mTag + " onBattleRequestAccept:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestAccept(invitee);
    }

    @Override
    public void onBattleRequestReject(String battleId, BattleUser inviter, BattleUser invitee) {
        LiveStreamLog.info(mTag + " onBattleRequestReject:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        mBattleManager.onBattleRequestReject(invitee);
    }
}
