package com.trtc.uikit.livekit.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleStoppedReason;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleUser;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.BattleController;

import java.util.List;

public final class LiveBattleManagerObserver extends TUILiveBattleManager.Observer {
    private final String mTag = "LiveBattleManagerObserver[" + hashCode() + "]";

    private final BattleController mBattleController;

    public LiveBattleManagerObserver(LiveController controller) {
        super();
        mBattleController = controller.getBattleController();
    }

    @Override
    public void onBattleStarted(BattleInfo battleInfo) {
        LiveKitLog.info(mTag + " onBattleStarted:[battleInfo:" + new Gson().toJson(battleInfo) + "]");
        mBattleController.onBattleStarted(battleInfo);
    }

    @Override
    public void onBattleEnded(BattleInfo battleInfo, BattleStoppedReason reason) {
        LiveKitLog.info(mTag + " onBattleEnded:[battleInfo:"
                + new Gson().toJson(battleInfo) + ", reason:" + reason + "]");
        mBattleController.onBattleEnded(battleInfo);
    }

    @Override
    public void onUserJoinBattle(String battleId, BattleUser battleUser) {
        LiveKitLog.info(mTag + " onUserJoinBattle:[battleId:" + battleId
                + ", battleUser:" + new Gson().toJson(battleUser) + "]");
    }

    @Override
    public void onUserExitBattle(String battleId, BattleUser battleUser) {
        LiveKitLog.info(mTag + " onUserExitBattle:[battleId:" + battleId
                + ", battleUser:" + new Gson().toJson(battleUser) + "]");
        mBattleController.onUserExitBattle(battleUser);
    }

    @Override
    public void onBattleScoreChanged(String battleId, List<BattleUser> battleUserList) {
        LiveKitLog.info(mTag + " onBattleScoreChanged:[battleId:" + battleId
                + ", battleUser:" + new Gson().toJson(battleUserList) + "]");
        mBattleController.onBattleScoreChanged(battleUserList);
    }

    @Override
    public void onBattleRequestReceived(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        LiveKitLog.info(mTag + " onBattleRequestReceived:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleController.onBattleRequestReceived(battleInfo, inviter);
    }

    @Override
    public void onBattleRequestCancelled(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        LiveKitLog.info(mTag + " onBattleRequestCancelled:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleController.onBattleRequestCancelled(inviter);
    }

    @Override
    public void onBattleRequestTimeout(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        LiveKitLog.info(mTag + " onBattleRequestTimeout:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleController.onBattleRequestTimeout(inviter, invitee);
    }

    @Override
    public void onBattleRequestAccept(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        LiveKitLog.info(mTag + " onBattleRequestAccept:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleController.onBattleRequestAccept(invitee);
    }

    @Override
    public void onBattleRequestReject(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        LiveKitLog.info(mTag + " onBattleRequestReject:[battleInfo:" + new Gson().toJson(battleInfo)
                + ", inviter:" + new Gson().toJson(inviter) + ", invitee:" + new Gson().toJson(invitee) + "]");
        mBattleController.onBattleRequestReject(invitee);
    }
}
