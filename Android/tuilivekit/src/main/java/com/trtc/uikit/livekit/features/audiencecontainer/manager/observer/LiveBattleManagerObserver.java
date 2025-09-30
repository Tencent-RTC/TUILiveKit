package com.trtc.uikit.livekit.features.audiencecontainer.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleUser;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.BattleManager;
import io.trtc.tuikit.atomicxcore.api.deprecated.LiveCoreViewDefine;

import java.lang.ref.WeakReference;
import java.util.List;

public final class LiveBattleManagerObserver implements LiveCoreViewDefine.BattleObserver {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("LiveBattleManagerObserver");

    private final WeakReference<BattleManager> mBattleManagerWeak;

    public LiveBattleManagerObserver(AudienceManager manager) {
        mBattleManagerWeak = new WeakReference<>(manager.getBattleManager());
    }

    @Override
    public void onBattleStarted(BattleInfo battleInfo) {
        LOGGER.info(hashCode() + " onBattleStarted:[battleInfo:" + new Gson().toJson(battleInfo) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onBattleStarted(battleInfo);
        }
    }

    @Override
    public void onBattleEnded(BattleInfo battleInfo) {
        LOGGER.info(hashCode() + " onBattleEnded:[battleInfo:" + new Gson().toJson(battleInfo) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onBattleEnded(battleInfo);
        }
    }

    @Override
    public void onUserJoinBattle(String battleId, BattleUser battleUser) {
        LOGGER.info(hashCode() + " onUserJoinBattle:[battleId:" + battleId
                + ",battleUser:" + new Gson().toJson(battleUser) + "]");
    }

    @Override
    public void onUserExitBattle(String battleId, BattleUser battleUser) {
        LOGGER.info(hashCode() + " onUserExitBattle:[battleId:" + battleId
                + ",battleUser:" + new Gson().toJson(battleUser) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onUserExitBattle(battleUser);
        }
    }

    @Override
    public void onBattleScoreChanged(String battleId, List<BattleUser> battleUserList) {
        LOGGER.info(hashCode() + " onBattleScoreChanged:[battleId:" + battleId
                + ",battleUserList:" + new Gson().toJson(battleUserList) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onBattleScoreChanged(battleUserList);
        }
    }

    @Override
    public void onBattleRequestReceived(String battleId, BattleUser inviter, BattleUser invitee) {
        LOGGER.info(hashCode() + " onBattleRequestReceived:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onBattleRequestReceived(battleId, inviter);
        }
    }

    @Override
    public void onBattleRequestCancelled(String battleId, BattleUser inviter, BattleUser invitee) {
        LOGGER.info(hashCode() + " onBattleRequestCancelled:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onBattleRequestCancelled(inviter);
        }
    }

    @Override
    public void onBattleRequestTimeout(String battleId, BattleUser inviter, BattleUser invitee) {
        LOGGER.info(hashCode() + " onBattleRequestTimeout:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onBattleRequestTimeout(inviter, invitee);
        }
    }

    @Override
    public void onBattleRequestAccept(String battleId, BattleUser inviter, BattleUser invitee) {
        LOGGER.info(hashCode() + " onBattleRequestAccept:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onBattleRequestAccept(invitee);
        }
    }

    @Override
    public void onBattleRequestReject(String battleId, BattleUser inviter, BattleUser invitee) {
        LOGGER.info(hashCode() + " onBattleRequestReject:[battleId:" + battleId
                + ",inviter:" + new Gson().toJson(inviter) + ",invitee:" + new Gson().toJson(invitee) + "]");
        BattleManager manager = mBattleManagerWeak.get();
        if (manager != null) {
            manager.onBattleRequestReject(invitee);
        }
    }
}
