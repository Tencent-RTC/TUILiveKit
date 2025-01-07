package com.trtc.uikit.livekit.livestreamcore.manager.module;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleCode;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleConfig;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleStoppedReason;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleUser;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.BattleRequestCallback;
import com.trtc.uikit.livekit.livestreamcore.R;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.BattleState;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class BattleManager extends BaseManager {

    private final BattleState    mBattleState;
    private       BattleObserver mBattleObserver;

    public BattleManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
        mBattleState = state.battleState;
    }

    public void setBattleObserver(BattleObserver observer) {
        mBattleObserver = observer;
    }

    public BattleUser getBattleUser(String userId) {
        for (BattleUser battleUser : mBattleState.mBattledUsers.get()) {
            if (TextUtils.equals(userId, battleUser.userId)) {
                return battleUser;
            }
        }
        return null;
    }

    public void requestBattle(BattleConfig config, List<String> roomIdList, int timeout,
                              BattleRequestCallback callback) {
        mVideoLiveService.requestBattle(config, roomIdList, timeout, new TUILiveBattleManager.BattleRequestCallback() {
            @Override
            public void onSuccess(TUILiveBattleManager.BattleInfo battleInfo,
                                  Map<String, TUILiveBattleManager.BattleCode> map) {
                mBattleState.battleId = battleInfo.battleId;
                List<String> sentBattleRequestList = mBattleState.sentBattleRequestList.get();
                for (Map.Entry<String, TUILiveBattleManager.BattleCode> entry : map.entrySet()) {
                    String key = entry.getKey();
                    TUILiveBattleManager.BattleCode code = entry.getValue();
                    if (code == TUILiveBattleManager.BattleCode.SUCCESS) {
                        for (ConnectionUser user : mVideoLiveState.coHostState.connectedUserList.get()) {
                            if (TextUtils.equals(user.userId, key)) {
                                sentBattleRequestList.add(user.userId);
                                break;
                            }
                        }
                    } else {
                        BattleErrorHandler.onError(code);
                    }
                }
                if (callback != null) {
                    callback.onSuccess(battleInfo.battleId, sentBattleRequestList);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                mBattleState.battleId = "";
                mBattleState.sentBattleRequestList.get().clear();
                if (callback != null) {
                    callback.onError(TUICommonDefine.Error.FAILED, s);
                }
            }
        });
    }

    public void cancelRequest(String battleId, List<String> userIdList, TUIRoomDefine.ActionCallback callback) {
        List<String> sendUserList = mBattleState.sentBattleRequestList.get();
        List<String> validUserIdList = new ArrayList<>();
        for (String userId : userIdList) {
            for (String sendUserId : sendUserList) {
                if (TextUtils.equals(sendUserId, userId)) {
                    validUserIdList.add(userId);
                    break;
                }
            }
        }
        mVideoLiveService.cancelBattleRequest(battleId, validUserIdList, callback);
    }

    public void acceptBattle(String battleId, TUIRoomDefine.ActionCallback callback) {
        mVideoLiveService.acceptBattle(battleId, callback);
    }

    public void rejectBattle(String battleId, TUIRoomDefine.ActionCallback callback) {
        mVideoLiveService.rejectBattle(battleId, callback);
    }

    public void exitBattle(String battleId, TUIRoomDefine.ActionCallback callback) {
        mVideoLiveService.exitBattle(battleId, callback);
    }

    public void onBattleStarted(BattleInfo battleInfo) {
        List<BattleUser> battleUserList = mBattleState.mBattledUsers.get();
        battleUserList.add(battleInfo.inviter);
        battleUserList.addAll(battleInfo.inviteeList);
        mBattleState.mBattledUsers.set(battleUserList);
        if (mBattleObserver != null) {
            mBattleObserver.onBattleStarted(battleInfo);
        }
    }

    public void onBattleEnded(BattleInfo battleInfo, BattleStoppedReason reason) {
        mBattleState.mBattledUsers.clear();
        if (mBattleObserver != null) {
            mBattleObserver.onBattleEnded(battleInfo);
        }
    }

    public void onUserJoinBattle(String battleId, BattleUser battleUser) {
        if (mBattleObserver != null) {
            mBattleObserver.onUserJoinBattle(battleId, battleUser);
        }
    }

    public void onUserExitBattle(String battleId, BattleUser battleUser) {
        if (mBattleObserver != null) {
            mBattleObserver.onUserExitBattle(battleId, battleUser);
        }
    }

    public void onBattleScoreChanged(String battleId, List<BattleUser> battleUserList) {
        if (mBattleObserver != null) {
            mBattleObserver.onBattleScoreChanged(battleId, battleUserList);
        }
    }

    public void onBattleRequestReceived(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        if (mBattleObserver != null) {
            mBattleObserver.onBattleRequestReceived(battleInfo.battleId, inviter, invitee);
        }
    }

    public void onBattleRequestCancelled(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        if (mBattleObserver != null) {
            mBattleObserver.onBattleRequestCancelled(battleInfo.battleId, inviter, invitee);
        }
    }

    public void onBattleRequestTimeout(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        if (mBattleObserver != null) {
            mBattleObserver.onBattleRequestTimeout(battleInfo.battleId, inviter, invitee);
        }
    }

    public void onBattleRequestAccept(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        if (mBattleObserver != null) {
            mBattleObserver.onBattleRequestAccept(battleInfo.battleId, inviter, invitee);
        }
    }

    public void onBattleRequestReject(BattleInfo battleInfo, BattleUser inviter, BattleUser invitee) {
        if (mBattleObserver != null) {
            mBattleObserver.onBattleRequestReject(battleInfo.battleId, inviter, invitee);
        }
    }

    @Override
    public void destroy() {

    }

    public interface BattleObserver {
        void onBattleStarted(BattleInfo battleInfo);

        void onBattleEnded(BattleInfo battleInfo);

        void onUserJoinBattle(String battleId, BattleUser battleUser);

        void onUserExitBattle(String battleId, BattleUser battleUser);

        void onBattleScoreChanged(String battleId, List<BattleUser> battleUserList);

        void onBattleRequestReceived(String battleId, BattleUser inviter, BattleUser invitee);

        void onBattleRequestCancelled(String battleId, BattleUser inviter, BattleUser invitee);

        void onBattleRequestTimeout(String battleId, BattleUser inviter, BattleUser invitee);

        void onBattleRequestAccept(String battleId, BattleUser inviter, BattleUser invitee);

        void onBattleRequestReject(String battleId, BattleUser inviter, BattleUser invitee);
    }

    private static class BattleErrorHandler {

        public static void onError(BattleCode code) {
            if (code == BattleCode.SUCCESS || code == null) {
                return;
            }
            String message = convertCodeToString(code);
            Logger.info("BattleErrorHandler :[code:" + code + ",message:" + message + "]");
            showToast(message);
        }

        private static String convertCodeToString(BattleCode code) {
            Context context = ContextProvider.getApplicationContext();
            switch (code) {
                case BATTLING:
                case BATTLING_OTHER_ROOM:
                    return context.getString(R.string.livestreamcore_battle_error_conflict);
                default:
                    return context.getString(R.string.livestreamcore_battle_error_other);
            }
        }

        private static void showToast(String tips) {
            Context context = ContextProvider.getApplicationContext();
            View view = LayoutInflater.from(context).inflate(R.layout.livestreamcore_connection_toast, null, false);

            TextView text = view.findViewById(R.id.tv_toast_text);
            text.setText(tips);
            ImageView image = view.findViewById(R.id.iv_toast_image);
            image.setImageResource(R.drawable.livestreamcore_connection_toast_icon);

            Toast toast = new Toast(view.getContext());
            toast.setDuration(Toast.LENGTH_SHORT);
            toast.setView(view);
            toast.show();
        }
    }
}
