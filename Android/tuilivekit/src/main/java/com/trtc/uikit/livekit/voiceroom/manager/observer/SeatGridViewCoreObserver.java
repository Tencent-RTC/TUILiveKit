package com.trtc.uikit.livekit.voiceroom.manager.observer;

import android.content.Context;
import android.view.View;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.view.seatmanager.ListMenuInfo;
import com.trtc.uikit.livekit.voiceroom.view.seatmanager.SeatActionSheetDialog;
import com.trtc.uikit.livekit.voiceroom.view.seatmanager.SeatActionSheetGenerator;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridViewObserver;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import java.util.List;

public class SeatGridViewCoreObserver extends SeatGridViewObserver {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("SeatGridViewCoreObserver");

    private final Context                  mContext;
    private final VoiceRoomManager         mVoiceRoomManager;
    private final SeatActionSheetGenerator mSeatActionSheetGenerator;
    private       SeatActionSheetDialog    mSeatActionSheetDialog;

    public SeatGridViewCoreObserver(Context context, VoiceRoomManager voiceRoomManager, SeatGridView seatGridView) {
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        mSeatActionSheetGenerator = new SeatActionSheetGenerator(mContext, mVoiceRoomManager, seatGridView);
    }

    @Override
    public void onSeatViewClicked(@NonNull View seatView, TUIRoomDefine.SeatInfo seatInfo) {
        LOGGER.info(hashCode() + "onSeatViewClicked userId: " + seatInfo.userId);
        List<ListMenuInfo> listMenuInfoList = mSeatActionSheetGenerator.generate(seatInfo);
        if (listMenuInfoList.isEmpty()) {
            return;
        }
        if (mSeatActionSheetDialog == null) {
            mSeatActionSheetDialog = new SeatActionSheetDialog(mContext);
        }
        mSeatActionSheetDialog.updateActionButton(listMenuInfoList);
        mSeatActionSheetDialog.show();
    }

    @Override
    public void onKickedOffSeat(TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + "onKickedOffSeat: " + userInfo.userId);
        mVoiceRoomManager.getSeatManager().onKickedOffSeat(userInfo);
    }

    @Override
    public void onSeatRequestCancelled(@NonNull VoiceRoomDefine.RequestType type, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + "onSeatRequestCancelled type: " + type + ",userInfo:" + userInfo.userId);
        mVoiceRoomManager.getSeatManager().onSeatRequestCancelled(type, userInfo);
    }

    @Override
    public void onSeatRequestReceived(@NonNull VoiceRoomDefine.RequestType type, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + "onSeatRequestReceived type: " + type + ",userInfo:" + userInfo.userId);
        mVoiceRoomManager.getSeatManager().onSeatRequestReceived(type, userInfo);
    }

    @Override
    public void onKickedOutOfRoom(@NonNull String roomId, @NonNull TUIRoomDefine.KickedOutOfRoomReason reason,
                                  @NonNull String message) {
        LOGGER.info(hashCode() + "onKickedOutOfRoom roomId: " + roomId + ",reason:" + reason + ",message:" + message);
        mVoiceRoomManager.getRoomManager().onKickedOutOfRoom(roomId, reason, message);
    }

    @Override
    public void onRoomDismissed(@NonNull String roomId) {
        LOGGER.info(hashCode() + "onRoomDismissed roomId: " + roomId);
        mVoiceRoomManager.getRoomManager().onRoomDismissed(roomId);
        mSeatActionSheetGenerator.destroy();
        if (mSeatActionSheetDialog != null) {
            mSeatActionSheetDialog.dismiss();
        }
    }
}
