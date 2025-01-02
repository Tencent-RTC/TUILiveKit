package com.trtc.uikit.livekit.voiceroom.manager.observer;

import android.content.Context;
import android.view.View;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.view.seatmanager.ListMenuInfo;
import com.trtc.uikit.livekit.voiceroom.view.seatmanager.SeatActionSheetDialog;
import com.trtc.uikit.livekit.voiceroom.view.seatmanager.SeatActionSheetGenerator;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridViewObserver;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import java.util.List;

public class SeatGridViewCoreObserver extends SeatGridViewObserver {
    private static final String FILE = "SeatGridViewCoreObserver";

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
    public void onSeatViewClicked(View seatView, TUIRoomDefine.SeatInfo seatInfo) {
        Logger.info(FILE, "onSeatViewClicked userId: " + seatInfo.userId);
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
    public void onUserAudioStateChanged(TUIRoomDefine.UserInfo userInfo, boolean hasAudio,
                                        TUIRoomDefine.ChangeReason reason) {
        Logger.info(FILE, "onUserAudioStateChanged: " + userInfo.userId + ",hasAudio:" + hasAudio);
        mVoiceRoomManager.getMediaManager().onUserAudioStateChanged(userInfo, hasAudio, reason);
    }

    @Override
    public void onKickedOffSeat(TUIRoomDefine.UserInfo userInfo) {
        Logger.info(FILE, "onKickedOffSeat: " + userInfo.userId);
        mVoiceRoomManager.getSeatManager().onKickedOffSeat(userInfo);
    }

    @Override
    public void onSeatRequestCancelled(VoiceRoomDefine.RequestType type, TUIRoomDefine.UserInfo userInfo) {
        Logger.info(FILE, "onSeatRequestCancelled type: " + type + ",userInfo:" + userInfo.userId);
        mVoiceRoomManager.getSeatManager().onSeatRequestCancelled(type, userInfo);
    }

    @Override
    public void onSeatRequestReceived(VoiceRoomDefine.RequestType type, TUIRoomDefine.UserInfo userInfo) {
        Logger.info(FILE, "onSeatRequestReceived type: " + type + ",userInfo:" + userInfo.userId);
        mVoiceRoomManager.getSeatManager().onSeatRequestReceived(type, userInfo);
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        Logger.info(FILE, "onKickedOutOfRoom roomId: " + roomId + ",reason:" + reason + ",message:" + message);
        mVoiceRoomManager.getRoomManager().onKickedOutOfRoom(roomId, reason, message);
    }

    @Override
    public void onRoomDismissed(String roomId) {
        Logger.info(FILE, "onRoomDismissed roomId: " + roomId);
        mVoiceRoomManager.getRoomManager().onRoomDismissed(roomId);
    }
}
