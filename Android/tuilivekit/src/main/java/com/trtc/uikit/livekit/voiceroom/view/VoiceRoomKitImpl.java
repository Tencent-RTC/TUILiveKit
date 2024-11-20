package com.trtc.uikit.livekit.voiceroom.view;

import android.content.Context;
import android.content.Intent;

import com.trtc.uikit.livekit.voiceroom.VoiceRoomDefine;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomKit;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;

public class VoiceRoomKitImpl implements VoiceRoomKit {
    private static volatile VoiceRoomKitImpl sInstance;
    private final           Context          mContext;

    private VoiceRoomKitImpl(Context context) {
        mContext = context.getApplicationContext();
    }

    public static synchronized VoiceRoomKitImpl createInstance(Context context) {
        if (null == sInstance) {
            synchronized (VoiceRoomKitImpl.class) {
                if (null == sInstance) {
                    sInstance = new VoiceRoomKitImpl(context);
                }
            }
        }
        return sInstance;
    }

    @Override
    public void createRoom(String roomId, VoiceRoomDefine.CreateRoomParams params) {
        FloatWindowManager.getInstance().releaseFloatWindow();
        Intent intent = new Intent(mContext, VoiceRoomActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_ID, roomId);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_CREATE_ROOM_PARAMS, params);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_IS_ANCHOR, true);
        mContext.startActivity(intent);
    }

    @Override
    public void enterRoom(String roomId) {
        Intent intent = new Intent(mContext, VoiceRoomActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_ID, roomId);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_IS_ANCHOR, false);
        mContext.startActivity(intent);
    }
}
