package com.trtc.uikit.livekit.view.voiceroom;

import android.content.Context;
import android.content.Intent;

import com.trtc.uikit.livekit.VoiceRoomDefine;
import com.trtc.uikit.livekit.VoiceRoomKit;
import com.trtc.uikit.livekit.view.liveroom.VideoLiveKitImpl;

public class VoiceRoomKitImpl implements VoiceRoomKit {
    private static volatile VoiceRoomKitImpl sInstance;
    private final           Context          mContext;

    private VoiceRoomKitImpl(Context context) {
        mContext = context.getApplicationContext();
    }

    public static synchronized VoiceRoomKitImpl createInstance(Context context) {
        if (null == sInstance) {
            synchronized (VideoLiveKitImpl.class) {
                if (null == sInstance) {
                    sInstance = new VoiceRoomKitImpl(context);
                }
            }
        }
        return sInstance;
    }

    @Override
    public void createRoom(String roomId, VoiceRoomDefine.CreateRoomParams params) {
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
