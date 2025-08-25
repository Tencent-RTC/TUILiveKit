package com.trtc.uikit.livekit.voiceroom.view;

import static com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior.AUTO_CREATE;
import static com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior.JOIN;
import static com.trtc.uikit.livekit.voiceroom.view.TUIVoiceRoomFragment.RoomBehavior.PREPARE_CREATE;

import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomDefine;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomKit;

import java.util.Objects;

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
        Intent intent = new Intent(mContext, VoiceRoomActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_ID, roomId);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_CREATE_ROOM_PARAMS, params);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_BEHAVIOR, PREPARE_CREATE.ordinal());
        mContext.startActivity(intent);
    }

    @Override
    public void enterRoom(String roomId) {
        Intent intent = new Intent(mContext, VoiceRoomActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_ID, roomId);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_BEHAVIOR, JOIN.ordinal());
        mContext.startActivity(intent);
    }

    @Override
    public void enterRoom(@NonNull TUILiveListManager.LiveInfo liveInfo) {
        Intent intent = new Intent(mContext, VoiceRoomActivity.class);
        boolean isAnchor = Objects.equals(liveInfo.ownerId, TUILogin.getUserId());
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_ID, liveInfo.roomId);
        if (isAnchor) {
            intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_BEHAVIOR, AUTO_CREATE.ordinal());
        } else {
            intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_BEHAVIOR, JOIN.ordinal());
        }
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        mContext.startActivity(intent);
    }
}
