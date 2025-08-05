package com.trtc.uikit.livekit.ktv.impl;

import static android.content.Context.APP_OPS_SERVICE;

import android.app.Activity;
import android.app.AppOpsManager;
import android.app.PictureInPictureParams;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.media.AudioManager;
import android.net.Uri;
import android.os.Build;
import android.telephony.PhoneStateListener;
import android.telephony.TelephonyManager;
import android.text.TextUtils;
import android.util.Rational;

import androidx.annotation.RequiresApi;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.uikit.livekit.KTVAnchorActivity;
import com.tencent.cloud.uikit.livekit.KTVAudienceActivity;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.ktv.KTVRoomKit;
import com.trtc.uikit.livekit.livestream.VideoLiveAnchorActivity;
import com.trtc.uikit.livekit.voiceroom.view.VoiceRoomActivity;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

public class KTVRoomKitImpl implements KTVRoomKit {
    private final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorPrepareView");
    public static final  String INTENT_KEY_NEED_CREATE   = "intent_key_need_create";
    public static final  String INTENT_KEY_ROOM_ID       = "intent_key_room_id";
    private static volatile KTVRoomKitImpl sInstance;
    private final           Context                mContext;
    private                 AudioModeListener      audioModeListener;
    private                 PhoneCallStateListener phoneListener;
    private                 AudioManager           audioManager;
    private                 TelephonyManager       telephonyManager;
    private                 boolean                audioModeListenerRegistered = false;

    private final List<WeakReference<CallingAPIListener>> mCallingAPIListenerList = new CopyOnWriteArrayList<>();

    private KTVRoomKitImpl(Context context) {
        mContext = context.getApplicationContext();
        initAudioModeListener(mContext);
    }

    private void initAudioModeListener(Context context) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            audioModeListener = new AudioModeListener();
            audioManager = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
            phoneListener = null;
            telephonyManager = null;
        } else {
            audioModeListener = null;
            audioManager = null;
            phoneListener = new PhoneCallStateListener();
            telephonyManager = (TelephonyManager) context.getSystemService(Service.TELEPHONY_SERVICE);
        }
    }

    public static synchronized KTVRoomKitImpl createInstance(Context context) {
        if (null == sInstance) {
            synchronized (KTVRoomKitImpl.class) {
                if (null == sInstance) {
                    sInstance = new KTVRoomKitImpl(context);
                }
            }
        }
        return sInstance;
    }

    @Override
    public void startLive(String roomId) {
        Intent intent = new Intent(mContext, KTVAnchorActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_ROOM_ID, roomId);
        mContext.startActivity(intent);
    }

    @Override
    public void joinLive(String roomId) {
        Intent intent = new Intent(mContext, KTVAudienceActivity.class);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_ID, roomId);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        mContext.startActivity(intent);
    }

    @Override
    public void joinLive(LiveInfo liveInfo) {
        if (liveInfo == null || TextUtils.isEmpty(liveInfo.roomId)) {
            return;
        }
        Intent intent;
        if (Objects.equals(liveInfo.ownerId, TUILogin.getUserId())) {
            intent = new Intent(mContext, KTVAnchorActivity.class);
            intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_NEED_CREATE, false);
        } else {
            intent = new Intent(mContext, KTVAudienceActivity.class);
        }
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_ROOM_ID, liveInfo.roomId);
        mContext.startActivity(intent);
    }

    @Override
    public void leaveLive(TUIRoomDefine.ActionCallback callback) {
        TUIRoomEngine.sharedInstance().exitRoom(true, callback);
        notifyLeaveLive();
    }

    @Override
    public void stopLive(TUIRoomDefine.ActionCallback callback) {
        TUIRoomEngine.sharedInstance().destroyRoom(callback);
        notifyStopLive();
    }

    public boolean enterPictureInPictureMode(Activity activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            AppOpsManager appOpsManager = (AppOpsManager) activity.getSystemService(APP_OPS_SERVICE);
            if (AppOpsManager.MODE_ALLOWED == appOpsManager.checkOpNoThrow(AppOpsManager.OPSTR_PICTURE_IN_PICTURE,
                    activity.getApplicationInfo().uid, activity.getPackageName())) {
                Rational aspectRatio = new Rational(9, 16);
                PictureInPictureParams params =
                        new PictureInPictureParams.Builder().setAspectRatio(aspectRatio).build();
                activity.enterPictureInPictureMode(params);
                return true;
            } else {
                Intent intent = new Intent("android.settings.PICTURE_IN_PICTURE_SETTINGS",
                        Uri.parse("package:" + activity.getPackageName()));
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                activity.startActivity(intent);
                LOGGER.warn("Picture in Picture not permission");
            }
        } else {
            ToastUtil.toastShortMessage(activity.getString(R.string.common_picture_in_picture_android_system_tips));
            LOGGER.warn("Picture-in-picture mode is not supported under android 8.0 lower version");
        }
        return false;
    }

    public void addCallingAPIListener(CallingAPIListener listener) {
        if (listener != null) {
            mCallingAPIListenerList.add(new WeakReference<>(listener));
        }
    }

    public void removeCallingAPIListener(CallingAPIListener listener) {
        for (WeakReference<CallingAPIListener> listenerWeakReference : mCallingAPIListenerList) {
            if (listenerWeakReference.get() == listener) {
                mCallingAPIListenerList.remove(listenerWeakReference);
            }
        }
    }

    public void startPushLocalVideoOnResume() {
        boolean isInCall = isInCall();
        if (isInCall) {
            stopPushLocalVideo();
        } else {
            startPushLocalVideo();
        }
        startListeningPhoneState();
    }

    public void stopPushLocalVideoOnStop() {
        stopPushLocalVideo();
        stopListeningPhoneState();
    }

    private void startListeningPhoneState() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (audioModeListener != null && !audioModeListenerRegistered) {
                audioManager.addOnModeChangedListener(mContext.getMainExecutor(), audioModeListener);
                audioModeListenerRegistered = true;
            }
        } else if (telephonyManager != null && phoneListener != null) {
            telephonyManager.listen(phoneListener, PhoneStateListener.LISTEN_CALL_STATE);
        }
    }

    private void stopListeningPhoneState() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (audioModeListener != null && audioModeListenerRegistered) {
                audioManager.removeOnModeChangedListener(audioModeListener);
                audioModeListenerRegistered = false;
            }
        } else if (telephonyManager != null && phoneListener != null) {
            telephonyManager.listen(phoneListener, PhoneStateListener.LISTEN_NONE);
        }
    }

    private void startPushLocalVideo() {
        TUIRoomEngine.sharedInstance().startPushLocalVideo();
    }

    public void stopPushLocalVideo() {
        TUIRoomEngine.sharedInstance().stopPushLocalVideo();
    }

    private boolean isInCall() {
        if (audioManager == null) {
            int state = telephonyManager.getCallState();
            return state == TelephonyManager.CALL_STATE_OFFHOOK;
        } else {
            int mode = audioManager.getMode();
            return mode == AudioManager.MODE_IN_CALL;
        }
    }

    @RequiresApi(api = Build.VERSION_CODES.S)
    private final class AudioModeListener implements AudioManager.OnModeChangedListener {
        @Override
        public void onModeChanged(int mode) {
            LOGGER.info("onModeChanged, mode:" + mode);
            switch (mode) {
                case AudioManager.MODE_IN_CALL:
                    stopPushLocalVideo();
                    break;
                case AudioManager.MODE_NORMAL:
                default:
                    startPushLocalVideo();
                    break;
            }
        }
    }


    private final class PhoneCallStateListener extends PhoneStateListener {
        @Override
        public void onCallStateChanged(int state, String incomingNumber) {
            super.onCallStateChanged(state, incomingNumber);
            LOGGER.info("onCallStateChanged, state:" + state + ", incomingNumber:" + incomingNumber);
            switch (state) {
                case TelephonyManager.CALL_STATE_OFFHOOK:
                    stopPushLocalVideo();
                    break;
                case TelephonyManager.CALL_STATE_IDLE:
                default:
                    startPushLocalVideo();
                    break;
            }
        }
    }


    public interface CallingAPIListener {
        void onLeaveLive();

        void onStopLive();

    }

    private void notifyLeaveLive() {
        for (WeakReference<CallingAPIListener> listenerWeakReference : mCallingAPIListenerList) {
            CallingAPIListener callingAPIListener = listenerWeakReference.get();
            if (callingAPIListener != null) {
                callingAPIListener.onLeaveLive();
            }
        }
    }

    private void notifyStopLive() {
        for (WeakReference<CallingAPIListener> listenerWeakReference : mCallingAPIListenerList) {
            CallingAPIListener callingAPIListener = listenerWeakReference.get();
            if (callingAPIListener != null) {
                callingAPIListener.onStopLive();
            }
        }
    }
}
