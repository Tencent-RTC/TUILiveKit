package com.trtc.uikit.livekit.component.floatwindow.service;

import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;
import android.view.View;
import android.view.ViewGroup;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.component.common.StateCache;
import com.trtc.uikit.livekit.LiveIdentityGenerator;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.core.FloatWindow;
import com.trtc.uikit.livekit.component.floatwindow.core.FloatWindowObserver;
import com.trtc.uikit.livekit.component.floatwindow.store.FloatWindowStore;
import com.trtc.uikit.livekit.component.floatwindow.view.VideoFloatView;
import com.trtc.uikit.livekit.component.floatwindow.view.VoiceFloatView;
import com.trtc.uikit.livekit.component.roomlist.view.ListAudienceActivity;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.view.anchor.VideoLiveAnchorActivity;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomDefine;
import com.trtc.uikit.livekit.voiceroom.view.VoiceRoomActivity;

public final class FloatWindowManager {

    private static final String TAG = "FloatWindowManager";

    private final FloatWindowStore mStore = new FloatWindowStore();

    private LiveStreamManager mLiveStreamManager;
    private LiveCoreView      mCoreView;

    private static final FloatWindowManager INSTANCE = new FloatWindowManager();

    public static FloatWindowManager getInstance() {
        return INSTANCE;
    }

    private FloatWindowManager() {
        Context context = ContextProvider.getApplicationContext();
        int w = context.getResources().getDimensionPixelSize(R.dimen.livekit_float_view_width);
        int h = context.getResources().getDimensionPixelSize(R.dimen.livekit_float_view_height);
        FloatWindow.getInstance().setSize(w, h);
        FloatWindow.getInstance().setObserver(new FloatWindowObserver() {
            @Override
            public void onClickWindow() {
                onClickFloatWindow();
            }
        });
    }

    private View createFloatView(Context context, LiveCoreView coreView) {
        LiveStreamManager liveStreamManager = getLiveStreamManager();
        if (liveStreamManager == null) {
            LiveStreamLog.error(TAG + " initWindow failed: liveStreamManager is null");
            return null;
        }

        String roomId = liveStreamManager.getRoomState().roomId;
        LiveIdentityGenerator mRoomIdStrategy = LiveIdentityGenerator.getInstance();
        LiveIdentityGenerator.RoomType roomType = mRoomIdStrategy.getIDType(roomId);
        if (roomType == LiveIdentityGenerator.RoomType.VOICE) {
            return new VoiceFloatView(context);
        } else {
            return new VideoFloatView(context, liveStreamManager, coreView);
        }
    }

    public void onClickFloatWindow() {
        dismissFloatWindow();
        resumeLive();
    }

    public void releaseFloatWindow() {
        LiveStreamLog.info(TAG + " releaseFloatWindow");
        LiveStreamManager liveStreamManager = mLiveStreamManager;
        if (liveStreamManager != null) {
            String roomId = liveStreamManager.getRoomState().roomId;
            StateCache.getInstance().remove(roomId);
            if (mCoreView != null) {
                String ownerId = liveStreamManager.getRoomState().ownerInfo.userId;
                String selfId = liveStreamManager.getUserState().selfInfo.userId;
                if (TextUtils.equals(ownerId, selfId)) {
                    mCoreView.stopLiveStream(null);
                } else {
                    mCoreView.leaveLiveStream(null);
                }
            }
            liveStreamManager.destroy();
            setLiveStreamManager(null);
            setCoreView(null);
        }
        dismissFloatWindow();
    }

    public FloatWindowStore getStore() {
        return mStore;
    }

    public boolean isWillOpenFloatWindow() {
        return mStore.isWillOpenFloatWindow;
    }

    public void setWillOpenFloatWindow(boolean open) {
        mStore.isWillOpenFloatWindow = open;
    }

    public boolean isShowingFloatWindow() {
        return FloatWindow.getInstance().isShowing();
    }

    public boolean hasPermission() {
        return FloatWindow.getInstance().hasPermission();
    }

    public void requestPermission() {
        FloatWindow.getInstance().requestPermission();
    }

    public void setLiveStreamManager(LiveStreamManager liveStreamManager) {
        mLiveStreamManager = liveStreamManager;
    }

    public LiveStreamManager getLiveStreamManager() {
        return mLiveStreamManager;
    }

    public void setCoreView(LiveCoreView coreView) {
        mCoreView = coreView;
    }

    public LiveCoreView getCoreView() {
        return mCoreView;
    }

    public void resumeLive() {
        LiveStreamManager liveStreamManager = mLiveStreamManager;
        LiveStreamLog.info(TAG + " resumeLive: liveStreamManager=" + liveStreamManager);
        if (liveStreamManager == null) {
            return;
        }
        LiveIdentityGenerator mRoomIdStrategy = LiveIdentityGenerator.getInstance();
        TUIRoomDefine.Role role = liveStreamManager.getUserState().selfInfo.role.get();
        String roomId = liveStreamManager.getRoomState().roomId;
        LiveIdentityGenerator.RoomType roomType = mRoomIdStrategy.getIDType(roomId);
        if (role == TUIRoomDefine.Role.GENERAL_USER) {
            joinRoom(roomId);
        } else if (role == TUIRoomDefine.Role.ROOM_OWNER) {
            if (roomType == LiveIdentityGenerator.RoomType.VOICE) {
                VoiceRoomDefine.CreateRoomParams params = new VoiceRoomDefine.CreateRoomParams();
                params.roomName = liveStreamManager.getRoomState().roomName.get();
                params.maxAnchorCount = liveStreamManager.getRoomState().maxSeatCount.get();
                startVoiceStream(roomId, params);
            } else {
                startLiveStream(roomId);
            }
        }
    }

    private void startLiveStream(String roomId) {
        LiveStreamLog.info(TAG + " startLiveStream:" + roomId);
        Context context = ContextProvider.getApplicationContext();
        Intent intent = new Intent(context, VideoLiveAnchorActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_ROOM_ID, roomId);
        context.startActivity(intent);
    }

    private void joinRoom(String roomId) {
        LiveStreamLog.info(TAG + " joinRoom:" + roomId);
        Context context = ContextProvider.getApplicationContext();
        Intent intent = new Intent(context, ListAudienceActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra("roomId", roomId);
        context.startActivity(intent);
    }

    private void startVoiceStream(String roomId, VoiceRoomDefine.CreateRoomParams params) {
        LiveStreamLog.info(TAG + " startVoiceStream:" + roomId + ", params=" + new Gson().toJson(params));
        Context context = ContextProvider.getApplicationContext();
        Intent intent = new Intent(context, VoiceRoomActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_ID, roomId);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_CREATE_ROOM_PARAMS, params);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_IS_ANCHOR, true);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_IS_RESUME, true);
        context.startActivity(intent);
    }

    public void showFloatWindow() {
        LiveStreamLog.info(TAG + " showFloatWindow");
        if (isShowingFloatWindow()) {
            return;
        }
        if (mLiveStreamManager == null || mCoreView == null) {
            return;
        }
        mLiveStreamManager.getLiveService().addRoomEngineObserver(mRoomObserver);
        ViewGroup viewGroup = (ViewGroup) mCoreView.getParent();
        if (viewGroup != null) {
            viewGroup.removeView(mCoreView);
        }
        Context context = ContextProvider.getApplicationContext();
        View view = createFloatView(context, mCoreView);
        if (view == null) {
            return;
        }
        FloatWindow.getInstance().setView(view);
        FloatWindow.getInstance().show();
        mStore.isShowingFloatWindow.set(true);
    }

    private void dismissFloatWindow() {
        LiveStreamLog.info(TAG + " dismissFloatWindow");
        LiveStreamManager liveStreamManager = getLiveStreamManager();
        if (liveStreamManager != null) {
            liveStreamManager.getLiveService().removeRoomEngineObserver(mRoomObserver);
        }
        if (mCoreView != null) {
            ViewGroup viewGroup = (ViewGroup) mCoreView.getParent();
            if (viewGroup != null) {
                viewGroup.removeView(mCoreView);
            }
        }
        FloatWindow.getInstance().close();
        mStore.isShowingFloatWindow.set(false);
    }

    private final TUIRoomObserver mRoomObserver = new TUIRoomObserver() {
        @Override
        public void onRoomDismissed(String roomId) {
            dismissFloatWindow();
        }

        @Override
        public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
            dismissFloatWindow();
        }

        @Override
        public void onKickedOffLine(String message) {
            dismissFloatWindow();
        }
    };
}
