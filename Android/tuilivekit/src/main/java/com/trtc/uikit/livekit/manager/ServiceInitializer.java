package com.trtc.uikit.livekit.manager;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

public final class ServiceInitializer extends ContentProvider {

    private final ITUINotification mNotification = (key, subKey, param) -> {
        if (TUIConstants.TUILogin.EVENT_LOGIN_STATE_CHANGED.equals(key)
                && TUIConstants.TUILogin.EVENT_SUB_KEY_USER_LOGIN_SUCCESS.equals(subKey)
                && TUILogin.isUserLogined()) {
            TUIRoomEngine.login(TUIConfig.getAppContext(), TUILogin.getSdkAppId(), TUILogin.getUserId(),
                    TUILogin.getUserSig(), new TUIRoomDefine.ActionCallback() {
                        @Override
                        public void onSuccess() {
                            LiveKitLog.info("serviceInitializer login:[Success]");
                        }

                        @Override
                        public void onError(TUICommonDefine.Error error, String message) {
                            LiveKitLog.error("serviceInitializer login:[Error:" + error + ",message:" + message + "]");
                        }
                    });
        }
    };

    @Override
    public boolean onCreate() {
        TUICore.registerEvent(TUIConstants.TUILogin.EVENT_LOGIN_STATE_CHANGED,
                TUIConstants.TUILogin.EVENT_SUB_KEY_USER_LOGIN_SUCCESS, mNotification);
        if (getContext() != null) {
            ContextProvider.setApplicationContext(this.getContext().getApplicationContext());
        }
        return false;
    }

    @Nullable
    @Override
    public Cursor query(@NonNull Uri uri, @Nullable String[] projection, @Nullable String selection,
                        @Nullable String[] selectionArgs, @Nullable String sortOrder) {
        return null;
    }

    @Nullable
    @Override
    public String getType(@NonNull Uri uri) {
        return null;
    }

    @Nullable
    @Override
    public Uri insert(@NonNull Uri uri, @Nullable ContentValues values) {
        return null;
    }

    @Override
    public int delete(@NonNull Uri uri, @Nullable String selection, @Nullable String[] selectionArgs) {
        return 0;
    }

    @Override
    public int update(@NonNull Uri uri, @Nullable ContentValues values, @Nullable String selection,
                      @Nullable String[] selectionArgs) {
        return 0;
    }
}

