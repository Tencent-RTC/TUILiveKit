package com.trtc.uikit.livekit.liveroom.core;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.system.ContextProvider;

import java.util.Map;

public final class ServiceInitializer extends ContentProvider {

    private final ITUINotification mNotification = new ITUINotification() {

        @Override
        public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
            if (TUIConstants.TUILogin.EVENT_LOGIN_STATE_CHANGED.equals(key)
                    && TUIConstants.TUILogin.EVENT_SUB_KEY_USER_LOGIN_SUCCESS.equals(subKey)
                    && TUILogin.isUserLogined()) {
                EngineManager.login(TUILogin.getSdkAppId(), TUILogin.getUserId(), TUILogin.getUserSig());
            }
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

