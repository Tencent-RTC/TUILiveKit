package com.tencent.tcmediax.tceffectplayerkit.core;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;
import android.util.Log;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.tcmediax.tceffectplayerkit.Constants;

public final class TUIEffectPlayerProvider extends ContentProvider {
    private static final String TAG = "TUIEffectPlayerProvider";

    @Override
    public boolean onCreate() {
        Log.d(TAG, "TUIEffectPlayerProvider onCreate");
        TUICore.registerExtension(Constants.KEY_EXTENSION_NAME, new TUIEffectPlayerExtension());
        TUICore.registerService(Constants.KEY_SERVICE_NAME, new TUIEffectPlayerService());
        return false;
    }

    @Override
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        return null;
    }


    @Override
    public String getType(Uri uri) {
        return null;
    }


    @Override
    public Uri insert(Uri uri, ContentValues values) {
        return null;
    }

    @Override
    public int delete(Uri uri, String selection, String[] selectionArgs) {
        return 0;
    }

    @Override
    public int update(Uri uri, ContentValues values, String selection, String[] selectionArgs) {
        return 0;
    }
}
