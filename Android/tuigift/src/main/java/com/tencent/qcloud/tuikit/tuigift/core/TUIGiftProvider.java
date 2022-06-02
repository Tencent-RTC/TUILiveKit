package com.tencent.qcloud.tuikit.tuigift.core;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;
import android.util.Log;

import com.tencent.qcloud.tuicore.TUICore;

/**
 * 礼物组件通过ContentProvider的方式注册到TUICore中(TUICore是各组件连接及通信的核心类);
 * 注册后,TUICore通过TUIGiftExtension,可以获取礼物组件的布局,使用发送和接收礼物功能。
 */
public final class TUIGiftProvider extends ContentProvider {
    private static final String TAG = "TUIGiftProvider";

    @Override
    public boolean onCreate() {
        Log.d(TAG, "TUIGiftProvider onCreate");
        TUICore.registerExtension(TUIGiftExtension.OBJECT_TUI_GIFT, new TUIGiftExtension());
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
