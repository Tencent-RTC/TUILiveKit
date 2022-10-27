package com.tencent.qcloud.tuikit.tuiaudioeffect.core;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuikit.tuiaudioeffect.util.AudioEffectUtils;


/**
 * TUIAudioEffect组件初始化并注册到TUICore。
 * 在 Manifest 文件中以 ContentProvider 的形式注册，App启动时会自动执行。
 */
public final class TUIAudioEffectProvider extends ContentProvider {

    @Override
    public boolean onCreate() {
        // 将TUIAudioEffect组件注册到TUICore
        TUICore.registerExtension(AudioEffectUtils.EXTENSION_AUDIOEFFECT, new TUIAudioEffectExtension());
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
