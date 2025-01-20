package com.tencent.cloud.tuikit.flutter.tuilivekit.tuilivekit_example;

import android.os.Bundle;
import android.util.Log;

import androidx.annotation.Nullable;

import com.tencent.effect.tencent_effect_flutter.XmagicProcesserFactory;
import com.tencent.trtcplugin.TRTCCloudPlugin;

import io.flutter.embedding.android.FlutterActivity;

public class MainActivity extends FlutterActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.i("BeautyPlugin", "MainActivity onCreate");
        TRTCCloudPlugin.register(new XmagicProcesserFactory());
    }
}
