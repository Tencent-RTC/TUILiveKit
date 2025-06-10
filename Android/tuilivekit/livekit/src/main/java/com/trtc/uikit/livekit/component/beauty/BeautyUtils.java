package com.trtc.uikit.livekit.component.beauty;

import android.content.Context;
import android.os.Bundle;

import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;

public class BeautyUtils {
    public static void showBeautyDialog(Context context) {
        TEBeautyManager teBeautyManager = TEBeautyManager.getInstance();
        if (teBeautyManager.isSupportTEBeauty()) {
            teBeautyManager.checkBeautyResource(context, new TUIServiceCallback() {
                @Override
                public void onServiceCallback(int code, String message, Bundle bundle) {
                    if (code == 0) {
                        new BeautyPanelDialog(context).show();
                    } else {
                        ToastUtil.toastShortMessage("check beauty resource failed:" + code + ",message:" + message);
                    }
                }
            });
        } else {
            new BeautyPanelDialog(context).show();
        }
    }
}
