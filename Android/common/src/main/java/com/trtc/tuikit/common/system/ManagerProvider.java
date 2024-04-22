package com.trtc.tuikit.common.system;

import android.content.Context;
import android.view.inputmethod.InputMethodManager;

import com.tencent.qcloud.tuicore.TUILogin;

public class ManagerProvider {

    public static InputMethodManager getInputMethodManager() {
        Context context = ContextProvider.getApplicationContext();
        return (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
    }
}
