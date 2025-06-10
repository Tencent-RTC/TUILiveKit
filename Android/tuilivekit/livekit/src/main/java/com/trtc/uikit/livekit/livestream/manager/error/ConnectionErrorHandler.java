package com.trtc.uikit.livekit.livestream.manager.error;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;

public class ConnectionErrorHandler {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("ConnectionErrorHandler");

    public static void onError(TUILiveConnectionManager.ConnectionCode code) {
        if (code == TUILiveConnectionManager.ConnectionCode.SUCCESS || code == null) {
            return;
        }
        String message = convertToErrorMessage(code);
        LOGGER.info("ConnectionErrorHandler :[code:" + code + ",message:" + message + "]");
        showToast(message);
    }

    private static String convertToErrorMessage(TUILiveConnectionManager.ConnectionCode resultCode) {
        Context context = ContextProvider.getApplicationContext();
        switch (resultCode) {
            case CONNECTING:
            case CONNECTING_OTHER_ROOM:
                return context.getString(R.string.common_connect_conflict);
            case CONNECTION_FULL:
                return context.getString(R.string.common_connection_room_full);
            default:
                return context.getString(R.string.common_connect_error);
        }
    }

    private static void showToast(String tips) {
        Context context = ContextProvider.getApplicationContext();
        View view = LayoutInflater.from(context).inflate(R.layout.livekit_connection_toast, null, false);

        TextView text = view.findViewById(R.id.tv_toast_text);
        text.setText(tips);
        ImageView image = view.findViewById(R.id.iv_toast_image);
        image.setImageResource(R.drawable.livekit_connection_toast_icon);

        Toast toast = new Toast(view.getContext());
        toast.setDuration(Toast.LENGTH_SHORT);
        toast.setView(view);
        toast.show();
    }
}
