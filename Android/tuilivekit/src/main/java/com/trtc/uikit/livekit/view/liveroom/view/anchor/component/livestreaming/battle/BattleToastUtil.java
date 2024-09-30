package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.battle;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.trtc.uikit.livekit.R;

public final class BattleToastUtil {

    public static void showToast(Context context, String tips) {
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
