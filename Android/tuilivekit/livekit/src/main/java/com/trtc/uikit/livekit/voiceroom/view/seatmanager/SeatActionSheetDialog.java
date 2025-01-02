package com.trtc.uikit.livekit.voiceroom.view.seatmanager;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.core.content.ContextCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

import java.util.List;

public class SeatActionSheetDialog extends PopupDialog {
    private final Context mContext;
    private final ViewGroup mViewContainer;

    public SeatActionSheetDialog(Context context) {
        super(context);
        mContext = context;
        View rootView = View.inflate(context, R.layout.livekit_voiceroom_seat_action_sheet_panel, null);
        rootView.setBackground(ContextCompat.getDrawable(context, R.drawable.livekit_dialog_background_light));
        setView(rootView);
        mViewContainer = rootView.findViewById(R.id.view_container);
        rootView.findViewById(R.id.text_cancel).setOnClickListener(view -> dismiss());
    }

    public void updateActionButton(List<ListMenuInfo> menuInfoList) {
        mViewContainer.removeAllViews();
        for (ListMenuInfo menuInfo : menuInfoList) {
            View itemView = LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_item_seat_action_sheet,
                    mViewContainer, false);
            TextView textAction = itemView.findViewById(R.id.text_action);
            textAction.setText(menuInfo.text);
            textAction.setOnClickListener(view -> {
                if (menuInfo.listener != null) {
                    menuInfo.listener.onClick();
                }
                dismiss();
            });
            mViewContainer.addView(itemView);
        }
    }
}
