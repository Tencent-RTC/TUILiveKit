package com.trtc.uikit.livekit.voiceroom.view.panel.seatactionsheet;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.core.content.ContextCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.model.ListMenuInfo;

import java.util.List;

public class SeatActionSheetPanel extends PopupDialog {
    private final Context mContext;

    public SeatActionSheetPanel(Context context, List<ListMenuInfo> menuInfoList) {
        super(context);
        mContext = context;
        View rootView = View.inflate(context, R.layout.livekit_voiceroom_seat_action_sheet_panel, null);
        rootView.setBackground(ContextCompat.getDrawable(context, R.drawable.livekit_dialog_background));
        ViewGroup viewContainer = rootView.findViewById(R.id.view_container);
        setActionButton(viewContainer, menuInfoList);
        rootView.findViewById(R.id.text_cancel).setOnClickListener(view -> dismiss());
        setView(rootView);
    }

    private void setActionButton(ViewGroup viewContainer, List<ListMenuInfo> menuInfoList) {
        viewContainer.removeAllViews();
        for (ListMenuInfo menuInfo : menuInfoList) {
            View itemView = LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_item_seat_action_sheet,
                    viewContainer, false);
            TextView textAction = itemView.findViewById(R.id.text_action);
            textAction.setText(menuInfo.text);
            textAction.setOnClickListener(view -> {
                if (menuInfo.listener != null) {
                    menuInfo.listener.onClick();
                }
                dismiss();
            });
            viewContainer.addView(itemView);
        }
    }
}
