package com.trtc.uikit.livekit.view.voiceroom.view.panel.seatactionsheet;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.core.content.ContextCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.view.voiceroom.model.ListMenuInfo;

import java.util.List;

public class SeatActionSheetPanel extends PopupDialog {
    private final Context mContext;
    private final ViewGroup mViewContainer;

    public SeatActionSheetPanel(Context context) {
        super(context);
        mContext = context;
        View rootView = View.inflate(context, R.layout.livekit_voiceroom_seat_action_sheet_panel, null);
        rootView.setBackground(ContextCompat.getDrawable(context, R.drawable.livekit_dialog_background_light));
        mViewContainer = rootView.findViewById(R.id.view_container);
        rootView.findViewById(R.id.text_cancel).setOnClickListener(view -> dismiss());
        setView(rootView);
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
