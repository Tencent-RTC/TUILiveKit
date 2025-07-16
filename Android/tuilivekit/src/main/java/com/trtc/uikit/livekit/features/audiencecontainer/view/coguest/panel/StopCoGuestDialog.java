package com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel;

import static com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState.CoGuestStatus.NONE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class StopCoGuestDialog extends PopupDialog {

    private final LiveCoreView    mLiveStream;
    private final AudienceManager mLiveManager;

    public StopCoGuestDialog(@NonNull Context context, LiveCoreView liveStream,
                             AudienceManager manager) {
        super(context);
        mLiveStream = liveStream;
        mLiveManager = manager;
        initView();
    }

    private void initView() {
        @SuppressLint("InflateParams")
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_dialog_co_guest_stop, null);
        TextView textStopCoGuest = view.findViewById(R.id.tv_stop_co_guest);
        TextView textDismiss = view.findViewById(R.id.tv_dismiss);
        textStopCoGuest.setOnClickListener(v -> {
            mLiveStream.terminateIntraRoomConnection();
            mLiveManager.getCoGuestManager().updateCoGuestStates(NONE);
            dismiss();
        });

        textDismiss.setOnClickListener(v -> {
            dismiss();
        });

        setView(view);
    }
}
