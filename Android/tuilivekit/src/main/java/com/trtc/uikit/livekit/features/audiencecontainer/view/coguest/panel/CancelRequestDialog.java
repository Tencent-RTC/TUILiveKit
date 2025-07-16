package com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.panel;

import static com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState.CoGuestStatus.NONE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class CancelRequestDialog extends PopupDialog {

    private final LiveCoreView    mLiveStream;
    private final AudienceManager mLiveManager;

    public CancelRequestDialog(@NonNull Context context, LiveCoreView liveStream,
                               AudienceManager manager) {
        super(context);
        mLiveStream = liveStream;
        mLiveManager = manager;
        initView();
    }

    private void initView() {
        @SuppressLint("InflateParams")
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_dialog_co_guest_cancel, null);
        TextView textCancelCoGuest = view.findViewById(R.id.tv_cancel_co_guest);
        TextView textDismiss = view.findViewById(R.id.tv_dismiss);
        textCancelCoGuest.setOnClickListener(v -> {
            if (!v.isEnabled()) {
                return;
            }
            v.setEnabled(false);
            mLiveStream.cancelIntraRoomConnection("", new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mLiveManager.getCoGuestManager().updateCoGuestStates(NONE);
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorLocalized.onError(error);
                }
            });
            dismiss();
        });

        textDismiss.setOnClickListener(v -> {
            dismiss();
        });

        setView(view);
    }
}
