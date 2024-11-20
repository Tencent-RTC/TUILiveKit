package com.trtc.uikit.livekit.voiceroom.view.settings;

import android.content.Context;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;

public class SettingsDialog extends PopupDialog {
    private final Context          mContext;
    private final VoiceRoomManager mVoiceRoomManager;

    public SettingsDialog(@NonNull Context context, VoiceRoomManager voiceRoomManager) {
        super(context);
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        initView();
    }

    private void initView() {
        View rootView = View.inflate(mContext, R.layout.livekit_voiceroom_settings_panel, null);
        RecyclerView recycleSettingsList = rootView.findViewById(R.id.rv_settings_list);
        recycleSettingsList.setLayoutManager(new GridLayoutManager(mContext, SettingsListAdapter.ITEM_COUNT));
        SettingsListAdapter adapter = new SettingsListAdapter(mContext, mVoiceRoomManager);
        recycleSettingsList.addItemDecoration(new SettingsListAdapter.SpaceItemDecoration(mContext));
        recycleSettingsList.setAdapter(adapter);
        setView(rootView);
    }
}
