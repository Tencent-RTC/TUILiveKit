package com.trtc.uikit.livekit.voiceroom.view.preview;

import android.content.Context;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import androidx.core.content.res.ResourcesCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;

import java.util.List;


public class StreamCategoryPicker extends PopupDialog {
    private       ListView         mListView;
    private final VoiceRoomManager mVoiceRoomManager;

    public StreamCategoryPicker(Context context, VoiceRoomManager liveController) {
        super(context);
        mVoiceRoomManager = liveController;
        initView(context);
        initListItemClickListener();
    }

    private void initView(Context context) {
        mListView = new ListView(context);
        mListView.setBackgroundResource(R.drawable.livekit_dialog_background);

        List<String> dataList = mVoiceRoomManager.getRoomManager().getLiveCategoryList();
        ArrayAdapter<String> adapter = new ArrayAdapter<>(context, R.layout.livekit_layout_bottom_list_item, dataList);
        mListView.setAdapter(adapter);
        mListView.setDivider(ResourcesCompat.getDrawable(context.getResources(), R.drawable.livekit_line_divider,
                null));
        mListView.setDividerHeight(1);
        setView(mListView);
    }

    private void initListItemClickListener() {
        mListView.setOnItemClickListener((parent, view, position, id) -> {
            String category = (String) parent.getItemAtPosition(position);
            mVoiceRoomManager.getRoomManager().setLiveCategory(category);
            dismiss();
        });
    }
}
