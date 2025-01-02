package com.trtc.uikit.livekit.livestream.view.anchor.preview.dialog;

import android.content.Context;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import androidx.core.content.res.ResourcesCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.RoomState;

import java.util.Arrays;
import java.util.List;


public class StreamPrivacyStatusPicker extends PopupDialog {

    private       ListView  mListView;
    private final RoomState mRoomState;

    public StreamPrivacyStatusPicker(Context context, LiveStreamManager manager) {
        super(context);
        mRoomState = manager.getRoomState();
        initView(context);
        initListItemClickListener();
    }

    private void initView(Context context) {
        mListView = new ListView(context);
        mListView.setBackgroundResource(R.drawable.livekit_dialog_background);

        List<String> dataList = getDataList(context);
        ArrayAdapter<String> adapter = new ArrayAdapter<>(context, R.layout.livekit_layout_bottom_list_item, dataList);
        mListView.setAdapter(adapter);
        mListView.setDivider(ResourcesCompat.getDrawable(context.getResources(), R.drawable.livekit_line_divider,
                null));
        mListView.setDividerHeight(1);
        setView(mListView);
    }

    private void initListItemClickListener() {
        mListView.setOnItemClickListener((parent, view, position, id) -> {
            mRoomState.liveMode.set(RoomState.LiveStreamPrivacyStatus.values()[position]);
            dismiss();
        });
    }

    private List<String> getDataList(Context context) {
        return Arrays.asList(context.getResources().getStringArray(R.array.livekit_stream_privacy_status));
    }
}
