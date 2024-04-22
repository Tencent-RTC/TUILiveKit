package com.trtc.uikit.livekit.voiceroom.view.preview;

import android.content.Context;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import androidx.core.content.res.ResourcesCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;

import java.util.Arrays;
import java.util.List;


public class StreamCategoryPicker extends PopupDialog {
    private       ListView  mListView;
    private final RoomState mRoomState;

    public StreamCategoryPicker(Context context, LiveController liveController) {
        super(context);
        mRoomState = liveController.getRoomSate();
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
            String category = (String) parent.getItemAtPosition(position);
            mRoomState.category.set(category);
            dismiss();
        });
    }

    private List<String> getDataList(Context context) {
        return Arrays.asList(context.getResources().getStringArray(R.array.livekit_stream_category_list));
    }
}
