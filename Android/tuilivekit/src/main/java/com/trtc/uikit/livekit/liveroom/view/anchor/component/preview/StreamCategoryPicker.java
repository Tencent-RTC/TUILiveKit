package com.trtc.uikit.livekit.liveroom.view.anchor.component.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

import java.util.Arrays;
import java.util.List;


public class StreamCategoryPicker extends PopupDialog {

    private       ListView     mListView;
    private final LiveRoomInfo mLiveRoomInfo;

    public StreamCategoryPicker(Context context, LiveRoomInfo roomInfo) {
        super(context);
        mLiveRoomInfo = roomInfo;
        initView(context);
        initListItemClickListener();
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    private void initView(Context context) {
        mListView = new ListView(context);
        mListView.setBackgroundResource(R.drawable.livekit_dialog_background);

        List<String> dataList = getDataList(context);
        ArrayAdapter<String> adapter = new ArrayAdapter<>(context, R.layout.livekit_layout_bottom_list_item, dataList);
        mListView.setAdapter(adapter);
        mListView.setDivider(context.getResources().getDrawable(R.drawable.livekit_line_divider));
        mListView.setDividerHeight(1);
        setView(mListView);

    }

    private void initListItemClickListener() {
        mListView.setOnItemClickListener((parent, view, position, id) -> {
            String category = (String) parent.getItemAtPosition(position);
            mLiveRoomInfo.category.set(category);
            dismiss();
        });
    }

    private List<String> getDataList(Context context) {
        return Arrays.asList(context.getResources().getStringArray(R.array.livekit_stream_category_list));
    }
}
