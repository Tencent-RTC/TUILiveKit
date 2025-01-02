package com.trtc.uikit.livekit.livestream.view.anchor.preview.dialog;

import android.content.Context;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import androidx.core.content.res.ResourcesCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;

import java.util.List;


public class StreamCategoryPicker extends PopupDialog {
    private       ListView          mListView;
    private final LiveStreamManager mLiveManager;

    public StreamCategoryPicker(Context context, LiveStreamManager manager) {
        super(context);
        mLiveManager = manager;
        initView(context);
        initListItemClickListener();
    }

    private void initView(Context context) {
        mListView = new ListView(context);
        mListView.setBackgroundResource(R.drawable.livekit_dialog_background);

        List<String> dataList = mLiveManager.getRoomManager().getLiveCategoryList();
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
            mLiveManager.getRoomManager().setLiveCategory(category);
            dismiss();
        });
    }
}
