package com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit;

import android.content.Context;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import androidx.core.content.res.ResourcesCompat;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine;
import com.trtc.uikit.livekit.features.anchorprepare.manager.AnchorPrepareManager;

import java.util.Arrays;
import java.util.List;

public class LivePrivacyStatusPicker extends PopupDialog {

    private       ListView             mListView;
    private final AnchorPrepareManager mManager;

    public LivePrivacyStatusPicker(Context context, AnchorPrepareManager manager) {
        super(context);
        mManager = manager;
        initView(context);
        initListItemClickListener();
    }

    private void initView(Context context) {
        mListView = new ListView(context);
        mListView.setBackgroundResource(R.drawable.anchor_prepare_dialog_background);

        List<String> dataList = getDataList(context);
        ArrayAdapter<String> adapter = new ArrayAdapter<>(context,
                R.layout.anchor_prepare_layout_stream_privacy_status_pick_item, dataList);
        mListView.setAdapter(adapter);
        mListView.setDivider(ResourcesCompat.getDrawable(context.getResources(), R.drawable.anchor_prepare_line_divider,
                null));
        mListView.setDividerHeight(1);
        setView(mListView);
    }

    private void initListItemClickListener() {
        mListView.setOnItemClickListener((parent, view, position, id) -> {
            mManager.setLiveMode(AnchorPrepareViewDefine.LiveStreamPrivacyStatus.values()[position]);
            dismiss();
        });
    }

    private List<String> getDataList(Context context) {
        return Arrays.asList(context.getResources().getStringArray(R.array.common_stream_privacy_status));
    }
}
