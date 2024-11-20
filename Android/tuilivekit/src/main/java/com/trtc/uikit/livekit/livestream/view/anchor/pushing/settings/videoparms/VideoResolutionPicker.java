package com.trtc.uikit.livekit.livestream.view.anchor.pushing.settings.videoparms;

import android.content.Context;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import androidx.core.content.ContextCompat;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;

import java.util.Arrays;
import java.util.List;

public class VideoResolutionPicker extends PopupDialog {

    private       ListView          mListView;
    private final LiveStreamManager mLiveStreamManager;

    public VideoResolutionPicker(Context context, LiveStreamManager liveStreamManager) {
        super(context);
        mLiveStreamManager = liveStreamManager;
        initView(context);
        initListItemClickListener();
    }

    private void initView(Context context) {
        mListView = new ListView(context);
        mListView.setBackgroundResource(R.drawable.livekit_dialog_background);

        List<String> dataList = getDataList(context);
        ArrayAdapter<String> adapter = new ArrayAdapter<>(context, R.layout.livekit_layout_bottom_list_item, dataList);
        mListView.setAdapter(adapter);
        mListView.setDivider(ContextCompat.getDrawable(context, R.drawable.livekit_line_divider));
        mListView.setDividerHeight(1);
        setView(mListView);
    }

    private void initListItemClickListener() {
        mListView.setOnItemClickListener((parent, view, position, id) -> {
            TUIRoomEngine.sharedInstance().updateVideoQuality(getResolutionByPosition(position));
            mLiveStreamManager.getMediaState().videoQuality.set(getResolutionByPosition(position));
            dismiss();
        });
    }

    private TUIRoomDefine.VideoQuality getResolutionByPosition(int position) {
        switch (position) {
            case 0:
                return TUIRoomDefine.VideoQuality.Q_360P;
            case 1:
                return TUIRoomDefine.VideoQuality.Q_540P;
            case 2:
                return TUIRoomDefine.VideoQuality.Q_720P;
            default:
                return TUIRoomDefine.VideoQuality.Q_1080P;

        }
    }

    private List<String> getDataList(Context context) {
        return Arrays.asList(context.getResources().getStringArray(R.array.livekit_video_resolution));
    }
}

