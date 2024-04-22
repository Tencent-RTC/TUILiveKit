package com.trtc.uikit.livekit.liveroom.view.anchor.component.common.videoparams;

import android.content.Context;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

import java.util.Arrays;
import java.util.List;

public class VideoResolutionPicker extends PopupDialog {

    private       ListView          mListView;
    private final LiveRoomInfo      mLiveRoomInfo;
    private final RoomEngineService mRoomEngineService;

    public VideoResolutionPicker(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        initView(context);
        initListItemClickListener();
    }

    private void initView(Context context) {
        mListView = new ListView(context);
        mListView.setBackgroundResource(R.drawable.livekit_dialog_background);

        List<String> dataList = getDataList(context);
        ArrayAdapter adapter = new ArrayAdapter<>(context, R.layout.livekit_layout_bottom_list_item, dataList);
        mListView.setAdapter(adapter);
        mListView.setDivider(context.getResources().getDrawable(R.drawable.livekit_line_divider));
        mListView.setDividerHeight(1);
        setView(mListView);
    }

    private void initListItemClickListener() {
        mListView.setOnItemClickListener((parent, view, position, id) -> {
            mRoomEngineService.updateVideoQuality(getResolutionByPosition(position));
            mLiveRoomInfo.anchorInfo.videoInfo.videoQuality.set(getResolutionByPosition(position));
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

