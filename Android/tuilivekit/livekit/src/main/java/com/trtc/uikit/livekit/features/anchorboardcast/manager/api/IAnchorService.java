package com.trtc.uikit.livekit.features.anchorboardcast.manager.api;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.observer.AnchorRoomObserver;

public interface IAnchorService {

    void getSeatList(TUIRoomDefine.GetSeatListCallback callback);

    void addObserver(AnchorRoomObserver mRoomObserver);

    void removeObserver(AnchorRoomObserver mRoomObserver);

    void fetchLiveList(String recommendedCursor, int fetchListCount, TUILiveListManager.LiveInfoListCallback liveInfoListCallback);
}
