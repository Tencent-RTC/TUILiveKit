package com.trtc.uikit.livekit.features.anchorboardcast.manager.api;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMFriendshipListener;
import com.tencent.imsdk.v2.V2TIMValueCallback;

import java.util.List;

public interface IAnchorAPI {
    void destroy();

    void addRoomEngineObserver(TUIRoomObserver observer);

    void removeRoomEngineObserver(TUIRoomObserver observer);

    void addLiveListManagerObserver(TUILiveListManager.Observer observer);

    void removeLiveListManagerObserver(TUILiveListManager.Observer observer);

    void addFriendListener(V2TIMFriendshipListener observer);

    void removeFriendListener(V2TIMFriendshipListener observer);

    void getLiveInfo(String roomId, TUILiveListManager.LiveInfoCallback callback);

    /****************************************** Seat Business *******************************************/

    void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams params, TUIRoomDefine.ActionCallback callback);

    /****************************************** User Business *******************************************/
    void getUserList(long nextSequence, TUIRoomDefine.GetUserListCallback callback);

    void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback);

    void disableSendingMessageByAdmin(String userId, boolean isDisable, TUIRoomDefine.ActionCallback callback);

    void kickRemoteUserOutOfRoom(String userId, TUIRoomDefine.ActionCallback callback);

    /****************************************** Media Business *******************************************/

    void setLocalVideoView(TUIVideoView view);

    /****************************************** IM Business *******************************************/
    void followUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback);

    void unfollowUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback);

    void checkFollowType(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>> callback);

    void fetchLiveList(String cursor, int count, TUILiveListManager.LiveInfoListCallback callback);

    void setLiveInfo(LiveInfo liveInfo, List<LiveModifyFlag> flagList, TUIRoomDefine.ActionCallback callback);
}
