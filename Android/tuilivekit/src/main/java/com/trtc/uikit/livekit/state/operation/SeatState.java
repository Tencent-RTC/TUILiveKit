package com.trtc.uikit.livekit.state.operation;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.tuikit.common.livedata.LiveMapData;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

public class SeatState {
    private boolean                                  filterEmptySeat        = false;
    public  LiveData<List<SeatInfo>>                 seatList               =
            new LiveData<>(new CopyOnWriteArrayList<>());
    public  LiveData<LinkedHashSet<SeatApplication>> seatApplicationList    = new LiveData<>(new LinkedHashSet<>());
    public  LiveData<String>                         mySeatApplicationId    = new LiveData<>("");
    public  LiveMapData<String, SeatInvitation>      sentSeatInvitationMap  = new LiveMapData<>(new HashMap<>());
    public  LiveData<SeatInvitation>                 receivedSeatInvitation = new LiveData<>(new SeatInvitation());

    public void reset() {
        seatList.get().clear();
        seatApplicationList.get().clear();
        mySeatApplicationId.set("");
        sentSeatInvitationMap.get().clear();
        receivedSeatInvitation.get().reset();
    }

    public void setFilterEmptySeat(boolean filterEmptySeat) {
        this.filterEmptySeat = filterEmptySeat;
    }

    @NonNull
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("{");
        builder.append("filterEmptySeat:").append(filterEmptySeat).append(",");
        for (SeatInfo seatInfo : seatList.get()) {
            builder.append("[");
            builder.append(seatInfo.index);
            builder.append(",");
            builder.append(seatInfo.userId.get());
            builder.append(",");
            builder.append(seatInfo.isLocked.get() ? "1" : "0");
            builder.append(",");
            builder.append(seatInfo.isAudioLocked.get() ? "1" : "0");
            builder.append("]");
        }
        builder.append("}");
        return builder.toString();
    }

    public void initSeatList(int maxSeatCount) {
        if (filterEmptySeat) {
            return;
        }
        List<SeatInfo> list = new ArrayList<>(maxSeatCount);
        for (int i = 0; i < maxSeatCount; i++) {
            SeatInfo seatInfo = new SeatInfo();
            seatInfo.index = i;
            list.add(seatInfo);
        }
        this.seatList.get().clear();
        this.seatList.addAll(list);
    }

    private void initSeatList(List<TUIRoomDefine.SeatInfo> list) {
        List<SeatInfo> newList = new ArrayList<>(list.size());
        for (TUIRoomDefine.SeatInfo info : list) {
            if (filterEmptySeat && TextUtils.isEmpty(info.userId)) {
                continue;
            }
            SeatInfo seatInfo = new SeatInfo();
            seatInfo.updateState(info);
            newList.add(seatInfo);
        }
        this.seatList.get().clear();
        this.seatList.addAll(newList);
    }

    public void updateSeatList(List<TUIRoomDefine.SeatInfo> list) {
        if (filterEmptySeat) {
            initSeatList(list);
            return;
        }
        if (list.size() != this.seatList.get().size()) {
            initSeatList(list);
            return;
        }
        for (int i = 0; i < list.size(); i++) {
            SeatInfo oldSeatInfo = this.seatList.get().get(i);
            TUIRoomDefine.SeatInfo newSeatInfo = list.get(i);
            if (oldSeatInfo == null || newSeatInfo == null) {
                continue;
            }
            oldSeatInfo.updateState(newSeatInfo);
        }
    }

    public void initSeatApplicationList(List<TUIRoomDefine.Request> list) {
        seatApplicationList.get().clear();
        List<SeatApplication> newList = new ArrayList<>();
        for (TUIRoomDefine.Request request : list) {
            SeatApplication application = new SeatApplication(request.requestId);
            application.updateState(request);
            newList.add(application);
        }
        seatApplicationList.addAll(newList);
    }

    public void addSeatApplication(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        SeatApplication seatApplication = new SeatApplication(request.requestId);
        seatApplication.updateState(request);
        seatApplicationList.add(seatApplication);
    }

    public void removeSeatApplication(String id) {
        SeatApplication application = new SeatApplication(id);
        seatApplicationList.remove(application);
    }

    public void addSendSeatInvitation(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        SeatInvitation seatInvitation = new SeatInvitation();
        seatInvitation.updateState(request);
        sentSeatInvitationMap.put(request.userId, seatInvitation);
    }

    public void removeSentSeatInvitation(String userId) {
        sentSeatInvitationMap.remove(userId);
    }

    public void addReceivedSeatInvitation(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        SeatInvitation seatInvitation = new SeatInvitation();
        seatInvitation.updateState(request);
        receivedSeatInvitation.set(seatInvitation);
    }

    public void removeReceivedSeatInvitation() {
        SeatInvitation seatInvitation = receivedSeatInvitation.get();
        if (!TextUtils.isEmpty(seatInvitation.id)) {
            receivedSeatInvitation.get().reset();
            receivedSeatInvitation.notifyDataChanged();
        }
    }

    private boolean isRequestInvalid(TUIRoomDefine.Request request) {
        if (request == null) {
            return true;
        }
        if (TextUtils.isEmpty(request.userId)) {
            return true;
        }
        return TextUtils.isEmpty(request.requestId);
    }

    public static class SeatInfo {
        public int               index         = 0;
        public LiveData<String>  userId        = new LiveData<>("");
        public LiveData<String>  name          = new LiveData<>("");
        public LiveData<String>  avatarUrl     = new LiveData<>("");
        public LiveData<Boolean> isLocked      = new LiveData<>(false);
        public LiveData<Boolean> isAudioLocked = new LiveData<>(false);
        public LiveData<Boolean> isVideoLocked = new LiveData<>(false);

        public SeatInfo() {
        }

        public SeatInfo(String userId) {
            this.userId.set(userId);
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatInfo{"
                    + "index=" + index
                    + ", userId=" + userId.get()
                    + ", isLocked=" + (isLocked.get() ? "1" : "0")
                    + ", isAudioLocked=" + (isAudioLocked.get() ? "1" : "0")
                    + ", isVideoLocked=" + (isVideoLocked.get() ? "1" : "0")
                    + '}';
        }

        @Override
        public boolean equals(@Nullable Object obj) {
            if (obj instanceof SeatInfo) {
                return this.userId.get().equals(((SeatInfo) obj).userId.get());
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(userId.get());
        }

        public void updateState(TUIRoomDefine.SeatInfo seatInfo) {
            this.index = seatInfo.index;
            this.userId.set(seatInfo.userId, false);
            if (!TextUtils.isEmpty(seatInfo.userName)) {
                this.name.set(seatInfo.userName, false);
            } else {
                this.name.set(this.userId.get(), false);
            }
            this.avatarUrl.set(seatInfo.avatarUrl, false);
            this.isLocked.set(seatInfo.isLocked, false);
            this.isAudioLocked.set(seatInfo.isAudioLocked, false);
        }

        public void updateState(SeatInfo newSeatInfo) {
            this.index = newSeatInfo.index;
            this.userId.set(newSeatInfo.userId.get());
            if (!TextUtils.isEmpty(newSeatInfo.name.get())) {
                this.name.set(newSeatInfo.name.get(), false);
            } else {
                this.name.set(this.userId.get(), false);
            }
            this.avatarUrl.set(newSeatInfo.avatarUrl.get(), false);
            this.isLocked.set(newSeatInfo.isLocked.get(), false);
            this.isAudioLocked.set(newSeatInfo.isAudioLocked.get(), false);
        }
    }

    public static class SeatApplication {
        public String id        = "";
        public String userId    = "";
        public String userName  = "";
        public String avatarUrl = "";
        public long   timestamp;

        public SeatApplication(String id) {
            this.id = id;
        }

        public void updateState(TUIRoomDefine.Request request) {
            this.avatarUrl = request.avatarUrl;
            this.userId = request.userId;
            this.timestamp = request.timestamp;
            if (!TextUtils.isEmpty(request.userName)) {
                this.userName = request.userName;
            } else {
                this.userName = request.userId;
            }
        }

        @Override
        public boolean equals(@Nullable Object obj) {
            if (TextUtils.isEmpty(this.id)) {
                return false;
            }
            if (obj instanceof SeatApplication) {
                return this.id.equals(((SeatApplication) obj).id);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(id);
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatApplication{"
                    + "id='" + id + '\''
                    + ", userId='" + userId + '\''
                    + ", timestamp=" + timestamp
                    + '}';
        }
    }

    public static class SeatInvitation {
        public String id        = "";
        public String userId    = "";
        public String userName  = "";
        public String avatarUrl = "";
        public long   timestamp;

        public void updateState(TUIRoomDefine.Request request) {
            this.id = request.requestId;
            this.avatarUrl = request.avatarUrl;
            this.userId = request.userId;
            this.timestamp = request.timestamp;
            if (!TextUtils.isEmpty(request.userName)) {
                this.userName = request.userName;
            } else {
                this.userName = request.userId;
            }
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatInvitation{"
                    + "id='" + id + '\''
                    + ", userId='" + userId + '\''
                    + ", timestamp=" + timestamp
                    + '}';
        }

        public void reset() {
            this.id = "";
            this.userId = "";
            this.userName = "";
            this.avatarUrl = "";
            this.timestamp = 0;
        }
    }
}
