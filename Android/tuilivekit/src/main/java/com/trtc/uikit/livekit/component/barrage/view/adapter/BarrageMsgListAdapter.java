package com.trtc.uikit.livekit.component.barrage.view.adapter;

import android.annotation.SuppressLint;
import android.util.Log;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.component.barrage.BarrageStreamView.OnMessageClickListener;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class BarrageMsgListAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private static final String TAG = "BarrageMsgListAdapter";

    private final List<Barrage> mMsgEntityList;

    private final Map<Integer, BarrageItemAdapter> mAdapterMap = new HashMap<>();
    private       BarrageItemTypeDelegate          mViewTypeDelegate;
    private       OnMessageClickListener           mOnMessageClickListener;

    public BarrageMsgListAdapter(List<Barrage> msgEntityList) {
        this.mMsgEntityList = msgEntityList;
        setItemTypeDelegate((position, barrage) -> 0);
    }

    public void setItemTypeDelegate(BarrageItemTypeDelegate delegate) {
        mViewTypeDelegate = delegate;
    }

    public void setItemAdapter(int itemType, BarrageItemAdapter adapter) {
        mAdapterMap.put(itemType, adapter);
        Log.i(TAG, "setItemAdapter:[itemType:" + itemType + ", adapter:" + adapter + "]");
    }

    public void setOnMessageClickListener(OnMessageClickListener listener) {
        mOnMessageClickListener = listener;
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        BarrageItemAdapter adapter = mAdapterMap.get(viewType);
        if (adapter != null) {
            return adapter.onCreateViewHolder(parent);
        }
        return null;
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position) {
        for (Integer viewType : mAdapterMap.keySet()) {
            BarrageItemAdapter adapter = mAdapterMap.get(viewType);
            if (adapter == null) {
                continue;
            }
            int itemViewType = getItemViewType(position);
            if (itemViewType - viewType == 0) {
                Barrage barrage = mMsgEntityList.get(position);
                adapter.onBindViewHolder(holder, position, barrage);
                holder.itemView.setOnClickListener(v -> {
                    OnMessageClickListener listener = mOnMessageClickListener;
                    if (listener != null) {
                        TUIRoomDefine.UserInfo userInfo = new TUIRoomDefine.UserInfo();
                        userInfo.userId = barrage.user.userId;
                        userInfo.userName = barrage.user.userName;
                        userInfo.avatarUrl = barrage.user.avatarUrl;
                        listener.onMessageClick(userInfo);
                    }
                });
                break;
            }
        }
    }

    @Override
    public int getItemCount() {
        return mMsgEntityList.size();
    }

    @Override
    public int getItemViewType(int position) {
        Barrage barrage = mMsgEntityList.get(position);
        return mViewTypeDelegate.getItemType(position, barrage);
    }
}