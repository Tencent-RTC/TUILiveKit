package com.trtc.uikit.livekit.component.barrage.view.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Log;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class BarrageMsgListAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private static final String TAG = "BarrageMsgListAdapter";

    private final List<Barrage> mMsgEntityList;

    private final Map<Integer, BarrageItemAdapter> mAdapterMap = new HashMap<>();
    private       BarrageItemTypeDelegate          mViewTypeDelegate;

    public BarrageMsgListAdapter(Context context, String ownerId, List<Barrage> msgEntityList) {
        this.mMsgEntityList = msgEntityList;
        setItemAdapter(0, new BarrageItemDefaultAdapter(context, ownerId));
        setItemTypeDelegate((position, barrage) -> 0);
    }

    public void setItemTypeDelegate(BarrageItemTypeDelegate delegate) {
        mViewTypeDelegate = delegate;
    }

    public void setItemAdapter(int itemType, BarrageItemAdapter adapter) {
        mAdapterMap.put(itemType, adapter);
        Log.i(TAG, "setItemAdapter:[itemType:" + itemType + ", adapter:" + adapter + "]");
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
                adapter.onBindViewHolder(holder, position, mMsgEntityList.get(position));
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