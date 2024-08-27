package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.connection;

import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_REQUEST_CONNECTION;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.core.widget.NestedScrollView;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.view.StandardDialog;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.operation.ConnectionState;

import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class AnchorConnectionManagePanel extends BasicView implements ITUINotification {
    private       TextView                 mTextConnectedTitle;
    private       RecyclerView             mRecyclerConnectedList;
    private       RecyclerView             mRecyclerRecommendList;
    private       AnchorConnectingAdapter  mAnchorConnectedAdapter;
    private       AnchorRecommendedAdapter mAnchorRecommendedAdapter;
    private final LiveController           mLiveController;
    private       TextView                 mTextDisconnect;
    private       TextView                 mTextRecommendTitle;

    private final Observer<List<ConnectionState.ConnectionUser>> mRecommendObserver
            = this::onRecommendListChange;
    private final Observer<List<ConnectionState.ConnectionUser>> mConnectedObserver
            = this::onConnectedUserChange;

    public AnchorConnectionManagePanel(Context context, LiveController liveController,
                                       PopupDialog.DialogActionListener listener) {
        super(context, liveController);
        mLiveController = liveController;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_connection_manager_panel, this, true);
        bindViewId();

        initRefresh();
        initConnectingList();
        initRecommendTitle();
        initRecommendList();
        initDisconnectView();
        initNestedScrollView();
        initRecommendData();
    }

    @Override
    protected void addObserver() {
        mConnectionState.recommendUsers.observe(mRecommendObserver);
        mConnectionState.connectedUsers.observe(mConnectedObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_REQUEST_CONNECTION, this);
    }

    @Override
    protected void removeObserver() {
        mConnectionState.recommendUsers.removeObserver(mRecommendObserver);
        mConnectionState.connectedUsers.removeObserver(mConnectedObserver);
        TUICore.unRegisterEvent(this);
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onRecommendListChange(List<ConnectionState.ConnectionUser> recommendList) {
        post(() -> {
            if (mLiveController.getConnectionController().getRecommendedList().isEmpty()) {
                mTextRecommendTitle.setVisibility(GONE);
            } else {
                mTextRecommendTitle.setVisibility(VISIBLE);
            }
            mAnchorRecommendedAdapter.updateData(mLiveController.getConnectionController().getRecommendedList());
            mAnchorRecommendedAdapter.notifyDataSetChanged();
        });
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onConnectedUserChange(List<ConnectionState.ConnectionUser> connectedList) {
        post(() -> {
            if (mConnectionState.connectedUsers.get().isEmpty()) {
                mTextDisconnect.setVisibility(GONE);
                mTextConnectedTitle.setVisibility(GONE);
            } else {
                mTextDisconnect.setVisibility(VISIBLE);
                mTextConnectedTitle.setVisibility(VISIBLE);
                mTextConnectedTitle.setVisibility(VISIBLE);
                mTextConnectedTitle.setText(String.format(mContext.getString(R.string.livekit_connection_list_title),
                        mConnectionState.connectedUsers.get().size() - 1));
            }
            mAnchorConnectedAdapter.updateData(mConnectionState.connectedUsers.get());
            mAnchorConnectedAdapter.notifyDataSetChanged();
        });
    }

    private void bindViewId() {
        mTextConnectedTitle = findViewById(R.id.tv_connected_title);
        mTextRecommendTitle = findViewById(R.id.tv_recommend_title);
        mRecyclerConnectedList = findViewById(R.id.rv_connecting_user_list);

        mRecyclerRecommendList = findViewById(R.id.rv_recommendation_user_list);
    }

    private void initRecommendList() {
        mRecyclerRecommendList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));
        mAnchorRecommendedAdapter = new AnchorRecommendedAdapter(mContext, mLiveController,
                mConnectionState.recommendUsers.get());
        mRecyclerRecommendList.setAdapter(mAnchorRecommendedAdapter);
    }

    private void initConnectingList() {
        mRecyclerConnectedList.setLayoutManager(new LinearLayoutManager(mContext, LinearLayoutManager.VERTICAL,
                false));

        mAnchorConnectedAdapter = new AnchorConnectingAdapter(mContext, mLiveController);
        mRecyclerConnectedList.setAdapter(mAnchorConnectedAdapter);
    }

    private void initRefresh() {
        SwipeRefreshLayout swipeRefreshLayout = findViewById(R.id.srl_recommendation_user_list);
        swipeRefreshLayout.setOnRefreshListener(() -> {
            initRecommendData();
            swipeRefreshLayout.setRefreshing(false);
        });
    }

    private void initRecommendData() {
        mConnectionController.fetchLiveList();
    }

    private void initRecommendTitle() {
        if (mConnectionState.recommendUsers.get().isEmpty()) {
            mTextRecommendTitle.setVisibility(GONE);
        } else {
            mTextRecommendTitle.setVisibility(VISIBLE);
        }
    }

    private void initDisconnectView() {
        mTextDisconnect = findViewById(R.id.tv_disconnect);
        mTextDisconnect.setOnClickListener(v -> {
            showDisconnectDialog();
        });
    }

    private void initNestedScrollView() {
        NestedScrollView nestedScrollView = findViewById(R.id.nsv_scroll_view);
        nestedScrollView.setOnScrollChangeListener((NestedScrollView.OnScrollChangeListener)
                (v, scrollX, scrollY, oldScrollX, oldScrollY) -> {
                    if (scrollY >= (v.getChildAt(0).getMeasuredHeight() - v.getMeasuredHeight())) {
                        initRecommendData();
                    }
                });
    }

    private void showDisconnectDialog() {
        StandardDialog dialog = new StandardDialog(getContext());
        dialog.setContent(getContext().getString(R.string.livekit_disconnect_tips));
        dialog.setAvatar(null);

        dialog.setNegativeText(getContext().getString(R.string.livekit_disconnect_cancel),
                negativeView -> {
                    dialog.dismiss();
                });
        dialog.setPositiveText(getContext().getString(R.string.livekit_end_connect), positiveView -> {
            dialog.dismiss();
            disconnect();
        });
        dialog.show();
    }

    private void disconnect() {
        mConnectionController.disconnect();
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (TextUtils.equals(key, EVENT_KEY_LIVE_KIT)
                && TextUtils.equals(subKey, EVENT_SUB_KEY_REQUEST_CONNECTION)) {
            if (param == null) {
                showConnectionErrorToast(TUILiveConnectionManager.ConnectionCode.UNKNOWN);
            } else {
                Map.Entry<String, Object> entry = param.entrySet().iterator().next();
                showConnectionErrorToast((TUILiveConnectionManager.ConnectionCode) entry.getValue());
            }
        }
    }

    private void showConnectionErrorToast(TUILiveConnectionManager.ConnectionCode resultCode) {
        switch (resultCode) {
            case CONNECTING:
            case CONNECTING_OTHER_ROOM:
                showConnectionToast(mContext.getString(R.string.livekit_connect_conflict));
                break;
            case CONNECTION_FULL:
                showConnectionToast(mContext.getString(R.string.livekit_connection_room_full));
                break;
            default:
                showConnectionToast(mContext.getString(R.string.livekit_connect_error));
                break;
        }
    }

    private void showConnectionToast(String tips) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.livekit_connection_toast, null, true);

        TextView text = view.findViewById(R.id.tv_toast_text);
        text.setText(tips);
        ImageView image = view.findViewById(R.id.iv_toast_image);
        image.setImageResource(R.drawable.livekit_connection_toast_icon);

        Toast toast = new Toast(view.getContext());
        toast.setDuration(Toast.LENGTH_SHORT);
        toast.setView(view);
        toast.show();
    }
}