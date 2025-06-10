package com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.managerpanel;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_REQUEST_CONNECTION;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.core.widget.NestedScrollView;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class CoHostManageDialog extends PopupDialog implements ITUINotification {
    private       TextView                 mTextConnectedTitle;
    private       RecyclerView             mRecyclerConnectedList;
    private RecyclerView             mRecyclerRecommendList;
    private ConnectedAdapter mAnchorConnectedAdapter;
    private RecommendAdapter mAnchorRecommendedAdapter;
    private TextView         mTextDisconnect;
    private       SwipeRefreshLayout       mSwipeRefreshLayout;
    private       NestedScrollView mNestedScrollView;
    private final AnchorManager    mManager;
    private final LiveCoreView     mCoreView;

    private final Observer<List<TUILiveConnectionManager.ConnectionUser>> mRecommendObserver
            = this::onRecommendListChange;
    private final Observer<List<TUILiveConnectionManager.ConnectionUser>> mConnectedObserver
            = this::onConnectedUserChange;
    private       TextView                                                mTextRecommendTitle;

    public CoHostManageDialog(Context context, AnchorManager manager) {
        super(context);
        mManager = manager;
        mCoreView = mManager.getCoreView();
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_connection_manager_panel
                , null);
        bindViewId(view);
        initRefresh();
        initConnectingList();
        initRecommendTitle();
        initRecommendList();
        initDisconnectView();
        initNestedScrollView();
        initRecommendData();
        setView(view);
        addObserver();
    }

    private void bindViewId(View view) {
        mTextConnectedTitle = view.findViewById(R.id.tv_connected_title);
        mTextRecommendTitle = view.findViewById(R.id.tv_recommend_title);
        mRecyclerConnectedList = view.findViewById(R.id.rv_connecting_user_list);
        mRecyclerRecommendList = view.findViewById(R.id.rv_recommendation_user_list);
        mSwipeRefreshLayout = view.findViewById(R.id.srl_recommendation_user_list);
        mTextDisconnect = view.findViewById(R.id.tv_disconnect);
        mNestedScrollView = view.findViewById(R.id.nsv_scroll_view);
    }

    protected void addObserver() {
        mManager.getState().recommendUsers.observeForever(mRecommendObserver);
        mManager.getCoreState().coHostState.connectedUserList.observeForever(mConnectedObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_REQUEST_CONNECTION, this);
    }

    protected void removeObserver() {
        mManager.getState().recommendUsers.removeObserver(mRecommendObserver);
        mManager.getCoreState().coHostState.connectedUserList.removeObserver(mConnectedObserver);
        TUICore.unRegisterEvent(this);
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onRecommendListChange(List<TUILiveConnectionManager.ConnectionUser> recommendList) {
        mTextRecommendTitle.post(() -> {
            if (mManager.getState().recommendUsers.getValue().isEmpty()) {
                mTextRecommendTitle.setVisibility(GONE);
            } else {
                mTextRecommendTitle.setVisibility(VISIBLE);
            }
            mAnchorRecommendedAdapter.updateData(mManager.getState().recommendUsers.getValue());
            mAnchorRecommendedAdapter.notifyDataSetChanged();
        });
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onConnectedUserChange(List<TUILiveConnectionManager.ConnectionUser> connectedList) {
        mTextDisconnect.post(() -> {
            if (mManager.getCoreState().coHostState.connectedUserList.getValue().isEmpty()) {
                mTextDisconnect.setVisibility(GONE);
                mTextConnectedTitle.setVisibility(GONE);
            } else {
                mTextDisconnect.setVisibility(VISIBLE);
                mTextConnectedTitle.setVisibility(VISIBLE);
                mTextConnectedTitle.setVisibility(VISIBLE);
                mTextConnectedTitle.setText(
                        String.format(getContext().getString(R.string.common_connection_list_title),
                                mManager.getCoreState().coHostState.connectedUserList.getValue().size() - 1));
            }
            mAnchorConnectedAdapter.updateData(mManager.getCoreState().coHostState.connectedUserList.getValue());
            mAnchorConnectedAdapter.notifyDataSetChanged();
        });
    }

    private void initRecommendList() {
        mRecyclerRecommendList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL,
                false));
        mAnchorRecommendedAdapter = new RecommendAdapter(getContext(), mManager);
        mRecyclerRecommendList.setAdapter(mAnchorRecommendedAdapter);
    }

    private void initConnectingList() {
        mRecyclerConnectedList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL,
                false));

        mAnchorConnectedAdapter = new ConnectedAdapter(getContext(), mManager);
        mRecyclerConnectedList.setAdapter(mAnchorConnectedAdapter);
    }

    private void initRefresh() {
        updateRefreshLayoutHeight();
        mSwipeRefreshLayout.setOnRefreshListener(() -> {
            initRecommendData();
            mSwipeRefreshLayout.setRefreshing(false);
        });
    }

    private void updateRefreshLayoutHeight() {
        LinearLayout.LayoutParams params = (LinearLayout.LayoutParams) mSwipeRefreshLayout.getLayoutParams();
        params.height = ScreenUtil.getScreenHeight(getContext()) - ScreenUtil.dip2px(100);
        mSwipeRefreshLayout.setLayoutParams(params);
    }

    private void initRecommendData() {
        mManager.fetchLiveList();
    }

    private void initRecommendTitle() {
        if (mManager.getState().recommendUsers.getValue().isEmpty()) {
            mTextRecommendTitle.setVisibility(GONE);
        } else {
            mTextRecommendTitle.setVisibility(VISIBLE);
        }
    }

    private void initDisconnectView() {
        mTextDisconnect.setOnClickListener(v -> {
            showDisconnectDialog();
        });
    }

    private void initNestedScrollView() {
        mNestedScrollView.setOnScrollChangeListener((NestedScrollView.OnScrollChangeListener)
                (v, scrollX, scrollY, oldScrollX, oldScrollY) -> {
                    if (scrollY >= (v.getChildAt(0).getMeasuredHeight() - v.getMeasuredHeight())) {
                        initRecommendData();
                    }
                });
    }

    private void showDisconnectDialog() {
        TerminateCoHostDialog dialog = new TerminateCoHostDialog(getContext(), mManager);

        dialog.show();
    }

    private void disconnect() {
        mManager.terminateCrossRoomConnection();
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
                showConnectionToast(getContext().getString(R.string.common_connect_conflict));
                break;
            case CONNECTION_FULL:
                showConnectionToast(getContext().getString(R.string.common_connection_room_full));
                break;
            default:
                showConnectionToast(getContext().getString(R.string.common_connect_error));
                break;
        }
    }

    private void showConnectionToast(String tips) {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_connection_toast, null, true);

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