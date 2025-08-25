package com.trtc.uikit.livekit.features.anchorboardcast.view.cohost.panel;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;
import static com.trtc.uikit.livekit.features.anchorboardcast.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.features.anchorboardcast.manager.Constants.EVENT_SUB_KEY_REQUEST_CONNECTION;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.RadioGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.core.widget.NestedScrollView;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.Debug;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class AnchorCoHostManageDialog extends PopupDialog implements ITUINotification {
    private       TextView                 mTextConnectedTitle;
    private       NestedScrollView         mNestedScrollView;
    private       RecyclerView             mRecyclerConnectedList;
    private       RecyclerView             mRecyclerRecommendList;
    private       AnchorConnectingAdapter  mAnchorConnectedAdapter;
    private       AnchorRecommendedAdapter mAnchorRecommendedAdapter;
    private       LinearLayoutManager      mLinearLayoutManager;
    private       TextView                 mTextDisconnect;
    private       SwipeRefreshLayout       mSwipeRefreshLayout;
    private final AnchorManager            mAnchorManager;
    private final LiveCoreView             mLiveStream;

    private final Observer<List<CoHostState.ConnectionUser>> mRecommendObserver = this::onRecommendListChange;
    private final Observer<List<ConnectionUser>>             mConnectedObserver = this::onConnectedUserChange;
    private       TextView                                   mTextRecommendTitle;

    public AnchorCoHostManageDialog(Context context, AnchorManager manager, LiveCoreView liveStream) {
        super(context);
        mAnchorManager = manager;
        mLiveStream = liveStream;
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_layout_anchor_connection_manager_panel
                , null);
        bindViewId(view);
        initBackView(view);
        initRefresh();
        initConnectingList();
        initRecommendTitle();
        initRecommendList();
        initDisconnectView();
        initNestedScrollView();
        refreshData();
        setView(view);
        addObserver();
    }

    private void bindViewId(View view) {
        mNestedScrollView = view.findViewById(R.id.nsv_scroll_view);
        mTextConnectedTitle = view.findViewById(R.id.tv_connected_title);
        mTextRecommendTitle = view.findViewById(R.id.tv_recommend_title);
        mRecyclerConnectedList = view.findViewById(R.id.rv_connecting_user_list);
        mRecyclerRecommendList = view.findViewById(R.id.rv_recommendation_user_list);
        mSwipeRefreshLayout = view.findViewById(R.id.srl_recommendation_user_list);
        mTextDisconnect = view.findViewById(R.id.tv_disconnect);
    }

    protected void addObserver() {
        mAnchorManager.getCoHostState().recommendUsers.observeForever(mRecommendObserver);
        mAnchorManager.getCoreState().coHostState.connectedUserList.observeForever(mConnectedObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_REQUEST_CONNECTION, this);
    }

    protected void removeObserver() {
        mAnchorManager.getCoHostState().recommendUsers.removeObserver(mRecommendObserver);
        mAnchorManager.getCoreState().coHostState.connectedUserList.removeObserver(mConnectedObserver);
        TUICore.unRegisterEvent(this);
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onRecommendListChange(List<CoHostState.ConnectionUser> recommendList) {
        mTextRecommendTitle.post(() -> {
            if (mAnchorManager.getCoHostManager().getRecommendedList().isEmpty()) {
                mTextRecommendTitle.setVisibility(GONE);
            } else {
                mTextRecommendTitle.setVisibility(VISIBLE);
            }
            mAnchorRecommendedAdapter.updateData(mAnchorManager.getCoHostManager().getRecommendedList());
            mAnchorRecommendedAdapter.notifyDataSetChanged();
        });
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onConnectedUserChange(List<ConnectionUser> connectedList) {
        mTextDisconnect.post(() -> {
            if (mAnchorManager.getCoreState().coHostState.connectedUserList.getValue().isEmpty()) {
                mTextDisconnect.setVisibility(GONE);
                mTextConnectedTitle.setVisibility(GONE);
            } else {
                mTextDisconnect.setVisibility(VISIBLE);
                mTextConnectedTitle.setVisibility(VISIBLE);
                mTextConnectedTitle.setVisibility(VISIBLE);
                mTextConnectedTitle.setText(getContext().getString(R.string.common_connection_list_title,
                        mAnchorManager.getCoreState().coHostState.connectedUserList.getValue().size() - 1));
            }
            mAnchorConnectedAdapter.updateData(mAnchorManager.getCoreState().coHostState.connectedUserList.getValue());
            mAnchorConnectedAdapter.notifyDataSetChanged();
        });
    }

    private void initRecommendList() {
        mLinearLayoutManager = new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL, false);
        mRecyclerRecommendList.setLayoutManager(mLinearLayoutManager);
        mAnchorRecommendedAdapter = new AnchorRecommendedAdapter(getContext(), mAnchorManager, mLiveStream);
        mRecyclerRecommendList.setAdapter(mAnchorRecommendedAdapter);
    }

    private void refreshData() {
        mAnchorManager.getCoHostManager().fetchLiveList(true);
    }

    private void loadMoreData() {
        mAnchorManager.getCoHostManager().fetchLiveList(false);
    }

    private void initConnectingList() {
        mRecyclerConnectedList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL,
                false));

        mAnchorConnectedAdapter = new AnchorConnectingAdapter(getContext(), mAnchorManager);
        mRecyclerConnectedList.setAdapter(mAnchorConnectedAdapter);
    }

    private void initBackView(View rootView) {
        View view = rootView.findViewById(R.id.iv_back);
        if (view != null) {
            view.setOnClickListener(v -> dismiss());
        }
    }

    private void initRefresh() {
        mSwipeRefreshLayout.setOnRefreshListener(() -> {
            mAnchorManager.getCoHostManager().fetchLiveList(true);
            mSwipeRefreshLayout.setRefreshing(false);
        });
    }

    private void initRecommendTitle() {
        if (mAnchorManager.getCoHostState().recommendUsers.getValue().isEmpty()) {
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
                        Log.d("xander_test", "loadMoreData");
                        if (!mAnchorManager.getCoHostState().isLoadMore && !mAnchorManager.getCoHostState().isLastPage) {
                            loadMoreData();
                        }
                    }
                });
    }

    private void showDisconnectDialog() {
        StandardDialog dialog = new StandardDialog(getContext());
        dialog.setContent(getContext().getString(R.string.common_disconnect_tips));
        dialog.setAvatar(null);

        dialog.setNegativeText(getContext().getString(R.string.common_disconnect_cancel), negativeView -> {
            dialog.dismiss();
        });
        dialog.setPositiveText(getContext().getString(R.string.common_end_connect), positiveView -> {
            dialog.dismiss();
            disconnect();
        });
        dialog.show();
    }

    private void disconnect() {
        mLiveStream.terminateCrossRoomConnection();
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (TextUtils.equals(key, EVENT_KEY_LIVE_KIT) && TextUtils.equals(subKey, EVENT_SUB_KEY_REQUEST_CONNECTION)) {
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