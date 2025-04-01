package com.trtc.uikit.livekit.livestream.view.anchor.pushing.cohost;

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
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.List;
import java.util.Map;

@SuppressLint("ViewConstructor")
public class AnchorCoHostManageDialog extends PopupDialog implements ITUINotification {
    private       TextView                 mTextConnectedTitle;
    private       RecyclerView             mRecyclerConnectedList;
    private       RecyclerView             mRecyclerRecommendList;
    private       AnchorConnectingAdapter  mAnchorConnectedAdapter;
    private       AnchorRecommendedAdapter mAnchorRecommendedAdapter;
    private       TextView                 mTextDisconnect;
    private       SwipeRefreshLayout       mSwipeRefreshLayout;
    private       NestedScrollView         mNestedScrollView;
    private final LiveStreamManager        mLiveManager;
    private final LiveCoreView             mLiveStream;

    private final Observer<List<CoHostState.ConnectionUser>> mRecommendObserver
            = this::onRecommendListChange;
    private final Observer<List<CoHostState.ConnectionUser>> mConnectedObserver
            = this::onConnectedUserChange;
    private       TextView                                   mTextRecommendTitle;

    public AnchorCoHostManageDialog(Context context, LiveStreamManager manager, LiveCoreView liveStream) {
        super(context);
        mLiveManager = manager;
        mLiveStream = liveStream;
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
        mLiveManager.getCoHostState().recommendUsers.observeForever(mRecommendObserver);
        mLiveManager.getCoHostState().connectedUsers.observeForever(mConnectedObserver);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_REQUEST_CONNECTION, this);
    }

    protected void removeObserver() {
        mLiveManager.getCoHostState().recommendUsers.removeObserver(mRecommendObserver);
        mLiveManager.getCoHostState().connectedUsers.removeObserver(mConnectedObserver);
        TUICore.unRegisterEvent(this);
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onRecommendListChange(List<CoHostState.ConnectionUser> recommendList) {
        mTextRecommendTitle.post(() -> {
            if (mLiveManager.getCoHostManager().getRecommendedList().isEmpty()) {
                mTextRecommendTitle.setVisibility(GONE);
            } else {
                mTextRecommendTitle.setVisibility(VISIBLE);
            }
            mAnchorRecommendedAdapter.updateData(mLiveManager.getCoHostManager().getRecommendedList());
            mAnchorRecommendedAdapter.notifyDataSetChanged();
        });
    }

    @SuppressLint("NotifyDataSetChanged")
    private void onConnectedUserChange(List<CoHostState.ConnectionUser> connectedList) {
        mTextDisconnect.post(() -> {
            if (mLiveManager.getCoHostState().connectedUsers.getValue().isEmpty()) {
                mTextDisconnect.setVisibility(GONE);
                mTextConnectedTitle.setVisibility(GONE);
            } else {
                mTextDisconnect.setVisibility(VISIBLE);
                mTextConnectedTitle.setVisibility(VISIBLE);
                mTextConnectedTitle.setVisibility(VISIBLE);
                mTextConnectedTitle.setText(
                        String.format(getContext().getString(R.string.live_connection_list_title),
                                mLiveManager.getCoHostState().connectedUsers.getValue().size() - 1));
            }
            mAnchorConnectedAdapter.updateData(mLiveManager.getCoHostState().connectedUsers.getValue());
            mAnchorConnectedAdapter.notifyDataSetChanged();
        });
    }

    private void initRecommendList() {
        mRecyclerRecommendList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL,
                false));
        mAnchorRecommendedAdapter = new AnchorRecommendedAdapter(getContext(), mLiveManager, mLiveStream);
        mRecyclerRecommendList.setAdapter(mAnchorRecommendedAdapter);
    }

    private void initConnectingList() {
        mRecyclerConnectedList.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL,
                false));

        mAnchorConnectedAdapter = new AnchorConnectingAdapter(getContext(), mLiveManager);
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
        mLiveManager.getCoHostManager().fetchLiveList();
    }

    private void initRecommendTitle() {
        if (mLiveManager.getCoHostState().recommendUsers.getValue().isEmpty()) {
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
        StandardDialog dialog = new StandardDialog(getContext());
        dialog.setContent(getContext().getString(R.string.live_disconnect_tips));
        dialog.setAvatar(null);

        dialog.setNegativeText(getContext().getString(R.string.live_disconnect_cancel),
                negativeView -> {
                    dialog.dismiss();
                });
        dialog.setPositiveText(getContext().getString(R.string.live_end_connect), positiveView -> {
            dialog.dismiss();
            disconnect();
        });
        dialog.show();
    }

    private void disconnect() {
        mLiveStream.terminateCrossRoomConnection();
        mLiveManager.getCoHostManager().cleanConnectedUsers();
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
                showConnectionToast(getContext().getString(R.string.live_connect_conflict));
                break;
            case CONNECTION_FULL:
                showConnectionToast(getContext().getString(R.string.live_connection_room_full));
                break;
            default:
                showConnectionToast(getContext().getString(R.string.live_connect_error));
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