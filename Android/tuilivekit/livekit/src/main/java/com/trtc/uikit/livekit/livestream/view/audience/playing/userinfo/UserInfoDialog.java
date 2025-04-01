package com.trtc.uikit.livekit.livestream.view.audience.playing.userinfo;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.state.UserState;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

@SuppressLint("ViewConstructor")
public class UserInfoDialog extends PopupDialog {
    private final Context                                              mContext;
    private       Button                                               mButtonFollow;
    private       TextView                                             mTextUserName;
    private final LiveStreamManager                                    mLiveStreamManager;
    private final androidx.lifecycle.Observer<Set<UserState.UserInfo>> mMyFollowingUserObserver =
            this::onMyFollowingUserChanged;
    private       TextView                                             mTextUserId;
    private       ImageView                                            mImageAvatar;
    private       TextView                                             mTextFans;
    private       TUIRoomDefine.UserInfo                               mUserInfo;

    public UserInfoDialog(Context context, LiveStreamManager liveStreamManager) {
        super(context);
        mContext = context;
        mLiveStreamManager = liveStreamManager;
        initView();
    }

    public void init(TUIRoomDefine.UserInfo userInfo) {
        mUserInfo = userInfo;
        updateView();
    }

    private void addObserver() {
        mLiveStreamManager.getUserState().myFollowingUserList.observeForever(mMyFollowingUserObserver);
    }

    private void removeObserver() {
        mLiveStreamManager.getUserState().myFollowingUserList.removeObserver(mMyFollowingUserObserver);
    }

    private void initView() {
        View view = LayoutInflater.from(mContext).inflate(R.layout.livekit_user_info, null);
        bindViewId(view);
        updateView();
        setView(view);
    }

    private void bindViewId(View view) {
        mButtonFollow = view.findViewById(R.id.btn_follow);
        mTextUserName = view.findViewById(R.id.tv_anchor_name);
        mTextUserId = view.findViewById(R.id.tv_user_id);
        mImageAvatar = view.findViewById(R.id.iv_avatar);
        mTextFans = view.findViewById(R.id.tv_fans);
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
        getFansNumber();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void updateView() {
        if (mUserInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(mUserInfo.userId)) {
            return;
        }
        mTextUserName.setText(mUserInfo.userName.isEmpty() ? mUserInfo.userId : mUserInfo.userName);
        mTextUserId.setText(mUserInfo.userId);
        String avatarUrl = mUserInfo.avatarUrl;
        if (TextUtils.isEmpty(avatarUrl)) {
            mImageAvatar.setImageResource(R.drawable.livekit_ic_avatar);
        } else {
            ImageLoader.load(mContext, mImageAvatar, avatarUrl, R.drawable.livekit_ic_avatar);
        }

        refreshFollowButton();
        mButtonFollow.setOnClickListener(this::onFollowButtonClick);
    }

    private void getFansNumber() {
        if (mUserInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(mUserInfo.userId)) {
            return;
        }
        List<String> userIDList = new ArrayList<>();
        userIDList.add(mUserInfo.userId);
        V2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowInfo> v2TIMFollowInfos) {
                        if (v2TIMFollowInfos != null && !v2TIMFollowInfos.isEmpty()) {
                            V2TIMFollowInfo result = v2TIMFollowInfos.get(0);
                            if (result != null) {
                                mTextFans.setText(String.valueOf(result.getFollowersCount()));
                            }
                        }
                    }

                    @Override
                    public void onError(int code, String desc) {
                        LiveStreamLog.error("UserInfoDialog"+ " getUserFollowInfo failed:errorCode:" + "message:" + desc);
                        ToastUtil.toastShortMessage(code + "," + desc);
                    }
                });
    }

    private void refreshFollowButton() {
        if (mLiveStreamManager.getUserState().myFollowingUserList.getValue().contains(
                new UserState.UserInfo(mUserInfo.userId))) {
            mButtonFollow.setText(R.string.live_unfollow_anchor);
            mButtonFollow.setBackgroundResource(R.drawable.livekit_user_info_detail_button_unfollow);
        } else {
            mButtonFollow.setText(R.string.live_follow_anchor);
            mButtonFollow.setBackgroundResource(R.drawable.livekit_user_info_button_follow);
        }
        getFansNumber();
    }

    private void onMyFollowingUserChanged(Set<UserState.UserInfo> followUsers) {
        if (mUserInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(mUserInfo.userId)) {
            return;
        }
        refreshFollowButton();
    }

    private void onFollowButtonClick(View view) {
        if (mLiveStreamManager.getUserState().myFollowingUserList.getValue().contains(
                new UserState.UserInfo(mUserInfo.userId))) {
            mLiveStreamManager.getUserManager().unfollowUser(mUserInfo.userId);
        } else {
            mLiveStreamManager.getUserManager().followUser(mUserInfo.userId);
        }
    }
}
