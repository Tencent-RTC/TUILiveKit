package com.tencent.liteav.demo;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.Toolbar;

import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;

import com.blankj.utilcode.util.SPUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.tencent.imsdk.v2.V2TIMGroupInfoResult;
import com.tencent.liteav.basic.IntentUtils;
import com.tencent.liteav.basic.UserModel;
import com.tencent.liteav.basic.UserModelManager;
import com.tencent.liteav.debug.GenerateTestUserSig;
import com.tencent.liteav.liveroom.model.LiveRoomManager;
import com.tencent.liteav.liveroom.model.TRTCLiveRoom;
import com.tencent.liteav.liveroom.model.TRTCLiveRoomCallback;
import com.tencent.liteav.liveroom.model.TRTCLiveRoomDef;
import com.tencent.liteav.liveroom.ui.anchor.TCCameraAnchorActivity;
import com.tencent.liteav.liveroom.ui.audience.TCAudienceActivity;
import com.tencent.liteav.liveroom.ui.common.utils.TCConstants;

import java.util.Collections;
import java.util.List;

public class MainActivity extends Activity {

    private static final String TAG = MainActivity.class.getSimpleName();

    private TRTCLiveRoom mTRTCLiveRoom;

    private Toolbar  mToolbar;
    private EditText mRoomIdEt;
    private TextView mEnterRoomTv;

    private String  mSelfUserId;                         //表示当前登录用户的UserID
    private boolean isUseCDNPlay = false;                //用来表示当前是否CDN模式（区别于TRTC模式）

    private final TextWatcher mEditTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            if (!TextUtils.isEmpty(mRoomIdEt.getText().toString())) {
                mEnterRoomTv.setEnabled(true);
            } else {
                mEnterRoomTv.setEnabled(false);
            }
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        initView();
        initData();
    }

    private void initData() {
        mRoomIdEt.addTextChangedListener(mEditTextWatcher);
        mSelfUserId = UserModelManager.getInstance().getUserModel().userId;
        isUseCDNPlay = SPUtils.getInstance().getBoolean(TCConstants.USE_CDN_PLAY, false);

        final UserModel userModel = UserModelManager.getInstance().getUserModel();
        mTRTCLiveRoom = TRTCLiveRoom.sharedInstance(this);
        mTRTCLiveRoom.login(GenerateTestUserSig.SDKAPPID, userModel.userId, userModel.userSig, new TRTCLiveRoomDef.TRTCLiveRoomConfig(isUseCDNPlay, "http://3891.liveplay.myqcloud.com/live"), new TRTCLiveRoomCallback.ActionCallback() {
            @Override
            public void onCallback(int code, String msg) {
                if (code == 0) {
                    mTRTCLiveRoom.setSelfProfile(userModel.userName, userModel.userAvatar, new TRTCLiveRoomCallback.ActionCallback() {
                        @Override
                        public void onCallback(int code, String msg) {
                            if (code == 0) {
                            }
                        }
                    });
                }
            }
        });
    }

    private void initView() {
        mToolbar = (Toolbar) findViewById(R.id.toolbar);
        mRoomIdEt = (EditText) findViewById(R.id.et_room_id);
        mEnterRoomTv = (TextView) findViewById(R.id.tv_enter_room);
        findViewById(R.id.tv_enter_room).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                final String roomId = mRoomIdEt.getText().toString().trim();
                enterRoom(roomId);
            }
        });

        findViewById(R.id.btn_create_room).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                createRoom();
            }
        });

        findViewById(R.id.btn_trtcliveroom_link).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent(Intent.ACTION_VIEW);
                intent.setData(Uri.parse(TCConstants.TRTC_LIVE_ROOM_DOCUMENT_URL));
                IntentUtils.safeStartActivity(MainActivity.this, intent);
            }
        });

    }

    /**
     * 创建房间
     */
    private void createRoom() {
        Intent intent = new Intent(this, TCCameraAnchorActivity.class);
        startActivity(intent);
    }

    /**
     * 进入房间
     *
     * @param roomIdStr
     */
    private void enterRoom(final String roomIdStr) {
        LiveRoomManager.getInstance().getGroupInfo(roomIdStr, new LiveRoomManager.GetGroupInfoCallback() {
            @Override
            public void onSuccess(V2TIMGroupInfoResult result) {
                if (isRoomExist(result)) {
                    realEnterRoom(roomIdStr);
                } else {
                    ToastUtils.showLong(R.string.room_not_exist);
                }
            }

            @Override
            public void onFailed(int code, String msg) {
                ToastUtils.showLong(msg);
            }
        });
    }

    private boolean isRoomExist(V2TIMGroupInfoResult result) {
        if (result == null) {
            Log.e(TAG, "room not exist result is null");
            return false;
        }
        return result.getResultCode() == 0;
    }

    private void realEnterRoom(String roomIdStr) {
        int roomId;
        try {
            roomId = Integer.parseInt(roomIdStr);
        } catch (Exception e) {
            roomId = 10000;
        }
        final int roomId2 = roomId;
        mTRTCLiveRoom.getRoomInfos(Collections.singletonList(roomId), new TRTCLiveRoomCallback.RoomInfoCallback() {
            @Override
            public void onCallback(int code, String msg, List<TRTCLiveRoomDef.TRTCLiveRoomInfo> list) {
                if (0 == code && null != list && !list.isEmpty()) {
                    final TRTCLiveRoomDef.TRTCLiveRoomInfo info = list.get(0);
                    runOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            gotoAudience(roomId2, info.ownerId, info.ownerName);
                        }
                    });
                }
            }
        });
    }

    private void gotoAudience(int roomId, String anchorId, String anchorName) {
        Intent intent = new Intent(MainActivity.this, TCAudienceActivity.class);
        intent.putExtra(TCConstants.GROUP_ID, roomId);
        intent.putExtra(TCConstants.PUSHER_ID, anchorId);
        intent.putExtra(TCConstants.PUSHER_NAME, anchorName);
        startActivity(intent);
    }
}
