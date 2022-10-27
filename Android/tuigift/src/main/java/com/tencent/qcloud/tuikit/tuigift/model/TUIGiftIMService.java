package com.tencent.qcloud.tuikit.tuigift.model;

import android.text.TextUtils;
import android.util.Log;

import com.google.gson.Gson;
import com.tencent.imsdk.v2.V2TIMGroupMemberInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMMessage;
import com.tencent.imsdk.v2.V2TIMSimpleMsgListener;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuikit.tuigift.presenter.TUIGiftCallBack;
import com.tencent.qcloud.tuikit.tuigift.presenter.TUIGiftPresenter;

import java.util.HashMap;

/**
 * 负责处理礼物发送、接受的消息服务
 */
public class TUIGiftIMService {
    private static final String TAG = "TUIGiftIMService";

    private String              mGroupId;
    private RecvGiftMsgListener mRecvGiftMsgListener;
    private TUIGiftPresenter    mPresenter;


    public TUIGiftIMService(String groupId) {
        this.mGroupId = groupId;
        mRecvGiftMsgListener = new RecvGiftMsgListener();
        initIMListener();
    }


    private void initIMListener() {
        V2TIMManager.getInstance().addSimpleMsgListener(mRecvGiftMsgListener);
    }

    public void unInitImListener() {
        V2TIMManager.getInstance().removeSimpleMsgListener(mRecvGiftMsgListener);
    }

    public void setPresenter(TUIGiftPresenter presenter) {
        this.mPresenter = presenter;
    }

    /**
     * 接收礼物信息
     */
    private class RecvGiftMsgListener extends V2TIMSimpleMsgListener {

        @Override
        public void onRecvGroupCustomMessage(String msgID, String groupID, V2TIMGroupMemberInfo sender,
                                             byte[] customData) {
            if (groupID == null || !groupID.equals(mGroupId)) {
                return;
            }
            String customStr = new String(customData);
            Log.i(TAG, "customData :" + customStr);
            if (TextUtils.isEmpty(customStr)) {
                Log.d(TAG, "onRecvGroupCustomMessage customData is empty");
                return;
            }

            try {
                Gson gson = new Gson();
                TUIGiftJson json = gson.fromJson(customStr, TUIGiftJson.class);
                if (!TUIGiftConstants.VALUE_VERSION.equals(json.getVersion())) {
                    Log.e(TAG, "protocol version is not match, ignore msg.");
                }
                Log.d(TAG, "onRecvGroupCustomMessage error : this is not gift msg.");
                //如果不是礼物消息,则不处理
                if (TUIGiftConstants.VALUE_BUSINESS_ID.equals(json.getBusinessID())) {

                    //礼物信息
                    TUIGiftJson.Data data = json.getData();
                    //扩展信息
                    TUIGiftJson.Data.ExtInfo extInfo = data.getExtInfo();

                    HashMap<String, String> userMap = new HashMap<>();
                    userMap.put(TUIGiftConstants.KEY_USER_ID, extInfo.getUserID());
                    userMap.put(TUIGiftConstants.KEY_USER_NAME, extInfo.getNickName());
                    userMap.put(TUIGiftConstants.KEY_USER_AVATAR, extInfo.getAvatarUrl());

                    TUIGiftModel model = new TUIGiftModel();
                    model.giftId = data.getGiftId();
                    model.animationUrl = data.getLottieUrl();
                    model.normalImageUrl = data.getImageUrl();
                    model.giveDesc = data.message;
                    model.extInfo = userMap;
                    Log.i(TAG, "model: " + model.toString());
                    if (mPresenter != null) {
                        mPresenter.recvGroupGiftMessage(mGroupId, model);
                    }
                } else if (TUIGiftConstants.VALUE_BUSINESS_ID_LIKE.equals(json.getBusinessID())) {
                    //礼物信息
                    TUIGiftJson.Data data = json.getData();
                    //扩展信息
                    TUIGiftJson.Data.ExtInfo extInfo = data.getExtInfo();

                    HashMap<String, String> userMap = new HashMap<>();
                    userMap.put(TUIGiftConstants.KEY_USER_ID, extInfo.getUserID());
                    userMap.put(TUIGiftConstants.KEY_USER_NAME, extInfo.getNickName());
                    userMap.put(TUIGiftConstants.KEY_USER_AVATAR, extInfo.getAvatarUrl());

                    TUIGiftModel model = new TUIGiftModel();
                    model.extInfo = userMap;
                    if (mPresenter != null) {
                        mPresenter.recvGroupLikeMessage(mGroupId);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * 发送礼物信息
     *
     * @param giftModel 待发送礼物信息
     * @param callback  发送结果回调
     */
    public void sendGroupGiftMessage(TUIGiftModel giftModel, final TUIGiftCallBack.ActionCallBack callback) {
        String data = getCusGiftMsgJsonStr(giftModel);
        Log.i(TAG, "send data: " + data.toString());
        V2TIMManager.getInstance().sendGroupCustomMessage(data.getBytes(), mGroupId,
                V2TIMMessage.V2TIM_PRIORITY_NORMAL, new V2TIMValueCallback<V2TIMMessage>() {
                    @Override
                    public void onError(int i, String s) {
                        if (callback != null) {
                            callback.onCallback(i, s);
                        }
                    }

                    @Override
                    public void onSuccess(V2TIMMessage v2TIMMessage) {
                        if (callback != null) {
                            callback.onCallback(0, "send group message success.");
                        }
                    }
                });
    }

    /**
     * 发送礼物信息
     *
     * @param callback 发送结果回调
     */
    public void sendGroupLikeMessage(final TUIGiftCallBack.ActionCallBack callback) {
        String data = getCusLikeMsgJsonStr();
        Log.i(TAG, "send like: " + data);
        V2TIMManager.getInstance().sendGroupCustomMessage(data.getBytes(), mGroupId,
                V2TIMMessage.V2TIM_PRIORITY_NORMAL, new V2TIMValueCallback<V2TIMMessage>() {
                    @Override
                    public void onError(int i, String s) {
                        if (callback != null) {
                            callback.onCallback(i, s);
                        }
                    }

                    @Override
                    public void onSuccess(V2TIMMessage v2TIMMessage) {
                        if (callback != null) {
                            callback.onCallback(0, "send group message success.");
                        }
                    }
                });
    }

    /**
     * 将礼物信息转换成JSON串
     *
     * @param giftModel 待转换礼物信息
     * @return 转换好的JSON串
     */
    private static String getCusGiftMsgJsonStr(TUIGiftModel giftModel) {
        TUIGiftJson sendJson = new TUIGiftJson();
        sendJson.setBusinessID(TUIGiftConstants.VALUE_BUSINESS_ID);
        sendJson.setPlatform(TUIGiftConstants.VALUE_PLATFORM);
        sendJson.setVersion(TUIGiftConstants.VALUE_VERSION);

        TUIGiftJson.Data data = new TUIGiftJson.Data();
        data.setMessage(giftModel.giveDesc);
        data.setLottieUrl(giftModel.animationUrl);
        data.setImageUrl(giftModel.normalImageUrl);
        data.setGiftId(giftModel.giftId);

        //扩展信息
        TUIGiftJson.Data.ExtInfo extInfo = new TUIGiftJson.Data.ExtInfo();
        extInfo.setUserID(TUILogin.getUserId());
        extInfo.setNickName(TUILogin.getNickName());
        extInfo.setAvatarUrl(TUILogin.getFaceUrl());

        data.setExtInfo(extInfo);
        sendJson.setData(data);

        Gson gson = new Gson();
        return gson.toJson(sendJson);
    }

    /**
     * 将点赞信息转换成JSON串
     *
     * @return 转换好的JSON串
     */
    private static String getCusLikeMsgJsonStr() {
        TUIGiftJson sendJson = new TUIGiftJson();
        sendJson.setBusinessID(TUIGiftConstants.VALUE_BUSINESS_ID_LIKE);
        sendJson.setPlatform(TUIGiftConstants.VALUE_PLATFORM);
        sendJson.setVersion(TUIGiftConstants.VALUE_VERSION);

        //扩展信息
        TUIGiftJson.Data.ExtInfo extInfo = new TUIGiftJson.Data.ExtInfo();
        extInfo.setUserID(TUILogin.getUserId());
        extInfo.setNickName(TUILogin.getNickName());
        extInfo.setAvatarUrl(TUILogin.getFaceUrl());

        TUIGiftJson.Data data = new TUIGiftJson.Data();
        data.setExtInfo(extInfo);
        sendJson.setData(data);

        Gson gson = new Gson();
        return gson.toJson(sendJson);
    }
}
