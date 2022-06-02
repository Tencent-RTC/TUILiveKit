package com.tencent.qcloud.tuikit.tuigift.core;

import android.content.Context;

import com.tencent.qcloud.tuicore.interfaces.ITUIExtension;
import com.tencent.qcloud.tuikit.tuigift.view.TUIGiftButton;
import com.tencent.qcloud.tuikit.tuigift.view.TUIGiftPlayView;
import com.tencent.qcloud.tuikit.tuigift.view.TUILikeButton;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;

/**
 * 礼物组件注册TUICore后,获取TUICore传入的用户组ID(房间ID),并绑定自己的布局文件;
 * 通过礼物面板点击礼物进行发送,礼物动画播放面板可以播放所接收到礼物的动画。
 */
public class TUIGiftExtension implements ITUIExtension {

    public static final String OBJECT_TUI_GIFT    = TUIGiftExtension.class.getName();
    public static final String KEY_EXTENSION_VIEW = "TUIExtensionView";
    public static final String KEY_PLAY_VIEW      = "TUIGiftPlayView";
    public static final String KEY_LIKE_BUTTON    = "TUILikeButton";
    public static final String KEY_TYPE_PLAY      = "play";
    public static final String KEY_TYPE_PANEL     = "panel";
    public static final String KEY_TYPE_PLUG      = "plug";

    public static Map<String, WeakReference<Object>> map = new HashMap<>();

    @Override
    public Map<String, Object> onGetExtensionInfo(String key, Map<String, Object> param) {
        //这个HashMap需携带返回给TUICore的View数据
        HashMap<String, Object> hashMap = new HashMap<>();

        if (OBJECT_TUI_GIFT.equals(key)) {
            Context context = (Context) param.get("context");
            String groupId = (String) param.get("groupId");
            TUIGiftButton giftButton = new TUIGiftButton(context, groupId);
            TUILikeButton likeButton = new TUILikeButton(context, groupId);
            TUIGiftPlayView playView = new TUIGiftPlayView(context, groupId);
            map.put(groupId + KEY_TYPE_PLAY, new WeakReference<Object>(playView));
            hashMap.put(KEY_EXTENSION_VIEW, giftButton);
            hashMap.put(KEY_LIKE_BUTTON, likeButton);
            hashMap.put(KEY_PLAY_VIEW, playView);
            return hashMap;
        }
        return null;
    }
}
