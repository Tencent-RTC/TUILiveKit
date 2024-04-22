package com.trtc.uikit.livekit.common.uicomponent.barrage.model;

import android.content.Context;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;

import androidx.core.content.res.ResourcesCompat;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.service.IEmojiResource;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public final class DefaultEmojiResource implements IEmojiResource {

    private final Map<Integer, String> resource = new LinkedHashMap<>();

    public DefaultEmojiResource() {
        resource.put(R.drawable.live_barrage_emoji_0, "[微笑]");
        resource.put(R.drawable.live_barrage_emoji_1, "[期待]");
        resource.put(R.drawable.live_barrage_emoji_2, "[眨眼]");
        resource.put(R.drawable.live_barrage_emoji_3, "[大笑]");
        resource.put(R.drawable.live_barrage_emoji_4, "[姨母笑]");
        resource.put(R.drawable.live_barrage_emoji_5, "[哈哈哈]");
        resource.put(R.drawable.live_barrage_emoji_6, "[愉快]");
        resource.put(R.drawable.live_barrage_emoji_7, "[无语]");
        resource.put(R.drawable.live_barrage_emoji_8, "[惊讶]");
        resource.put(R.drawable.live_barrage_emoji_9, "[悲伤]");
        resource.put(R.drawable.live_barrage_emoji_10, "[得意]");
        resource.put(R.drawable.live_barrage_emoji_11, "[傻了]");
        resource.put(R.drawable.live_barrage_emoji_12, "[色]");
        resource.put(R.drawable.live_barrage_emoji_13, "[憨笑]");
        resource.put(R.drawable.live_barrage_emoji_14, "[亲亲]");
        resource.put(R.drawable.live_barrage_emoji_15, "[大哭]");
        resource.put(R.drawable.live_barrage_emoji_16, "[哭笑]");
        resource.put(R.drawable.live_barrage_emoji_17, "[困]");
        resource.put(R.drawable.live_barrage_emoji_18, "[口罩]");
        resource.put(R.drawable.live_barrage_emoji_19, "[恐惧]");
        resource.put(R.drawable.live_barrage_emoji_20, "[龇牙]");
        resource.put(R.drawable.live_barrage_emoji_21, "[发怒]");
        resource.put(R.drawable.live_barrage_emoji_22, "[打哈欠]");
        resource.put(R.drawable.live_barrage_emoji_23, "[机智]");
        resource.put(R.drawable.live_barrage_emoji_24, "[星星眼]");
        resource.put(R.drawable.live_barrage_emoji_25, "[闭嘴]");
        resource.put(R.drawable.live_barrage_emoji_26, "[叹气]");
        resource.put(R.drawable.live_barrage_emoji_27, "[呵呵]");
        resource.put(R.drawable.live_barrage_emoji_28, "[收声]");
        resource.put(R.drawable.live_barrage_emoji_29, "[惊喜]");
        resource.put(R.drawable.live_barrage_emoji_30, "[白眼]");
        resource.put(R.drawable.live_barrage_emoji_31, "[OK]");
        resource.put(R.drawable.live_barrage_emoji_32, "[便便]");
        resource.put(R.drawable.live_barrage_emoji_33, "[怪兽]");
        resource.put(R.drawable.live_barrage_emoji_34, "[恶魔]");
        resource.put(R.drawable.live_barrage_emoji_35, "[恶魔怒]");
        resource.put(R.drawable.live_barrage_emoji_36, "[衰]");
        resource.put(R.drawable.live_barrage_emoji_37, "[猪]");
        resource.put(R.drawable.live_barrage_emoji_38, "[牛]");
        resource.put(R.drawable.live_barrage_emoji_39, "[AI]");
        resource.put(R.drawable.live_barrage_emoji_40, "[骷髅]");
        resource.put(R.drawable.live_barrage_emoji_41, "[炸弹]");
        resource.put(R.drawable.live_barrage_emoji_42, "[咖啡]");
        resource.put(R.drawable.live_barrage_emoji_43, "[蛋糕]");
        resource.put(R.drawable.live_barrage_emoji_44, "[啤酒]");
        resource.put(R.drawable.live_barrage_emoji_45, "[花]");
        resource.put(R.drawable.live_barrage_emoji_46, "[瓜]");
        resource.put(R.drawable.live_barrage_emoji_47, "[壕]");
        resource.put(R.drawable.live_barrage_emoji_48, "[爱心]");
        resource.put(R.drawable.live_barrage_emoji_49, "[月亮]");
        resource.put(R.drawable.live_barrage_emoji_50, "[太阳]");
        resource.put(R.drawable.live_barrage_emoji_51, "[星星]");
        resource.put(R.drawable.live_barrage_emoji_52, "[红包]");
        resource.put(R.drawable.live_barrage_emoji_53, "[庆祝]");
        resource.put(R.drawable.live_barrage_emoji_54, "[福]");
        resource.put(R.drawable.live_barrage_emoji_55, "[发]");
        resource.put(R.drawable.live_barrage_emoji_56, "[服]");
        resource.put(R.drawable.live_barrage_emoji_57, "[禁]");
        resource.put(R.drawable.live_barrage_emoji_58, "[666]");
        resource.put(R.drawable.live_barrage_emoji_59, "[857]");
        resource.put(R.drawable.live_barrage_emoji_60, "[刀]");
        resource.put(R.drawable.live_barrage_emoji_61, "[赞]");
    }

    public int getResId(String key) {
        for (Map.Entry<Integer, String> entry : resource.entrySet()) {
            if (TextUtils.equals(key, entry.getValue())) {
                return entry.getKey().intValue();
            }
        }
        return 0;
    }

    public List<Integer> getResIds() {
        List<Integer> list = new ArrayList<>();
        list.addAll(resource.keySet());
        return list;
    }

    public String getEncodeValue(int resId) {
        return resource.get(resId);
    }

    @Override
    public String getEncodePattern() {
        return "\\[(.*?)\\]";
    }

    @Override
    public Drawable getDrawable(Context context, int resId, Rect bounds) {
        Drawable drawable = ResourcesCompat.getDrawable(context.getResources(), resId, null);
        if (drawable != null && bounds != null) {
            drawable.setBounds(bounds.left, bounds.top, bounds.right, bounds.bottom);
        }
        return drawable;
    }
}
