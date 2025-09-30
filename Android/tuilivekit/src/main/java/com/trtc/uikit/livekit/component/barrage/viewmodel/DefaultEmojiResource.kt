package com.trtc.uikit.livekit.component.barrage.store.model

import android.content.Context
import android.graphics.Color
import android.graphics.Rect
import android.graphics.drawable.Drawable
import android.text.TextUtils
import androidx.core.content.res.ResourcesCompat
import androidx.core.graphics.drawable.toDrawable
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.component.barrage.viewmodel.IEmojiResource

class DefaultEmojiResource : IEmojiResource {

    private val resource = linkedMapOf(
        R.drawable.live_barrage_emoji_0 to "[TUIEmoji_Smile]",
        R.drawable.live_barrage_emoji_1 to "[TUIEmoji_Expect]",
        R.drawable.live_barrage_emoji_2 to "[TUIEmoji_Blink]",
        R.drawable.live_barrage_emoji_3 to "[TUIEmoji_Guffaw]",
        R.drawable.live_barrage_emoji_4 to "[TUIEmoji_KindSmile]",
        R.drawable.live_barrage_emoji_5 to "[TUIEmoji_Haha]",
        R.drawable.live_barrage_emoji_6 to "[TUIEmoji_Cheerful]",
        R.drawable.live_barrage_emoji_7 to "[TUIEmoji_Speechless]",
        R.drawable.live_barrage_emoji_8 to "[TUIEmoji_Amazed]",
        R.drawable.live_barrage_emoji_9 to "[TUIEmoji_Sorrow]",
        R.drawable.live_barrage_emoji_10 to "[TUIEmoji_Complacent]",
        R.drawable.live_barrage_emoji_11 to "[TUIEmoji_Silly]",
        R.drawable.live_barrage_emoji_12 to "[TUIEmoji_Lustful]",
        R.drawable.live_barrage_emoji_13 to "[TUIEmoji_Giggle]",
        R.drawable.live_barrage_emoji_14 to "[TUIEmoji_Kiss]",
        R.drawable.live_barrage_emoji_15 to "[TUIEmoji_Wail]",
        R.drawable.live_barrage_emoji_16 to "[TUIEmoji_TearsLaugh]",
        R.drawable.live_barrage_emoji_17 to "[TUIEmoji_Trapped]",
        R.drawable.live_barrage_emoji_18 to "[TUIEmoji_Mask]",
        R.drawable.live_barrage_emoji_19 to "[TUIEmoji_Fear]",
        R.drawable.live_barrage_emoji_20 to "[TUIEmoji_BareTeeth]",
        R.drawable.live_barrage_emoji_21 to "[TUIEmoji_FlareUp]",
        R.drawable.live_barrage_emoji_22 to "[TUIEmoji_Yawn]",
        R.drawable.live_barrage_emoji_23 to "[TUIEmoji_Tact]",
        R.drawable.live_barrage_emoji_24 to "[TUIEmoji_Stareyes]",
        R.drawable.live_barrage_emoji_25 to "[TUIEmoji_ShutUp]",
        R.drawable.live_barrage_emoji_26 to "[TUIEmoji_Sigh]",
        R.drawable.live_barrage_emoji_27 to "[TUIEmoji_Hehe]",
        R.drawable.live_barrage_emoji_28 to "[TUIEmoji_Silent]",
        R.drawable.live_barrage_emoji_29 to "[TUIEmoji_Surprised]",
        R.drawable.live_barrage_emoji_30 to "[TUIEmoji_Askance]",
        R.drawable.live_barrage_emoji_31 to "[TUIEmoji_Ok]",
        R.drawable.live_barrage_emoji_32 to "[TUIEmoji_Shit]",
        R.drawable.live_barrage_emoji_33 to "[TUIEmoji_Monster]",
        R.drawable.live_barrage_emoji_34 to "[TUIEmoji_Daemon]",
        R.drawable.live_barrage_emoji_35 to "[TUIEmoji_Rage]",
        R.drawable.live_barrage_emoji_36 to "[TUIEmoji_Fool]",
        R.drawable.live_barrage_emoji_37 to "[TUIEmoji_Pig]",
        R.drawable.live_barrage_emoji_38 to "[TUIEmoji_Cow]",
        R.drawable.live_barrage_emoji_39 to "[TUIEmoji_Ai]",
        R.drawable.live_barrage_emoji_40 to "[TUIEmoji_Skull]",
        R.drawable.live_barrage_emoji_41 to "[TUIEmoji_Bombs]",
        R.drawable.live_barrage_emoji_42 to "[TUIEmoji_Coffee]",
        R.drawable.live_barrage_emoji_43 to "[TUIEmoji_Cake]",
        R.drawable.live_barrage_emoji_44 to "[TUIEmoji_Beer]",
        R.drawable.live_barrage_emoji_45 to "[TUIEmoji_Flower]",
        R.drawable.live_barrage_emoji_46 to "[TUIEmoji_Watermelon]",
        R.drawable.live_barrage_emoji_47 to "[TUIEmoji_Rich]",
        R.drawable.live_barrage_emoji_48 to "[TUIEmoji_Heart]",
        R.drawable.live_barrage_emoji_49 to "[TUIEmoji_Moon]",
        R.drawable.live_barrage_emoji_50 to "[TUIEmoji_Sun]",
        R.drawable.live_barrage_emoji_51 to "[TUIEmoji_Star]",
        R.drawable.live_barrage_emoji_52 to "[TUIEmoji_RedPacket]",
        R.drawable.live_barrage_emoji_53 to "[TUIEmoji_Celebrate]",
        R.drawable.live_barrage_emoji_54 to "[TUIEmoji_Bless]",
        R.drawable.live_barrage_emoji_55 to "[TUIEmoji_Fortune]",
        R.drawable.live_barrage_emoji_56 to "[TUIEmoji_Convinced]",
        R.drawable.live_barrage_emoji_57 to "[TUIEmoji_Prohibit]",
        R.drawable.live_barrage_emoji_58 to "[TUIEmoji_666]",
        R.drawable.live_barrage_emoji_59 to "[TUIEmoji_857]",
        R.drawable.live_barrage_emoji_60 to "[TUIEmoji_Knife]",
        R.drawable.live_barrage_emoji_61 to "[TUIEmoji_Like]"
    )

    override fun getResId(key: String): Int {
        resource.forEach { (resId, value) ->
            if (TextUtils.equals(key, value)) {
                return resId
            }
        }
        return 0
    }

    override fun getResIds(): List<Int> = resource.keys.toList()

    override fun getEncodeValue(resId: Int): String = resource[resId] ?: ""

    override fun getEncodePattern(): String = "\\[TUIEmoji_[a-zA-Z0-9_]+\\]"

    override fun getDrawable(context: Context, resId: Int, bounds: Rect?): Drawable {
        return ResourcesCompat.getDrawable(context.resources, resId, null)?.apply {
            bounds?.let { setBounds(it.left, it.top, it.right, it.bottom) }
        } ?: Color.TRANSPARENT.toDrawable()
    }
}
