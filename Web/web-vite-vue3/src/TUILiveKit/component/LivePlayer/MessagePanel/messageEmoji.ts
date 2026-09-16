/**
 * IM emoji map for the live-chat message rail.
 *
 * The Tencent IM SDK ships text-based emoji as inline placeholders like
 * `[TUIEmoji_Smile]` inside a message's `textContent`. To render them we
 * need to translate each placeholder into the corresponding image URL on
 * the CDN. This mapping is a stable, public contract documented at:
 *   https://cloud.tencent.com/document/product/647/123836
 *
 * The map is intentionally duplicated here (rather than imported from a
 * deep path inside `tuikit-atomicx-vue3`) because:
 * 1. The SDK does not expose these constants from its public entry point.
 * 2. Reaching into `.../components/BarrageList/Message/TextMessage/emoji`
 *    would couple this demo to internal file paths that can move between
 *    SDK versions.
 * 3. The token → filename convention is documented Tencent Cloud protocol,
 *    so keeping our own copy is the low-risk, upstream-neutral choice.
 */

const EMOJI_BASE_URL = 'https://web.sdk.qcloud.com/im/assets/emoji-plugin/';

const EMOJI_URL_MAP: Record<string, string> = {
  '[TUIEmoji_Expect]': 'emoji_0@2x.png',
  '[TUIEmoji_Blink]': 'emoji_1@2x.png',
  '[TUIEmoji_Guffaw]': 'emoji_2@2x.png',
  '[TUIEmoji_KindSmile]': 'emoji_3@2x.png',
  '[TUIEmoji_Haha]': 'emoji_4@2x.png',
  '[TUIEmoji_Cheerful]': 'emoji_5@2x.png',
  '[TUIEmoji_Smile]': 'emoji_6@2x.png',
  '[TUIEmoji_Sorrow]': 'emoji_7@2x.png',
  '[TUIEmoji_Speechless]': 'emoji_8@2x.png',
  '[TUIEmoji_Amazed]': 'emoji_9@2x.png',
  '[TUIEmoji_Complacent]': 'emoji_10@2x.png',
  '[TUIEmoji_Lustful]': 'emoji_11@2x.png',
  '[TUIEmoji_Stareyes]': 'emoji_12@2x.png',
  '[TUIEmoji_Giggle]': 'emoji_13@2x.png',
  '[TUIEmoji_Daemon]': 'emoji_14@2x.png',
  '[TUIEmoji_Rage]': 'emoji_15@2x.png',
  '[TUIEmoji_Yawn]': 'emoji_16@2x.png',
  '[TUIEmoji_TearsLaugh]': 'emoji_17@2x.png',
  '[TUIEmoji_Silly]': 'emoji_18@2x.png',
  '[TUIEmoji_Wail]': 'emoji_19@2x.png',
  '[TUIEmoji_Kiss]': 'emoji_20@2x.png',
  '[TUIEmoji_Trapped]': 'emoji_21@2x.png',
  '[TUIEmoji_Fear]': 'emoji_22@2x.png',
  '[TUIEmoji_BareTeeth]': 'emoji_23@2x.png',
  '[TUIEmoji_FlareUp]': 'emoji_24@2x.png',
  '[TUIEmoji_Tact]': 'emoji_25@2x.png',
  '[TUIEmoji_Shit]': 'emoji_26@2x.png',
  '[TUIEmoji_ShutUp]': 'emoji_27@2x.png',
  '[TUIEmoji_Sigh]': 'emoji_28@2x.png',
  '[TUIEmoji_Hehe]': 'emoji_29@2x.png',
  '[TUIEmoji_Silent]': 'emoji_30@2x.png',
  '[TUIEmoji_Skull]': 'emoji_31@2x.png',
  '[TUIEmoji_Mask]': 'emoji_32@2x.png',
  '[TUIEmoji_Beer]': 'emoji_33@2x.png',
  '[TUIEmoji_Cake]': 'emoji_34@2x.png',
  '[TUIEmoji_RedPacket]': 'emoji_35@2x.png',
  '[TUIEmoji_Bombs]': 'emoji_36@2x.png',
  '[TUIEmoji_Ai]': 'emoji_37@2x.png',
  '[TUIEmoji_Celebrate]': 'emoji_38@2x.png',
  '[TUIEmoji_Bless]': 'emoji_39@2x.png',
  '[TUIEmoji_Flower]': 'emoji_40@2x.png',
  '[TUIEmoji_Watermelon]': 'emoji_41@2x.png',
  '[TUIEmoji_Cow]': 'emoji_42@2x.png',
  '[TUIEmoji_Fool]': 'emoji_43@2x.png',
  '[TUIEmoji_Surprised]': 'emoji_44@2x.png',
  '[TUIEmoji_Askance]': 'emoji_45@2x.png',
  '[TUIEmoji_Monster]': 'emoji_46@2x.png',
  '[TUIEmoji_Pig]': 'emoji_47@2x.png',
  '[TUIEmoji_Coffee]': 'emoji_48@2x.png',
  '[TUIEmoji_Ok]': 'emoji_49@2x.png',
  '[TUIEmoji_Heart]': 'emoji_50@2x.png',
  '[TUIEmoji_Sun]': 'emoji_51@2x.png',
  '[TUIEmoji_Moon]': 'emoji_52@2x.png',
  '[TUIEmoji_Star]': 'emoji_53@2x.png',
  '[TUIEmoji_Rich]': 'emoji_54@2x.png',
  '[TUIEmoji_Fortune]': 'emoji_55@2x.png',
  '[TUIEmoji_857]': 'emoji_56@2x.png',
  '[TUIEmoji_666]': 'emoji_57@2x.png',
  '[TUIEmoji_Prohibit]': 'emoji_58@2x.png',
  '[TUIEmoji_Convinced]': 'emoji_59@2x.png',
  '[TUIEmoji_Knife]': 'emoji_60@2x.png',
  '[TUIEmoji_Like]': 'emoji_61@2x.png',
};

export { EMOJI_BASE_URL, EMOJI_URL_MAP };
