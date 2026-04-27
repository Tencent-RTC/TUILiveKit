<template>
  <div class="education-side-panel">
    <!-- Tab navigation -->
    <nav class="panel-tabs">
      <button
        v-for="tab in tabs"
        :key="tab.id"
        class="tab-btn"
        :class="{ active: activeTab === tab.id }"
        @click="activeTab = tab.id"
      >
        <svg class="tab-icon" viewBox="0 0 24 24" fill="none">
          <template v-if="tab.id === 'chat'">
            <path d="M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z" />
          </template>
          <template v-else>
            <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" />
            <circle cx="9" cy="7" r="4" />
            <path d="M23 21v-2a4 4 0 0 0-3-3.87" />
            <path d="M16 3.13a4 4 0 0 1 0 7.75" />
          </template>
        </svg>
        <span>{{ t(tab.label) }}</span>
      </button>
    </nav>

    <!-- Tab panels -->
    <div class="panel-body">
      <!-- Chat tab -->
      <div v-show="activeTab === 'chat'" class="tab-panel chat-panel">
        <div ref="chatListRef" class="chat-list" @scroll="handleChatScroll">
          <template v-for="item in chatTimeline" :key="item.key">
            <div v-if="item.type === 'system'" class="system-message" :class="`system-message-${item.action}`">
              <span class="system-message-text">{{ item.label }}</span>
            </div>
            <div v-else class="chat-message">
              <Avatar
                class="chat-avatar"
                :src="getMessageAvatar(item.msg.sender)"
                :size="24"
                :alt="item.msg.sender.userName || item.msg.sender.userId"
                :style="getMessageAvatarStyle(item.msg.sender)"
              />
              <div class="chat-bubble">
                <span class="chat-user">{{ item.msg.sender.userName || item.msg.sender.userId }}</span>
                <span class="chat-text">
                  <template v-if="isGiftMessage(item.msg)">
                    <template v-if="getGiftPayloadByMessage(item.msg)">
                      <span class="gift-content">
                        <span class="gift-prefix">{{ `${t('BarrageList.SendGift')} ` }}</span>
                        <span
                          v-if="getGiftPayloadByMessage(item.msg)?.giftName"
                          class="gift-name"
                          :style="getGiftNameStyle(getGiftPayloadByMessage(item.msg)?.giftName || '')"
                        >{{ `${getGiftPayloadByMessage(item.msg)?.giftName} ` }}</span>
                        <img
                          v-if="getGiftPayloadByMessage(item.msg)?.iconUrl"
                          class="gift-icon"
                          :src="getGiftPayloadByMessage(item.msg)?.iconUrl"
                          :alt="getGiftPayloadByMessage(item.msg)?.giftName || ''"
                        >
                      </span>
                    </template>
                    <template v-else>
                      {{ t('BarrageList.SendGift') }}
                    </template>
                  </template>
                  <template v-else>
                    <template
                      v-for="(segment, segmentIndex) in getMessageContent(item.msg.textContent || '')"
                      :key="`${getMessageKey(item.msg)}-${segmentIndex}`"
                    >
                      <span v-if="segment.type === 'text'">{{ segment.value }}</span>
                      <img
                        v-else
                        class="msg-emoji"
                        :src="segment.value"
                        :alt="segment.emojiKey || ''"
                      >
                    </template>
                  </template>
                </span>
              </div>
            </div>
          </template>
          <div v-if="!chatTimeline.length" class="empty-state">
            <svg class="empty-icon" viewBox="0 0 24 24" fill="none">
              <path d="M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z" />
            </svg>
            <p>{{ t('No messages yet') }}</p>
            <span>{{ t('Start the conversation') }}</span>
          </div>
        </div>
      </div>

      <!-- Members tab -->
      <div v-show="activeTab === 'members'" class="tab-panel members-panel">
        <div class="members-list">
          <!-- Instructor -->
          <div class="member-item instructor">
            <Avatar
              class="member-avatar instructor-avatar"
              :src="getHostAvatar()"
              :size="28"
              :alt="hostName"
              :style="getHostAvatarStyle()"
            />
            <div class="member-info">
              <span class="member-name">{{ hostName }}</span>
            </div>
            <span class="member-status-dot online" />
          </div>
          <!-- Audience -->
          <div v-for="member in audienceMembers" :key="member.id" class="member-item">
            <Avatar
              class="member-avatar"
              :src="member.avatar"
              :size="28"
              :alt="member.name"
              :style="getAudienceAvatarStyle(member)"
            />
            <div class="member-info">
              <span class="member-name">{{ member.name }}</span>
              <span v-if="member.onSeat" class="member-role on-seat">{{ t('On Stage') }}</span>
            </div>
            <span class="member-status-dot" :class="member.online ? 'online' : 'offline'" />
          </div>
          <div v-if="!audienceMembers.length" class="empty-state">
            <p>{{ t('No audience yet') }}</p>
          </div>
        </div>
      </div>
    </div>

    <!-- Bottom input using BarrageInput component (only show in chat tab) -->
    <div v-show="activeTab === 'chat'" class="panel-input" :class="{ disabled: isInputDisabled }">
      <div class="input-wrapper" :class="{ focused: inputFocused, disabled: isInputDisabled }">
        <BarrageInput
          class="edu-barrage-input"
          :auto-focus="false"
          :disabled="isInputDisabled"
          :placeholder="inputPlaceholder"
          :on-will-send-barrage="handleWillSendBarrage"
          :on-did-send-barrage="handleDidSendBarrage"
          @focus="handleInputFocus"
          @blur="handleInputBlur"
        />
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, ref, watch } from 'vue';
import {
  useBarrageState,
  useLiveListState,
  useLoginState,
  BarrageInput,
  useLiveAudienceState,
  Avatar,
  useRoomEngine,
  useLiveGiftState,
  LiveGiftEvents,
  BarrageType,
} from 'tuikit-atomicx-vue3';
import { TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { TUIRoomEvents } from '@tencentcloud/tuiroom-engine-js';
import type { AudienceInfo, Barrage } from 'tuikit-atomicx-vue3';

interface AudienceMember {
  id: string;
  name: string;
  avatar: string;
  online: boolean;
  onSeat: boolean;
}

type MessageSegment = {
  type: 'text' | 'emoji';
  value: string;
  emojiKey?: string;
};

const BASIC_EMOJI_BASE_URL = 'https://web.sdk.qcloud.com/im/assets/emoji-plugin/';
const BASIC_EMOJI_URL_MAPPING: Record<string, string> = {
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

const props = withDefaults(defineProps<{ liveEnded?: boolean }>(), {
  liveEnded: false,
});

const { t } = useUIKit();
const { messageList, appendLocalTip } = useBarrageState();
const { currentLive } = useLiveListState();
const { loginUserInfo } = useLoginState();
const { audienceList } = useLiveAudienceState();
const { subscribeEvent: subscribeGiftEvent, unsubscribeEvent: unsubscribeGiftEvent } = useLiveGiftState();
const roomEngine = useRoomEngine();

const activeTab = ref<'chat' | 'members'>('chat');
const inputFocused = ref(false);
const chatListRef = ref<HTMLElement | null>(null);
const shouldStickToBottom = ref(true);
const systemTips = ref<SystemTip[]>([]);
const MAX_SYSTEM_TIPS = 200;

// Computed: filter messages with text content
const displayMessages = computed(() => messageList.value
  .filter((m: Barrage) => (m.textContent && m.textContent.length > 0) || isGiftMessage(m))
  .map((m: Barrage) => ({ ...m })));

// Check if current user is muted
const localAudience = computed(() => audienceList.value.find(item => item.userId === loginUserInfo.value?.userId));
const isMessageMuted = computed(() => !!localAudience.value?.isMessageDisabled);
const isInputDisabled = computed(() => props.liveEnded || isMessageMuted.value);
watch(isMessageMuted, (newVal, oldVal) => {
  if (oldVal === undefined) return;
  if (newVal && !oldVal) {
    TUIToast.info({ message: t('You have been muted in this room') });
  }
  if (!newVal && oldVal) {
    TUIToast.info({ message: t('You have been unmuted in this room') });
  }
});

const tabs = computed(() => [
  {
    id: 'chat' as const,
    label: 'Barrage list',
  },
  {
    id: 'members' as const,
    label: 'Online viewers',
  },
]);

const hostName = computed(() => currentLive.value?.liveOwner.userName || currentLive.value?.liveOwner.userId || t('Instructor'));

const inputPlaceholder = computed(() => {
  if (props.liveEnded) return t('Live has ended');
  if (isMessageMuted.value) return t('You have been muted in this room');
  return t('Type a message...');
});

const audienceMembers = computed<AudienceMember[]>(() => audienceList.value
  .filter((item: AudienceInfo) => item.userId !== currentLive.value?.liveOwner.userId)
  .map((item: AudienceInfo) => ({
    id: item.userId,
    name: item.userName || item.userId,
    avatar: getRawAvatar(item),
    online: true,
    onSeat: !!((item as AudienceInfo & { onSeat?: boolean; isOnSeat?: boolean }).onSeat
      || (item as AudienceInfo & { onSeat?: boolean; isOnSeat?: boolean }).isOnSeat),
  })));

const LIGHT_PALETTE = [
  { bg: '#eff6ff', text: '#2563eb' },
  { bg: '#f0fdfa', text: '#0d9488' },
  { bg: '#fffbeb', text: '#d97706' },
  { bg: '#fff1f2', text: '#f43f5e' },
  { bg: '#f0f9ff', text: '#0284c7' },
  { bg: '#f1f5f9', text: '#475569' },
  { bg: '#ecfdf5', text: '#059669' },
  { bg: '#fff7ed', text: '#ea580c' },
];

type AvatarLikeUser = {
  userId?: string;
  userName?: string;
  avatarUrl?: string;
  avatar?: string;
  avatarURL?: string;
};

type SystemTip = {
  key: string;
  label: string;
  timestampInSecond: number;
  action: 'enter' | 'leave';
};

type ChatTimelineItem =
  | { type: 'system'; key: string; label: string; action: 'enter' | 'leave' }
  | { type: 'message'; key: string; msg: Barrage };

type GiftPayload = {
  giftName: string;
  iconUrl: string;
};

const audienceAvatarByUserId = computed(() => {
  const map = new Map<string, string>();
  audienceList.value.forEach((viewer: AudienceInfo) => {
    const avatar = getRawAvatar(viewer);
    if (!viewer.userId || !avatar || map.has(viewer.userId)) return;
    map.set(viewer.userId, avatar);
  });
  return map;
});

const chatTimeline = computed<ChatTimelineItem[]>(() => {
  const mergedItems = [
    ...displayMessages.value.map(msg => ({
      type: 'message' as const,
      ts: msg.timestampInSecond || 0,
      key: getMessageKey(msg),
      msg,
    })),
    ...systemTips.value.map(item => ({
      type: 'system' as const,
      ts: item.timestampInSecond || 0,
      key: item.key,
      label: item.label,
      action: item.action,
    })),
  ].sort((a, b) => a.ts - b.ts);

  return mergedItems.map((item) => {
    if (item.type === 'system') {
      return {
        type: 'system' as const,
        key: item.key,
        label: item.label,
        action: item.action,
      };
    }
    return {
      type: 'message' as const,
      key: item.key,
      msg: item.msg,
    };
  });
});

function hashName(name: string): number {
  let hash = 0;
  for (let i = 0; i < name.length; i += 1) {
    hash = ((hash << 5) - hash + name.charCodeAt(i)) | 0;
  }
  return Math.abs(hash);
}

function getAvatarColor(name: string) {
  const idx = hashName(name) % LIGHT_PALETTE.length;
  return LIGHT_PALETTE[idx];
}

function getRawAvatar(user?: AvatarLikeUser | null): string {
  if (!user) return '';
  const candidate = user.avatarUrl || user.avatar || user.avatarURL || '';
  return typeof candidate === 'string' ? candidate.trim() : '';
}

function getHostAvatar() {
  return getRawAvatar(currentLive.value?.liveOwner as AvatarLikeUser);
}

function getHostAvatarStyle() {
  if (getHostAvatar()) return {};
  return {
    background: 'var(--edu-primary)',
    color: 'var(--edu-text-on-primary)',
  };
}

function getAudienceAvatarStyle(member: AudienceMember) {
  if (member.avatar) return {};
  const color = getAvatarColor(member.name);
  return {
    background: color.bg,
    color: color.text,
  };
}

function getMessageAvatar(sender: AvatarLikeUser) {
  const directAvatar = getRawAvatar(sender);
  if (directAvatar) return directAvatar;
  if (sender.userId && audienceAvatarByUserId.value.has(sender.userId)) {
    return audienceAvatarByUserId.value.get(sender.userId) || '';
  }
  if (sender.userId && sender.userId === currentLive.value?.liveOwner?.userId) {
    return getHostAvatar();
  }
  if (sender.userId && sender.userId === loginUserInfo.value?.userId) {
    return getRawAvatar(loginUserInfo.value as AvatarLikeUser);
  }
  return '';
}

function getMessageAvatarStyle(sender: AvatarLikeUser) {
  if (getMessageAvatar(sender)) return {};
  if (sender.userId && sender.userId === currentLive.value?.liveOwner?.userId) {
    return getHostAvatarStyle();
  }
  const color = getAvatarColor(sender.userName || sender.userId || '');
  return {
    background: color.bg,
    color: color.text,
  };
}

function getUserDisplayName(userInfo: { userId?: string; userName?: string; nameCard?: string }) {
  return userInfo.nameCard || userInfo.userName || userInfo.userId || '';
}

function appendSystemTip(action: 'enter' | 'leave', userInfo: { userId?: string; userName?: string; nameCard?: string }) {
  if (!userInfo?.userId) return;
  const name = getUserDisplayName(userInfo);
  if (!name) return;
  const suffix = action === 'enter' ? t('BarrageList.ComeIn') : t('BarrageList.Leave');
  const next = [...systemTips.value, {
    key: `sys-${action}-${userInfo.userId}-${Date.now()}`,
    label: `${name} ${suffix}`,
    timestampInSecond: Math.floor(Date.now() / 1000),
    action,
  }];
  systemTips.value = next.slice(-MAX_SYSTEM_TIPS);
}

function handleRemoteUserEnterRoom(eventInfo: { userInfo: { userId?: string; userName?: string; nameCard?: string } }) {
  appendSystemTip('enter', eventInfo.userInfo);
}

function handleRemoteUserLeaveRoom(eventInfo: { userInfo: { userId?: string; userName?: string; nameCard?: string } }) {
  appendSystemTip('leave', eventInfo.userInfo);
}

function getMessageKey(msg: Barrage): string {
  const seq = typeof msg.sequence === 'number' ? String(msg.sequence) : '';
  if (seq) return `msg-${seq}`;
  const ts = typeof msg.timestampInSecond === 'number' ? String(msg.timestampInSecond) : '';
  if (ts) return `msg-${msg.sender.userId || 'unknown'}-${ts}`;
  return `msg-${msg.sender.userId || 'unknown'}-${msg.textContent || ''}-${Date.now()}`;
}

function getMessageCacheKey(message: Barrage): string {
  const seq = typeof message.sequence === 'number' ? String(message.sequence) : '';
  if (seq) return seq;
  const ts = typeof message.timestampInSecond === 'number' ? String(message.timestampInSecond) : '';
  if (ts) return `${message.sender.userId || 'unknown'}-${ts}`;
  return `${message.sender.userId || 'unknown'}-${message.businessId || 'message'}-${message.textContent || ''}`;
}

function isGiftMessage(message: Barrage): boolean {
  if (message.businessId === 'gift') return true;
  if (!message.data) return false;
  try {
    const parsed = JSON.parse(message.data) as { type?: string };
    return parsed.type === 'gift';
  } catch (_error) {
    return false;
  }
}

function getGiftPayload(message: Barrage): GiftPayload | null {
  if (!message.data) return null;
  try {
    const parsed = JSON.parse(message.data) as {
      type?: string;
      giftInfo?: { name?: string; iconUrl?: string };
      giftName?: string;
      giftIcon?: string;
      iconUrl?: string;
      name?: string;
    };
    if (parsed.type && parsed.type !== 'gift' && message.businessId !== 'gift') {
      return null;
    }
    const giftName = parsed.giftInfo?.name || parsed.giftName || parsed.name || '';
    const iconUrl = parsed.giftInfo?.iconUrl || parsed.iconUrl || parsed.giftIcon || '';
    if (!giftName && !iconUrl) return null;
    return { giftName, iconUrl };
  } catch (_error) {
    return null;
  }
}

const giftPayloadMap = computed<Record<string, GiftPayload>>(() => {
  const map: Record<string, GiftPayload> = {};
  displayMessages.value.forEach((message) => {
    if (!isGiftMessage(message)) return;
    const payload = getGiftPayload(message);
    if (!payload) return;
    map[getMessageCacheKey(message)] = payload;
  });
  return map;
});

function getGiftPayloadByMessage(message: Barrage): GiftPayload | null {
  return giftPayloadMap.value[getMessageCacheKey(message)] || null;
}

function getGiftNameStyle(giftName: string) {
  const normalized = giftName.trim();
  if (!normalized) return {};
  let hash = 0;
  for (let i = 0; i < normalized.length; i += 1) {
    hash = (hash * 31 + normalized.charCodeAt(i)) >>> 0;
  }
  const hue = hash % 360;
  const saturation = 72 + (hash % 12);
  const lightness = 58 + ((hash >> 3) % 8);
  return {
    color: `hsl(${hue} ${saturation}% ${lightness}%)`,
    textShadow: `0 0 12px hsl(${hue} ${saturation}% ${lightness}% / 0.18)`,
  };
}

function getMessageContent(text: string): MessageSegment[] {
  const segments: MessageSegment[] = [];
  let temp = text;

  while (temp) {
    const left = temp.indexOf('[');
    const right = temp.indexOf(']');
    if (left === 0) {
      if (right === -1) {
        segments.push({ type: 'text', value: temp });
        break;
      }
      const emojiKey = temp.slice(0, right + 1);
      const emojiAsset = BASIC_EMOJI_URL_MAPPING[emojiKey];
      if (emojiAsset) {
        segments.push({
          type: 'emoji',
          value: `${BASIC_EMOJI_BASE_URL}${emojiAsset}`,
          emojiKey,
        });
        temp = temp.substring(right + 1);
      } else {
        segments.push({ type: 'text', value: '[' });
        temp = temp.slice(1);
      }
      continue;
    }

    if (left === -1) {
      segments.push({ type: 'text', value: temp });
      break;
    }

    segments.push({ type: 'text', value: temp.slice(0, left) });
    temp = temp.substring(left);
  }

  return segments;
}

function handleInputFocus() {
  inputFocused.value = true;
}

function handleInputBlur() {
  inputFocused.value = false;
}

function handleWillSendBarrage() {
  if (isInputDisabled.value) return false;
  return true;
}

function handleDidSendBarrage() {
  // Scroll to bottom after sending
  nextTick(() => {
    scrollChatToBottom(true);
  });
}

function handleGiftMessage(gift: {
  liveId: string;
  giftCount: number;
  sender: Barrage['sender'];
  giftInfo: { name?: string; iconUrl?: string };
}) {
  const lastBarrage = messageList.value.at(-1);
  const sequence = lastBarrage ? lastBarrage.sequence + 1 : 1;
  const barrage: Barrage = {
    liveId: gift.liveId,
    sender: gift.sender,
    sequence,
    timestampInSecond: Math.floor(Date.now() / 1000),
    messageType: BarrageType.custom,
    textContent: '',
    extensionInfo: null,
    businessId: 'gift',
    data: JSON.stringify({
      type: 'gift',
      giftInfo: gift.giftInfo,
      count: gift.giftCount,
    }),
  };
  appendLocalTip(barrage);
}

function scrollChatToBottom(force = false) {
  if (!force && !shouldStickToBottom.value) return;
  nextTick(() => {
    if (chatListRef.value) {
      chatListRef.value.scrollTop = chatListRef.value.scrollHeight;
      shouldStickToBottom.value = true;
    }
  });
}

function handleChatScroll() {
  if (!chatListRef.value) return;
  const el = chatListRef.value;
  shouldStickToBottom.value = (el.scrollHeight - el.clientHeight - el.scrollTop) < 24;
}

watch(activeTab, (tab) => {
  if (tab !== 'chat') return;
  nextTick(() => {
    handleChatScroll();
    scrollChatToBottom(true);
  });
});

// Auto-scroll chat on new messages only when user is near the bottom
watch(() => chatTimeline.value.length, (currentLength, previousLength) => {
  if (currentLength <= (previousLength ?? 0)) return;
  scrollChatToBottom();
});

onMounted(() => {
  subscribeGiftEvent(LiveGiftEvents.ON_RECEIVE_GIFT_MESSAGE, handleGiftMessage);
  roomEngine.instance?.on(TUIRoomEvents.onRemoteUserEnterRoom, handleRemoteUserEnterRoom);
  roomEngine.instance?.on(TUIRoomEvents.onRemoteUserLeaveRoom, handleRemoteUserLeaveRoom);
});

onUnmounted(() => {
  unsubscribeGiftEvent(LiveGiftEvents.ON_RECEIVE_GIFT_MESSAGE, handleGiftMessage);
  roomEngine.instance?.off(TUIRoomEvents.onRemoteUserEnterRoom, handleRemoteUserEnterRoom);
  roomEngine.instance?.off(TUIRoomEvents.onRemoteUserLeaveRoom, handleRemoteUserLeaveRoom);
});
</script>

<style scoped lang="scss">
.education-side-panel {
  display: flex;
  flex-direction: column;
  height: 100%;
  background: var(--edu-bg-surface);
  overflow: hidden;
}

// === Tab navigation ===
.panel-tabs {
  display: flex;
  gap: 0;
  border-bottom: 1px solid var(--edu-border);
  flex-shrink: 0;
  background: var(--edu-bg-surface);
}

.tab-btn {
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 5px;
  height: 42px;
  padding: 0 8px;
  border: none;
  border-bottom: 2px solid transparent;
  background: none;
  color: var(--edu-text-tertiary);
  font-size: var(--edu-font-size-small);
  font-weight: 500;
  cursor: pointer;
  transition: all 160ms ease;
  white-space: nowrap;

  &:hover {
    color: var(--edu-text-secondary);
    background: var(--edu-bg-hover);
  }

  &.active {
    color: var(--edu-primary);
    border-bottom-color: var(--edu-primary);
    font-weight: 600;
  }
}

.tab-icon {
  width: 15px;
  height: 15px;
  stroke: currentColor;
  stroke-width: 1.8;
  stroke-linecap: round;
  stroke-linejoin: round;
  flex-shrink: 0;
}

// === Panel body ===
.panel-body {
  flex: 1;
  min-height: 0;
  overflow: hidden;
}

.tab-panel {
  height: 100%;
  overflow: hidden;
}

// === Chat tab ===
.chat-panel {
  display: flex;
  flex-direction: column;
}

.chat-list {
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 10px 12px;
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  scrollbar-width: thin;
  scrollbar-color: color-mix(in srgb, var(--edu-primary) 28%, var(--edu-bg-muted)) transparent;
}

.chat-message {
  display: flex;
  align-items: flex-start;
  gap: 8px;
  padding: 4px 0;
}

.chat-avatar {
  flex-shrink: 0;
  margin-top: 2px;
  overflow: hidden;
}

.chat-bubble {
  display: inline-flex;
  flex-direction: column;
  align-items: flex-start;
  font-size: var(--edu-font-size-caption);
  line-height: 1.5;
  color: var(--edu-text-primary);
}

.chat-user {
  font-weight: 600;
  color: var(--edu-primary);
  margin-right: 6px;
  font-size: var(--edu-font-size-small);
}

.chat-text {
  display: inline-block;
  color: var(--edu-text-primary);
  white-space: pre-wrap;
  word-break: break-word;
}

.msg-emoji {
  width: 18px;
  height: 18px;
  vertical-align: text-bottom;
  margin: 0 1px;
}

.gift-content {
  display: inline-flex;
  align-items: center;
  gap: 4px;
}

.gift-prefix {
  color: var(--edu-text-secondary);
}

.gift-name {
  font-weight: 600;
}

.gift-icon {
  width: 18px;
  height: 18px;
  border-radius: 4px;
  object-fit: cover;
}

.system-message {
  display: flex;
  justify-content: flex-start;
  padding: 2px 0 4px;
}

.system-message-text {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-height: 22px;
  padding: 0 10px;
  border-radius: 999px;
  font-size: var(--edu-font-size-micro);
  font-weight: 500;
  line-height: 1.4;
  color: var(--edu-text-secondary);
  background: color-mix(in srgb, var(--edu-bg-muted) 84%, transparent);
}

.system-message-enter .system-message-text {
  color: color-mix(in srgb, var(--edu-primary) 78%, var(--edu-text-secondary));
}

.system-message-leave .system-message-text {
  color: color-mix(in srgb, #ef4444 72%, var(--edu-text-secondary));
}

// === Members tab ===
.members-panel {
  display: flex;
  flex-direction: column;
}

.members-list {
  display: flex;
  flex-direction: column;
  padding: 6px 0;
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  scrollbar-width: thin;
  scrollbar-color: color-mix(in srgb, var(--edu-primary) 28%, var(--edu-bg-muted)) transparent;
}

.chat-list::-webkit-scrollbar,
.members-list::-webkit-scrollbar {
  width: 10px;
}

.chat-list::-webkit-scrollbar-track,
.members-list::-webkit-scrollbar-track {
  background: transparent;
}

.chat-list::-webkit-scrollbar-thumb,
.members-list::-webkit-scrollbar-thumb {
  border: 3px solid transparent;
  border-radius: 999px;
  background: color-mix(in srgb, var(--edu-primary) 26%, var(--edu-bg-muted));
  background-clip: padding-box;
  min-height: 36px;
}

.chat-list::-webkit-scrollbar-thumb:hover,
.members-list::-webkit-scrollbar-thumb:hover {
  background: color-mix(in srgb, var(--edu-primary) 42%, var(--edu-bg-muted));
  background-clip: padding-box;
}

.chat-list::-webkit-scrollbar-corner,
.members-list::-webkit-scrollbar-corner {
  background: transparent;
}

.member-item {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 8px 14px;
  transition: background 120ms ease;

  &:hover {
    background: var(--edu-bg-hover);
  }

  &.instructor {
    border-bottom: 1px solid var(--edu-border);
    padding-bottom: 10px;
    margin-bottom: 2px;
  }
}

.member-avatar {
  flex-shrink: 0;
  overflow: hidden;

  &.instructor-avatar {
    background: var(--edu-primary);
    color: var(--edu-text-on-primary);
  }
}

.member-info {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
  gap: 1px;
}

.member-name {
  font-size: var(--edu-font-size-caption);
  font-weight: 500;
  color: var(--edu-text-primary);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.member-role {
  font-size: var(--edu-font-size-micro);
  color: var(--edu-text-tertiary);

  &.on-seat {
    color: var(--edu-primary);
    font-weight: 500;
  }
}

.member-status-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  flex-shrink: 0;

  &.online {
    background: var(--edu-primary);
  }

  &.offline {
    background: var(--edu-text-tertiary);
    opacity: 0.4;
  }
}

// === Empty state ===
.empty-state {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 6px;
  padding: 40px 16px;
  text-align: center;

  p {
    margin: 0;
    font-size: var(--edu-font-size-body);
    font-weight: 500;
    color: var(--edu-text-secondary);
  }

  span {
    font-size: var(--edu-font-size-small);
    color: var(--edu-text-tertiary);
  }
}

.empty-icon {
  width: 36px;
  height: 36px;
  stroke: var(--edu-text-secondary);
  stroke-width: 1.5;
  stroke-linecap: round;
  stroke-linejoin: round;
  opacity: 1;
}

// === Bottom input (using BarrageInput component) ===
.panel-input {
  display: flex;
  gap: 8px;
  align-items: center;
  padding: 10px 12px;
  border-top: 1px solid var(--edu-border);
  background: var(--edu-bg-surface);
  flex-shrink: 0;

  &.disabled {
    opacity: 0.5;
    pointer-events: none;
  }
}

.input-wrapper {
  flex: 1;
  min-width: 0;
  display: flex;
  align-items: center;
  border-radius: var(--edu-radius-md);
  min-height: 40px;
  padding: 6px 10px;
  background: var(--edu-bg-muted);
  border: 1px solid var(--edu-border);
  transition: all 180ms ease;

  &.focused {
    border-color: var(--edu-primary);
    box-shadow: 0 0 0 2px color-mix(in srgb, var(--edu-primary) 16%, transparent);
  }

  &.disabled {
    pointer-events: none;
  }
}

// === BarrageInput component deep styles ===
:deep(.edu-barrage-input) {
  flex: 1;
  min-width: 0;
}

:deep(.edu-barrage-input .live-message-input) {
  width: 100%;
}

:deep(.edu-barrage-input .message-input-container) {
  min-height: 28px;
  max-height: 76px;
  padding: 0;
  border: none;
  border-radius: 0;
  background: transparent;
  overflow: visible;
  display: flex;
  align-items: center;
}

:deep(.edu-barrage-input .input-wrapper) {
  align-items: center;
}

:deep(.edu-barrage-input .input-prefix) {
  display: flex;
  align-items: center;
  align-self: center;
}

:deep(.edu-barrage-input .input-actions) {
  min-height: 24px;
  display: flex;
  align-items: center;
  align-self: center;
  margin-right: 2px;
  gap: 0;
  padding-bottom: 0;
}

:deep(.edu-barrage-input .emoji-picker__icon) {
  width: 16px;
  height: 16px;
  color: var(--edu-text-tertiary);
}

:deep(.edu-barrage-input .tiptap.ProseMirror) {
  flex: 1;
  min-height: 20px;
  max-height: 72px;
  margin: 0;
  color: var(--edu-text-primary);
  font-size: var(--edu-font-size-caption);
  line-height: 20px;
  font-family: inherit;
  white-space: pre-wrap;
  overflow-wrap: anywhere;
  word-break: break-word;
  overflow-y: auto;

  p {
    margin: 0;
    white-space: inherit;
  }

  p.is-editor-empty:first-child::before {
    color: var(--edu-text-tertiary);
    font-size: var(--edu-font-size-caption);
  }
}
</style>
