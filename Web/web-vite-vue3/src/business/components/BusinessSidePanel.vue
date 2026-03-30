<template>
  <div class="biz-side-panel">
    <div class="panel-card">
      <!-- Tab Bar -->
      <div class="tab-bar">
        <div class="tab-bar-inner">
          <!-- Animated sliding pill background -->
          <div class="tab-slider" :style="sliderStyle" />
          <button
            v-for="(tab, index) in tabs"
            :key="tab.id"
            ref="tabBtnRefs"
            class="tab-btn"
            :class="{ active: activeTab === tab.id }"
            @click="switchTab(tab.id, index)"
          >
            <span class="tab-icon" aria-hidden="true">
              <svg v-if="tab.id === 'chat'" viewBox="0 0 16 16">
                <mask id="biz-chat-tab-mask" fill="white">
                  <path d="M7.99902 1.33337C11.6808 1.33355 14.665 4.31858 14.665 8.00037C14.665 9.39818 14.2333 10.6943 13.498 11.766C13.5009 11.7745 13.5058 11.7824 13.5078 11.7914L13.9082 13.5599C13.9513 13.751 13.7809 13.9214 13.5898 13.8783L11.8213 13.4779C11.8154 13.4766 11.8103 13.4727 11.8047 13.4711C10.7253 14.2234 9.41443 14.6663 7.99902 14.6664C4.31723 14.6664 1.33221 11.6821 1.33203 8.00037C1.33203 4.31847 4.31713 1.33337 7.99902 1.33337Z" />
                </mask>
                <path d="M7.99902 1.33337L7.99907 0.333374H7.99902V1.33337ZM14.665 8.00037L15.665 8.00041V8.00037H14.665ZM13.498 11.766L12.6735 11.2002L12.3924 11.6098L12.549 12.0812L13.498 11.766ZM13.5078 11.7914L14.4831 11.5706L14.483 11.57L13.5078 11.7914ZM13.9082 13.5599L14.8837 13.3399L14.8835 13.3391L13.9082 13.5599ZM13.5898 13.8783L13.369 14.8536L13.3699 14.8538L13.5898 13.8783ZM11.8213 13.4779L11.6 14.4531L11.6005 14.4532L11.8213 13.4779ZM11.8047 13.4711L12.0875 12.5119L11.6269 12.3761L11.2329 12.6507L11.8047 13.4711ZM7.99902 14.6664V15.6664H7.99907L7.99902 14.6664ZM1.33203 8.00037H0.332031V8.00041L1.33203 8.00037ZM7.99902 1.33337L7.99898 2.33337C11.1283 2.33352 13.665 4.87072 13.665 8.00037H14.665H15.665C15.665 3.76643 12.2332 0.333576 7.99907 0.333374L7.99902 1.33337ZM14.665 8.00037L13.665 8.00032C13.665 9.18896 13.2986 10.2891 12.6735 11.2002L13.498 11.766L14.3226 12.3317C15.168 11.0996 15.665 9.6074 15.665 8.00041L14.665 8.00037ZM13.498 11.766L12.549 12.0812C12.562 12.1203 12.5747 12.1507 12.5811 12.1657C12.5844 12.1735 12.5872 12.1797 12.5882 12.1819C12.5901 12.1862 12.5886 12.1828 12.5881 12.1817C12.5863 12.1776 12.5776 12.158 12.5679 12.1316C12.5578 12.1042 12.5441 12.0635 12.5326 12.0128L13.5078 11.7914L14.483 11.57C14.4705 11.5148 14.4555 11.4697 14.4437 11.438C14.4324 11.4073 14.4219 11.3835 14.4183 11.3753C14.4169 11.3721 14.4145 11.3666 14.4155 11.3688C14.4155 11.369 14.4174 11.3732 14.4198 11.3788C14.4245 11.3897 14.4355 11.4159 14.4471 11.4508L13.498 11.766ZM13.5078 11.7914L12.5325 12.0122L12.9329 13.7807L13.9082 13.5599L14.8835 13.3391L14.4831 11.5706L13.5078 11.7914ZM13.9082 13.5599L12.9327 13.78C12.8141 13.2543 13.2844 12.7843 13.8098 12.9028L13.5898 13.8783L13.3699 14.8538C14.2775 15.0585 15.0885 14.2478 14.8837 13.3399L13.9082 13.5599ZM13.5898 13.8783L13.8106 12.903L12.0421 12.5026L11.8213 13.4779L11.6005 14.4532L13.369 14.8536L13.5898 13.8783ZM11.8213 13.4779L12.0426 12.5027C12.111 12.5182 12.1646 12.5381 12.1982 12.552C12.2153 12.559 12.229 12.5653 12.2381 12.5695C12.2468 12.5736 12.2535 12.5769 12.2547 12.5775C12.2548 12.5776 12.2562 12.5783 12.2546 12.5774C12.2537 12.577 12.2515 12.5759 12.2488 12.5746C12.2436 12.5721 12.2332 12.5671 12.2202 12.5612C12.1948 12.5497 12.1482 12.5298 12.0875 12.5119L11.8047 13.4711L11.5219 14.4303C11.464 14.4132 11.4201 14.3943 11.3975 14.3841C11.3858 14.3789 11.3768 14.3745 11.3729 14.3726C11.3713 14.3718 11.3653 14.3688 11.3711 14.3717C11.3737 14.373 11.3817 14.3769 11.3918 14.3817C11.4022 14.3865 11.4174 14.3934 11.4358 14.401C11.4722 14.416 11.5287 14.4369 11.6 14.4531L11.8213 13.4779ZM11.8047 13.4711L11.2329 12.6507C10.3149 13.2905 9.20229 13.6663 7.99898 13.6664L7.99902 14.6664L7.99907 15.6664C9.62657 15.6663 11.1357 15.1562 12.3765 14.2915L11.8047 13.4711ZM7.99902 14.6664V13.6664C4.86938 13.6664 2.33218 11.1297 2.33203 8.00032L1.33203 8.00037L0.332031 8.00041C0.332234 12.2346 3.76509 15.6664 7.99902 15.6664V14.6664ZM1.33203 8.00037H2.33203C2.33203 4.87075 4.86941 2.33337 7.99902 2.33337V1.33337V0.333374C3.76484 0.333374 0.332031 3.76618 0.332031 8.00037H1.33203Z" fill="currentColor" mask="url(#biz-chat-tab-mask)" />
                <path d="M4.90039 7.34961C5.39727 7.34982 5.7998 7.75307 5.7998 8.25C5.79979 8.74691 5.39726 9.15018 4.90039 9.15039C4.40334 9.15039 4.00001 8.74705 4 8.25C4 7.75294 4.40333 7.34961 4.90039 7.34961ZM8 7.34961C8.49688 7.34982 8.89941 7.75307 8.89941 8.25C8.8994 8.74691 8.49687 9.15018 8 9.15039C7.50295 9.15039 7.09962 8.74705 7.09961 8.25C7.09961 7.75294 7.50294 7.34961 8 7.34961ZM11.0996 7.34961C11.5965 7.34982 11.999 7.75307 11.999 8.25C11.999 8.74691 11.5965 9.15018 11.0996 9.15039C10.6026 9.15039 10.1992 8.74705 10.1992 8.25C10.1992 7.75294 10.6026 7.34961 11.0996 7.34961Z" fill="currentColor" />
              </svg>
              <svg v-else viewBox="0 0 16 16">
                <path d="M6.00003 1C6.52575 1 7.02232 1.12482 7.46171 1.34645L6.83144 2.1586C6.57424 2.05626 6.2937 2 6.00003 2C4.75739 2 3.75003 3.00736 3.75003 4.25C3.75003 5.47717 4.73246 6.47488 5.95373 6.49953V7.49968C4.18015 7.47491 2.75003 6.02947 2.75003 4.25C2.75003 2.45507 4.2051 1 6.00003 1Z" />
                <path d="M0.540929 9.43593C2.18917 8.66419 4.01922 8.22779 5.95373 8.22094V9.22095C4.18421 9.22774 2.51071 9.62379 1 10.3252V12H2.85709V13H0.5C0.223857 13 0 12.7761 0 12.5V10.3086C0 9.9372 0.204537 9.59344 0.540929 9.43593Z" />
                <path d="M9.99992 8.31354C8.20499 8.31354 6.74992 6.85846 6.74992 5.06354C6.74992 3.26861 8.20499 1.81354 9.99992 1.81354C11.7948 1.81354 13.2499 3.26861 13.2499 5.06354C13.2499 6.85846 11.7948 8.31354 9.99992 8.31354ZM9.99992 7.31354C11.2426 7.31354 12.2499 6.30618 12.2499 5.06354C12.2499 3.8209 11.2426 2.81354 9.99992 2.81354C8.75728 2.81354 7.74992 3.8209 7.74992 5.06354C7.74992 6.30618 8.75728 7.31354 9.99992 7.31354Z" />
                <path d="M16 11.5178C16 11.1464 15.7954 10.8026 15.459 10.6451C13.7977 9.86723 11.9516 9.43005 9.99997 9.43005C8.04837 9.43005 6.20227 9.86723 4.54089 10.6451C4.2045 10.8026 3.99996 11.1464 3.99996 11.5178V14C3.99996 14.2761 4.22382 14.5 4.49996 14.5H15.5C15.7761 14.5 16 14.2761 16 14V11.5178ZM15 11.5344V13.5H4.99996V11.5344C6.52384 10.8269 8.21334 10.43 9.99997 10.43C11.7866 10.43 13.4761 10.8269 15 11.5344Z" />
              </svg>
            </span>
            <span class="tab-label">{{ t(tab.label) }}</span>
            <span v-if="tab.badge" class="tab-badge" :class="{ active: activeTab === tab.id }">{{ tab.badge }}</span>
          </button>
        </div>
      </div>

      <!-- Tab Content -->
      <div class="tab-content">
        <!-- Chat Panel -->
        <Transition name="tab-fade" mode="out-in">
          <div v-if="activeTab === 'chat'" key="chat" class="chat-panel">
            <div ref="chatListRef" class="chat-list" @scroll="handleChatScroll">
              <template v-for="item in chatTimeline">
                <div v-if="item.type === 'divider'" :key="item.key" class="time-divider">
                  <span class="divider-line" />
                  <span class="divider-text">{{ item.label }}</span>
                  <span class="divider-line" />
                </div>
                <div
                  v-else-if="item.type === 'system'"
                  :key="item.key"
                  class="system-msg"
                  :class="`system-msg-${item.action}`"
                >
                  <svg
                    v-if="item.action === 'enter'"
                    class="system-icon"
                    width="12"
                    height="12"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="currentColor"
                    stroke-width="2"
                    stroke-linecap="round"
                    stroke-linejoin="round"
                  >
                    <path d="M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4" />
                    <polyline points="10 17 15 12 10 7" />
                    <line x1="15" y1="12" x2="3" y2="12" />
                  </svg>
                  <svg
                    v-else
                    class="system-icon"
                    width="12"
                    height="12"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="currentColor"
                    stroke-width="2"
                    stroke-linecap="round"
                    stroke-linejoin="round"
                  >
                    <path d="M9 3H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h4" />
                    <polyline points="14 7 9 12 14 17" />
                    <line x1="21" y1="12" x2="9" y2="12" />
                  </svg>
                  <span>{{ item.label }}</span>
                </div>
                <div v-else :key="item.key" class="chat-msg">
                  <Avatar
                    class="msg-avatar"
                    :src="getMessageAvatar(item.msg.sender)"
                    :size="32"
                    :alt="item.msg.sender.userName || item.msg.sender.userId"
                    :style="getMsgAvatarStyle(item.msg.sender)"
                  />
                  <div class="msg-main">
                    <div class="msg-meta-line">
                      <span class="msg-name" :class="getRoleClass(item.msg.sender)">
                        {{ item.msg.sender.userName || item.msg.sender.userId }}
                      </span>
                      <span v-if="isHost(item.msg.sender)" class="role-badge host-badge">{{ t('Host') }}</span>
                    </div>
                    <div class="msg-bubble" :class="getRoleClass(item.msg.sender)">
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
                          :key="`${item.key}-${segmentIndex}`"
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
                    </div>
                  </div>
                </div>
              </template>
            </div>

            <!-- Chat Input -->
            <div class="chat-input-shell" :class="{ 'chat-disabled': isInputDisabled }">
              <div
                ref="barrageInputShellRef"
                class="input-wrapper"
                :class="{ focused: inputFocused, disabled: isInputDisabled }"
              >
                <BarrageInput
                  class="biz-barrage-input"
                  :auto-focus="false"
                  :disabled="isInputDisabled"
                  :placeholder="isInputDisabled ? (liveEnded ? t('Live has ended') : mutedText) : t('Type a message...')"
                  :on-will-send-barrage="handleWillSendBarrage"
                  :on-did-send-barrage="handleDidSendBarrage"
                  @focus="handleInputFocus"
                  @blur="handleInputBlur"
                />
              </div>
            </div>
          </div>

          <!-- Audience Panel -->
          <div v-else-if="activeTab === 'audience'" key="audience" class="audience-panel">
            <div class="audience-list-scroll">
              <!-- Host & Admin Group -->
              <div v-if="hostAndAdmins.length" class="audience-group">
                <TransitionGroup name="audience-item" tag="div" class="group-items">
                  <div
                    v-for="viewer in hostAndAdmins"
                    :key="viewer.userId"
                    class="audience-row"
                  >
                    <div class="audience-avatar-wrap">
                      <Avatar
                        class="audience-avatar"
                        :src="getViewerAvatar(viewer)"
                        :size="32"
                        :alt="viewer.userName || viewer.userId"
                        :style="getAvatarStyle(viewer)"
                      />
                    </div>
                    <div class="audience-info">
                      <span class="audience-name" :class="getRoleClass(viewer)">
                        {{ viewer.userName || viewer.userId }}
                      </span>
                      <span v-if="isHost(viewer)" class="audience-role-badge host-role">{{ t('Host') }}</span>
                      <span v-else-if="isSelf(viewer)" class="audience-role-badge self-role">{{ t('Me') }}</span>
                    </div>
                  </div>
                </TransitionGroup>
              </div>

              <!-- Viewers Group -->
              <div v-if="regularViewers.length" class="audience-group">
                <TransitionGroup name="audience-item" tag="div" class="group-items">
                  <div
                    v-for="viewer in displayedViewers"
                    :key="viewer.userId"
                    class="audience-row"
                  >
                    <div class="audience-avatar-wrap">
                      <Avatar
                        class="audience-avatar"
                        :src="getViewerAvatar(viewer)"
                        :size="32"
                        :alt="viewer.userName || viewer.userId"
                        :style="getAvatarStyle(viewer)"
                      />
                    </div>
                    <div class="audience-info">
                      <span class="audience-name">
                        {{ viewer.userName || viewer.userId }}
                      </span>
                      <span v-if="isSelf(viewer)" class="audience-role-badge self-role">{{ t('Me') }}</span>
                    </div>
                  </div>
                </TransitionGroup>
                <button
                  v-if="regularViewers.length > VIEWER_PAGE_SIZE && !showAllViewers"
                  class="show-more-btn"
                  @click="showAllViewers = true"
                >
                  {{ t('More') }} ({{ regularViewers.length - VIEWER_PAGE_SIZE }})
                </button>
              </div>

              <!-- Empty state -->
              <div v-if="!audienceList.length" class="audience-empty">
                <svg width="40" height="40" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.2" stroke-linecap="round" stroke-linejoin="round">
                  <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" />
                  <circle cx="9" cy="7" r="4" />
                  <path d="M23 21v-2a4 4 0 0 0-3-3.87" />
                  <path d="M16 3.13a4 4 0 0 1 0 7.75" />
                </svg>
                <span>{{ t('No audience yet') }}</span>
              </div>
            </div>
          </div>
        </Transition>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue';
import {
  useBarrageState,
  Avatar,
  BarrageInput,
  useLiveAudienceState,
  useLiveGiftState,
  useLiveListState,
  useLoginState,
  useRoomEngine,
  LiveGiftEvents,
  useMessageInputState,
  BarrageType,
} from 'tuikit-atomicx-vue3';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { TUIRoomEvents } from '@tencentcloud/tuiroom-engine-js';
import type { Barrage, AudienceInfo } from 'tuikit-atomicx-vue3';

const { t } = useUIKit();

const props = withDefaults(defineProps<{
  liveEnded?: boolean;
}>(), {
  liveEnded: false,
});

const { messageList, appendLocalTip } = useBarrageState();
const { inputRawValue } = useMessageInputState();
const { audienceList, audienceCount } = useLiveAudienceState();
const { subscribeEvent: subscribeGiftEvent, unsubscribeEvent: unsubscribeGiftEvent } = useLiveGiftState();
const { currentLive } = useLiveListState();
const { loginUserInfo } = useLoginState();
const roomEngine = useRoomEngine();
const localAudience = computed(() => audienceList.value.find(item => item.userId === loginUserInfo.value?.userId));
const isMessageMuted = computed(() => !!localAudience.value?.isMessageDisabled);
const isInputDisabled = computed(() => props.liveEnded || isMessageMuted.value);
const mutedText = computed(() => t('You have been muted in this room'));

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

// === Tab state ===
type TabId = 'chat' | 'audience';
const activeTab = ref<TabId>('chat');
const activeTabIndex = ref(0);
const VIEWER_PAGE_SIZE = 20;
const showAllViewers = ref(false);

const tabs = computed(() => [
  { id: 'chat' as TabId, label: 'Barrage list', badge: '' },
  { id: 'audience' as TabId, label: 'Online viewers', badge: formatCompact(displayAudienceCount.value) },
]);

// Tab button refs for measuring slider position
const tabBtnRefs = ref<HTMLElement[]>([]);

// Slider position state (measured from actual DOM elements)
const sliderLeft = ref(0);
const sliderWidth = ref(0);
const sliderReady = ref(false);
let tabResizeObserver: ResizeObserver | null = null;
let tabResizeRaf = 0;

function remeasureActiveSlider() {
  measureSlider(activeTabIndex.value);
}

function measureSlider(index: number) {
  nextTick(() => {
    const btns = tabBtnRefs.value;
    if (!btns || !btns[index]) return;
    const btn = btns[index] as HTMLElement;
    const parent = btn.parentElement;
    if (!parent) return;
    const parentRect = parent.getBoundingClientRect();
    const btnRect = btn.getBoundingClientRect();
    sliderLeft.value = btnRect.left - parentRect.left;
    sliderWidth.value = btnRect.width;
    sliderReady.value = true;
  });
}

const sliderStyle = computed(() => {
  if (!sliderReady.value) {
    // Fallback: percentage-based positioning before measurement
    const count = tabs.value.length;
    return {
      left: `${(activeTabIndex.value / count) * 100}%`,
      width: `${100 / count}%`,
      opacity: '0',
    };
  }
  return {
    left: `${sliderLeft.value}px`,
    width: `${sliderWidth.value}px`,
    opacity: '1',
  };
});

function switchTab(tabId: TabId, index: number) {
  if (activeTab.value === tabId) return;
  activeTab.value = tabId;
  activeTabIndex.value = index;
  measureSlider(index);
}

function handleWindowResize() {
  if (tabResizeRaf) {
    cancelAnimationFrame(tabResizeRaf);
  }
  tabResizeRaf = requestAnimationFrame(() => {
    remeasureActiveSlider();
  });
}

// === Chat state ===
const inputFocused = ref(false);
const chatListRef = ref<HTMLElement | null>(null);
const barrageInputShellRef = ref<HTMLElement | null>(null);
const inputHasContent = ref(false);
const shouldStickToBottom = ref(true);

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

function hashName(name: string): number {
  let hash = 0;
  for (let i = 0; i < name.length; i++) {
    hash = ((hash << 5) - hash + name.charCodeAt(i)) | 0;
  }
  return Math.abs(hash);
}

function getAvatarColor(name: string) {
  const idx = hashName(name) % LIGHT_PALETTE.length;
  const root = document.querySelector('.style-preset-business') as HTMLElement | null;
  if (root) {
    const styles = getComputedStyle(root);
    const bg = styles.getPropertyValue(`--preset-avatar-${idx}-bg`).trim();
    const text = styles.getPropertyValue(`--preset-avatar-${idx}-text`).trim();
    if (bg && text) return { bg, text };
  }
  return LIGHT_PALETTE[idx];
}

function isHost(sender: { userId?: string }): boolean {
  return sender.userId === currentLive.value?.liveOwner?.userId;
}

function isSelf(sender: { userId?: string }): boolean {
  return sender.userId === loginUserInfo.value?.userId;
}

// Determine CSS role class for a user
function getRoleClass(user: { userId?: string }): Record<string, boolean> {
  return {
    host: isHost(user),
    self: isSelf(user),
  };
}

const displayAudienceCount = computed(() => audienceCount.value || audienceList.value.length);

// === Audience grouping ===
const hostAndAdmins = computed(() => audienceList.value.filter((v: AudienceInfo) => isHost(v)));

const regularViewers = computed(() => audienceList.value.filter((v: AudienceInfo) => !isHost(v)));

const displayedViewers = computed(() => {
  if (showAllViewers.value) return regularViewers.value;
  return regularViewers.value.slice(0, VIEWER_PAGE_SIZE);
});
const audienceAvatarByUserId = computed(() => {
  const map = new Map<string, string>();
  audienceList.value.forEach((viewer: AudienceInfo) => {
    const { userId } = viewer;
    const avatar = getRawAvatar(viewer as AvatarLikeUser);
    if (!userId || !avatar || map.has(userId)) return;
    map.set(userId, avatar);
  });
  return map;
});

type AvatarLikeUser = {
  userId?: string;
  userName?: string;
  avatarUrl?: string;
  avatar?: string;
  avatarURL?: string;
};

function getRawAvatar(user?: AvatarLikeUser | null): string {
  if (!user) return '';
  const candidate = user.avatarUrl || user.avatar || user.avatarURL || '';
  return typeof candidate === 'string' ? candidate.trim() : '';
}

function getAvatarStyle(viewer: AudienceInfo) {
  if (getViewerAvatar(viewer)) return {};
  if (isHost(viewer)) return { background: 'var(--preset-primary)', color: 'var(--preset-send-btn-active-text)' };
  if (isSelf(viewer)) return { background: 'var(--preset-primary-hover)', color: 'var(--preset-send-btn-active-text)' };
  const c = getAvatarColor(viewer.userName || viewer.userId || '');
  return { background: c.bg, color: c.text };
}

function getViewerAvatar(viewer: AudienceInfo): string {
  return getRawAvatar(viewer as AvatarLikeUser);
}

function getMessageAvatar(sender: AvatarLikeUser): string {
  const directAvatar = getRawAvatar(sender);
  if (directAvatar) return directAvatar;
  if (sender.userId && audienceAvatarByUserId.value.has(sender.userId)) {
    return audienceAvatarByUserId.value.get(sender.userId) || '';
  }
  if (sender.userId && sender.userId === currentLive.value?.liveOwner?.userId) {
    const hostAvatar = getRawAvatar(currentLive.value.liveOwner as AvatarLikeUser);
    if (hostAvatar) return hostAvatar;
  }
  if (sender.userId && sender.userId === loginUserInfo.value?.userId) {
    const selfAvatar = getRawAvatar(loginUserInfo.value as AvatarLikeUser);
    if (selfAvatar) return selfAvatar;
  }
  return '';
}

function getMsgAvatarStyle(sender: { userId?: string; userName?: string }) {
  if (getMessageAvatar(sender as AvatarLikeUser)) return {};
  if (isHost(sender)) return { background: 'var(--preset-primary)', color: 'var(--preset-send-btn-active-text)' };
  if (isSelf(sender)) return { background: 'var(--preset-primary-hover)', color: 'var(--preset-send-btn-active-text)' };
  const c = getAvatarColor(sender.userName || sender.userId || '');
  return { background: c.bg, color: c.text };
}

function formatCompact(n: number): string {
  if (n >= 1000000) return `${(n / 1000000).toFixed(1)}m`;
  if (n >= 1000) return `${(n / 1000).toFixed(1)}k`;
  return String(n);
}

// === Chat timeline with grouping and system messages ===
const displayMessages = computed(() => messageList.value
  .filter((m: Barrage) => (m.textContent && m.textContent.length > 0) || isGiftMessage(m))
  .map((m: Barrage) => ({ ...m })));

type MessageSegment = {
  type: 'text' | 'emoji';
  value: string;
  emojiKey?: string;
};

type GiftPayload = {
  giftName: string;
  iconUrl: string;
};

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

function getMessageCacheKey(message: Barrage): string {
  const seq = typeof message.sequence === 'number' ? String(message.sequence) : '';
  if (seq) return seq;
  const ts = typeof message.timestampInSecond === 'number' ? String(message.timestampInSecond) : '';
  if (ts) return `${message.sender.userId || 'unknown'}-${ts}`;
  return `${message.sender.userId || 'unknown'}-${message.businessId || 'message'}-${message.textContent || ''}`;
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
  } catch (error) {
    console.error('[BusinessSidePanel] Failed to parse gift data:', error);
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
  const color = `hsl(${hue} ${saturation}% ${lightness}%)`;
  return {
    color,
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

type SystemTip = {
  key: string;
  label: string;
  timestampInSecond: number;
  action: 'enter' | 'leave';
};

const systemTips = ref<SystemTip[]>([]);
const MAX_SYSTEM_TIPS = 200;

function getUserDisplayName(userInfo: { userId?: string; userName?: string; nameCard?: string }) {
  return userInfo.nameCard || userInfo.userName || userInfo.userId || '';
}

function appendSystemTip(action: 'enter' | 'leave', userInfo: { userId?: string; userName?: string; nameCard?: string }) {
  if (!userInfo?.userId) return;
  const name = getUserDisplayName(userInfo);
  if (!name) return;
  const rawSuffix = action === 'enter' ? t('BarrageList.ComeIn') : t('BarrageList.Leave');
  const suffix = rawSuffix === 'BarrageList.ComeIn'
    ? 'came in'
    : (rawSuffix === 'BarrageList.Leave' ? 'left' : rawSuffix);
  const now = Math.floor(Date.now() / 1000);
  const tip: SystemTip = {
    key: `sys-${action}-${userInfo.userId}-${Date.now()}`,
    label: `${name} ${suffix}`,
    timestampInSecond: now,
    action,
  };
  const next = [...systemTips.value, tip];
  systemTips.value = next.slice(-MAX_SYSTEM_TIPS);
}

function handleRemoteUserEnterRoom(eventInfo: { userInfo: { userId?: string; userName?: string; nameCard?: string } }) {
  appendSystemTip('enter', eventInfo.userInfo);
}

function handleRemoteUserLeaveRoom(eventInfo: { userInfo: { userId?: string; userName?: string; nameCard?: string } }) {
  appendSystemTip('leave', eventInfo.userInfo);
}

const chatTimeline = computed(() => {
  const timeline: Array<
  | { type: 'divider'; key: string; label: string }
  | { type: 'system'; key: string; label: string; action: 'enter' | 'leave' }
  | { type: 'message'; key: string; msg: Barrage }
  > = [];

  const mergedTimelineItems = [
    ...displayMessages.value.map(msg => ({
      type: 'message' as const,
      ts: msg.timestampInSecond || 0,
      key: `msg-${msg.sequence}`,
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

  let lastDividerTs: number | null = null;

  mergedTimelineItems.forEach((item) => {
    const { ts } = item;
    const shouldShowDivider = lastDividerTs === null || ts - lastDividerTs >= 300;
    if (shouldShowDivider) {
      timeline.push({
        type: 'divider' as const,
        key: `divider-${item.key}`,
        label: formatChatDivider(ts),
      });
      lastDividerTs = ts;
    }

    if (item.type === 'message') {
      timeline.push({
        type: 'message' as const,
        key: item.key,
        msg: item.msg,
      });
      return;
    }
    timeline.push({
      type: 'system' as const,
      key: item.key,
      label: item.label,
      action: item.action,
    });
  });
  return timeline;
});

function scrollChatToBottom(force = false) {
  if (!force && !shouldStickToBottom.value) return;
  nextTick(() => {
    if (chatListRef.value) {
      chatListRef.value.scrollTop = chatListRef.value.scrollHeight;
      shouldStickToBottom.value = true;
    }
  });
}

watch(() => chatTimeline.value.length, (currentLength, previousLength) => {
  if (currentLength <= (previousLength ?? 0)) return;
  scrollChatToBottom(true);
});

watch(activeTab, (tab) => {
  if (tab !== 'chat') return;
  const scrollAfterTransition = () => {
    nextTick(() => {
      if (chatListRef.value) {
        handleChatScroll();
        scrollChatToBottom(true);
      } else {
        setTimeout(() => {
          nextTick(() => {
            handleChatScroll();
            scrollChatToBottom(true);
          });
        }, 50);
      }
    });
  };
  setTimeout(scrollAfterTransition, 200);
});

watch(() => tabs.value.map(tab => tab.badge).join('|'), () => {
  remeasureActiveSlider();
});

watch(inputRawValue, (value) => {
  inputHasContent.value = hasInputValue(value);
}, { deep: true, immediate: true });

function handleChatScroll() {
  if (!chatListRef.value) return;
  const el = chatListRef.value;
  shouldStickToBottom.value = (el.scrollHeight - el.clientHeight - el.scrollTop) < 24;
}

function getBarrageEditorElement(): HTMLElement | null {
  return barrageInputShellRef.value?.querySelector('.tiptap.ProseMirror') as HTMLElement | null;
}

function hasInputValue(value: string | Array<{ type?: string; content?: unknown }>) {
  if (typeof value === 'string') {
    return value.trim().length > 0;
  }
  if (!Array.isArray(value)) return false;
  return value.some((item) => {
    if (!item) return false;
    if (typeof item.content === 'string') {
      return item.content.trim().length > 0;
    }
    return !!item.content;
  });
}

function refreshInputHasContent() {
  if (hasInputValue(inputRawValue.value)) {
    inputHasContent.value = true;
    return;
  }
  const editorEl = getBarrageEditorElement();
  if (!editorEl) {
    inputHasContent.value = false;
    return;
  }
  const plainText = (editorEl.textContent || '').replace(/\u200b/g, '').trim();
  const hasEmoji = !!editorEl.querySelector('img.message-emoji');
  inputHasContent.value = !!plainText || hasEmoji;
}

function handleInputFocus() {
  inputFocused.value = true;
  refreshInputHasContent();
}

function handleInputBlur() {
  inputFocused.value = false;
  refreshInputHasContent();
}

function handleWillSendBarrage() {
  if (isInputDisabled.value) return false;
  return true;
}

function handleDidSendBarrage() {
  nextTick(() => {
    refreshInputHasContent();
  });
}

function formatChatDivider(ts: number): string {
  const d = new Date(ts * 1000);
  const now = new Date();
  const hhmm = `${String(d.getHours()).padStart(2, '0')}:${String(d.getMinutes()).padStart(2, '0')}`;
  const isToday = now.getFullYear() === d.getFullYear()
    && now.getMonth() === d.getMonth()
    && now.getDate() === d.getDate();
  if (isToday) return hhmm;
  return `${String(d.getMonth() + 1).padStart(2, '0')}/${String(d.getDate()).padStart(2, '0')} ${hhmm}`;
}

let editorMutationObserver: MutationObserver | null = null;
let observedEditorEl: HTMLElement | null = null;
let shellInputListenerBound = false;

function detachEditorObserver() {
  editorMutationObserver?.disconnect();
  editorMutationObserver = null;
  if (observedEditorEl) {
    observedEditorEl.removeEventListener('input', refreshInputHasContent);
    observedEditorEl.removeEventListener('keyup', refreshInputHasContent);
    observedEditorEl = null;
  }
}

function bindShellInputListeners() {
  if (shellInputListenerBound) return;
  const shell = barrageInputShellRef.value;
  if (!shell) return;
  shell.addEventListener('input', refreshInputHasContent, true);
  shell.addEventListener('keyup', refreshInputHasContent, true);
  shell.addEventListener('compositionend', refreshInputHasContent, true);
  shellInputListenerBound = true;
}

function unbindShellInputListeners() {
  if (!shellInputListenerBound) return;
  const shell = barrageInputShellRef.value;
  if (!shell) return;
  shell.removeEventListener('input', refreshInputHasContent, true);
  shell.removeEventListener('keyup', refreshInputHasContent, true);
  shell.removeEventListener('compositionend', refreshInputHasContent, true);
  shellInputListenerBound = false;
}

function attachEditorObserver() {
  nextTick(() => {
    const editorEl = getBarrageEditorElement();
    if (!editorEl || observedEditorEl === editorEl) {
      refreshInputHasContent();
      return;
    }
    detachEditorObserver();
    observedEditorEl = editorEl;
    editorMutationObserver = new MutationObserver(() => {
      refreshInputHasContent();
    });
    editorMutationObserver.observe(editorEl, {
      childList: true,
      subtree: true,
      characterData: true,
    });
    editorEl.addEventListener('input', refreshInputHasContent);
    editorEl.addEventListener('keyup', refreshInputHasContent);
    refreshInputHasContent();
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

onMounted(() => {
  window.addEventListener('resize', handleWindowResize);
  subscribeGiftEvent(LiveGiftEvents.ON_RECEIVE_GIFT_MESSAGE, handleGiftMessage);
  roomEngine.instance?.on(TUIRoomEvents.onRemoteUserEnterRoom, handleRemoteUserEnterRoom);
  roomEngine.instance?.on(TUIRoomEvents.onRemoteUserLeaveRoom, handleRemoteUserLeaveRoom);
  nextTick(() => {
    bindShellInputListeners();
    attachEditorObserver();
    // Initial measurement of the slider position
    remeasureActiveSlider();
    const firstTab = tabBtnRefs.value?.[0];
    const tabContainer = firstTab?.parentElement;
    if (tabContainer && typeof ResizeObserver !== 'undefined') {
      tabResizeObserver = new ResizeObserver(() => {
        remeasureActiveSlider();
      });
      tabResizeObserver.observe(tabContainer);
    }
  });
});
onUnmounted(() => {
  detachEditorObserver();
  unbindShellInputListeners();
  window.removeEventListener('resize', handleWindowResize);
  if (tabResizeRaf) {
    cancelAnimationFrame(tabResizeRaf);
    tabResizeRaf = 0;
  }
  tabResizeObserver?.disconnect();
  tabResizeObserver = null;
  unsubscribeGiftEvent(LiveGiftEvents.ON_RECEIVE_GIFT_MESSAGE, handleGiftMessage);
  roomEngine.instance?.off(TUIRoomEvents.onRemoteUserEnterRoom, handleRemoteUserEnterRoom);
  roomEngine.instance?.off(TUIRoomEvents.onRemoteUserLeaveRoom, handleRemoteUserLeaveRoom);
});

watch(activeTab, (tab) => {
  if (tab !== 'chat') {
    inputFocused.value = false;
    return;
  }
  attachEditorObserver();
});
</script>

<style lang="scss" scoped>
/* ============================================================
 * Business Side Panel — Dual Tab (Chat + Audience)
 * All colors use --preset-* CSS variable tokens
 * ============================================================ */

.biz-side-panel {
  display: flex;
  flex-direction: column;
  width: 100%;
  height: 100%;
  min-height: 0;
  padding: 10px;
  box-sizing: border-box;
  background: var(--preset-panel-bg, #0c1018);
  border-left: 1px solid var(--preset-panel-border, rgba(255, 255, 255, 0.08));
}

.panel-card {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
  border-radius: 16px;
  background:
    linear-gradient(
      180deg,
      color-mix(in srgb, var(--preset-chat-surface-bg, #111a27) 86%, #000 14%) 0%,
      color-mix(in srgb, var(--preset-chat-surface-bg, #111a27) 92%, #000 8%) 100%
    );
  border: 1px solid var(--preset-chat-surface-border, rgba(255, 255, 255, 0.1));
  overflow: hidden;
  box-shadow: var(--preset-chat-surface-shadow);
}

/* ── Tab Bar ── */

.tab-bar {
  flex-shrink: 0;
  display: flex;
  align-items: center;
  padding: 10px 12px 8px;
  border-bottom: 1px solid var(--preset-chat-surface-divider, rgba(255, 255, 255, 0.1));
  background: linear-gradient(
    180deg,
    color-mix(in srgb, var(--preset-tab-bar-bg, #1a2535) 58%, transparent) 0%,
    color-mix(in srgb, var(--preset-tab-bar-bg, #1a2535) 36%, transparent) 100%
  );
}

.tab-bar-inner {
  position: relative;
  flex: 1;
  display: flex;
  border-radius: 12px;
  padding: 3px;
  background: color-mix(in srgb, var(--preset-tab-track-bg, rgba(24, 35, 62, 0.78)) 84%, var(--preset-chat-surface-bg, #111a27));
  border: 1px solid color-mix(in srgb, var(--preset-tab-track-border, rgba(255, 255, 255, 0.22)) 80%, transparent);
  gap: 2px;
  overflow: hidden;
}

/* Sliding pill background behind the active tab */
.tab-slider {
  position: absolute;
  top: 3px;
  bottom: 3px;
  border-radius: 10px;
  background: var(
    --preset-tab-slider-bg,
    linear-gradient(
      180deg,
      color-mix(in srgb, var(--preset-primary, #4c8bf5) 54%, #ffffff 46%) 0%,
      color-mix(in srgb, var(--preset-primary, #4c8bf5) 72%, #13254c 28%) 100%
    )
  );
  box-shadow:
    0 2px 10px color-mix(in srgb, var(--preset-primary, #4c8bf5) 24%, transparent),
    inset 0 1px 0 color-mix(in srgb, var(--uikit-color-white-1, #fff) 28%, transparent);
  transition:
    left 260ms cubic-bezier(0.2, 0.8, 0.2, 1),
    width 260ms cubic-bezier(0.2, 0.8, 0.2, 1),
    opacity 200ms ease;
  pointer-events: none;
  z-index: 0;
}

.tab-btn {
  position: relative;
  z-index: 1;
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 7px;
  padding: 7px 14px;
  border: none;
  border-radius: 10px;
  background: transparent;
  cursor: pointer;
  font-size: 13px;
  font-weight: 500;
  color: var(--preset-tab-btn-text, rgba(255, 255, 255, 0.82));
  letter-spacing: 0.01em;
  transition:
    color 280ms ease,
    transform 180ms ease;
  user-select: none;

  &:hover:not(.active) {
    color: var(--preset-tab-btn-hover-text, rgba(255, 255, 255, 0.94));
  }

  &:active {
    transform: scale(0.97);
  }

  &.active {
    color: var(--preset-tab-btn-active-text, #ffffff);
    font-weight: 650;
  }

  .tab-icon {
    display: inline-flex;
    width: 15px;
    height: 15px;
    transition:
      color 280ms ease,
      transform 280ms ease;

    svg {
      width: 100%;
      height: 100%;
      fill: currentColor;
    }
  }

  &.active .tab-icon {
    color: var(--preset-primary, #4c8bf5);
    transform: scale(1.08);
  }
}

.tab-label {
  white-space: nowrap;
  transition: letter-spacing 280ms ease;

  .tab-btn.active & {
    letter-spacing: 0.02em;
  }
}

.tab-badge {
  padding: 1px 7px;
  border-radius: 999px;
  font-size: 11px;
  font-weight: 600;
  background: var(--preset-tab-badge-bg, rgba(255, 255, 255, 0.16));
  color: var(--preset-tab-badge-text, rgba(255, 255, 255, 0.78));
  line-height: 1.4;
  transition:
    background 280ms ease,
    color 280ms ease;

  &.active {
    background: var(--preset-tab-badge-active-bg, color-mix(in srgb, var(--preset-primary, #4c8bf5) 55%, rgba(255, 255, 255, 0.28)));
    color: var(--preset-tab-badge-active-text, #ffffff);
  }
}

/* ── Tab Content Container ── */

.tab-content {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.tab-fade-enter-active {
  transition: opacity 220ms ease, transform 220ms ease;
}

.tab-fade-leave-active {
  transition: opacity 140ms ease, transform 140ms ease;
}

.tab-fade-enter-from {
  opacity: 0;
  transform: translateY(6px);
}

.tab-fade-leave-to {
  opacity: 0;
  transform: translateY(-4px);
}

/* ── Chat Panel ── */

.chat-panel {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.chat-list {
  position: relative;
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 12px 12px 16px;
  background: var(--preset-chat-surface-bg);
  user-select: text;
  -webkit-user-select: text;
}

.time-divider {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0;
  margin: 8px 0 6px;
}

.divider-line {
  display: none;
}

.divider-text {
  font-size: 11px;
  font-weight: 500;
  color: var(--preset-msg-time);
  white-space: nowrap;
  letter-spacing: 0.3px;
}

.system-msg {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 4px 12px;
  font-size: 13px;
  color: var(--preset-system-msg-text);

  .system-icon {
    flex-shrink: 0;
    color: var(--preset-system-msg-icon);
  }
}

.chat-msg {
  display: flex;
  gap: 8px;
  border-radius: 10px;
  padding: 4px 5px;
  transition: none;

  user-select: text;
  -webkit-user-select: text;
}

.msg-avatar {
  width: 32px;
  height: 32px;
  border-radius: 999px;
  flex-shrink: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 12px;
  font-weight: 700;
  overflow: hidden;
}

.msg-avatar-img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
}

.msg-main {
  min-width: 0;
  flex: 1;
}

.msg-meta-line {
  display: flex;
  align-items: center;
  gap: 6px;
  margin-bottom: 3px;
}

.msg-name {
  font-size: 13px;
  font-weight: 600;
  color: var(--preset-msg-name);

  &.host {
    color: var(--preset-msg-host-name);
  }

  &.self {
    color: var(--preset-role-self-text);
  }
}

.role-badge {
  padding: 1px 6px;
  border-radius: 4px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.3px;
  line-height: 1.5;
}

.host-badge {
  background: var(--preset-role-host-bg);
  color: var(--preset-role-host-text);
}

.msg-bubble {
  display: inline-block;
  max-width: 100%;
  white-space: pre-wrap;
  overflow-wrap: anywhere;
  word-break: break-word;
  padding: 7px 11px;
  border-radius: 10px;
  font-size: 14px;
  line-height: 1.45;
  color: var(--preset-msg-content-text);
  background: var(--preset-msg-content-bg);
  border: 1px solid var(--preset-msg-content-border);
  user-select: text;
  -webkit-user-select: text;

  &.host {
    border-color: var(--preset-msg-host-bubble-border);
    background: var(--preset-msg-host-bubble-bg);
  }

  &.self {
    background: var(--preset-msg-self-bg);
    border-color: var(--preset-msg-self-border);
  }
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
  color: var(--preset-msg-content-text);
}

.gift-name {
  font-weight: 600;
}

.gift-icon {
  width: 16px;
  height: 16px;
  vertical-align: middle;
}

/* ── Chat Input ── */

.chat-input-shell {
  flex-shrink: 0;
  position: relative;
  padding: 10px 12px 12px;
  border-top: 1px solid var(--preset-chat-surface-divider, rgba(255, 255, 255, 0.08));
  background: var(--preset-input-bar-bg, transparent);

  &.chat-disabled {
    .input-wrapper {
      opacity: 0.55;
      pointer-events: none;
    }
  }
}

.chat-disabled-hint {
  margin-bottom: 8px;
  text-align: center;
  font-size: 13px;
  color: var(--preset-text-tertiary);
}

.input-wrapper {
  display: flex;
  align-items: center;
  justify-content: flex-start;
  flex-wrap: nowrap;
  gap: 8px;
  border-radius: 14px;
  min-height: 40px;
  padding: 4px 8px 4px 10px;
  background: color-mix(in srgb, var(--preset-input-wrapper-bg, rgba(255, 255, 255, 0.04)) 88%, var(--preset-chat-surface-bg, #111a27));
  border: 1px solid color-mix(in srgb, var(--preset-input-wrapper-border, rgba(255, 255, 255, 0.1)) 92%, transparent);
  transition: all 180ms ease;

  &.focused {
    border-color: var(--preset-input-wrapper-focus-border, color-mix(in srgb, var(--preset-primary, #1c66e5) 45%, transparent));
    box-shadow: var(--preset-input-wrapper-focus-shadow, 0 0 0 3px rgba(28, 102, 229, 0.15));
  }

  &.disabled {
    pointer-events: none;
  }
}

:deep(.biz-barrage-input) {
  flex: 1;
  min-width: 0;
}

:deep(.biz-barrage-input .live-message-input) {
  width: 100%;
}

:deep(.biz-barrage-input .message-input-container) {
  height: 32px;
  min-height: 32px;
  max-height: 32px;
  padding: 0;
  border: none;
  border-radius: 0;
  background: transparent;
  overflow: hidden;
  display: flex;
  align-items: center;
}

:deep(.biz-barrage-input .input-wrapper) {
  align-items: center;
}

:deep(.biz-barrage-input .input-prefix) {
  display: flex;
  align-items: center;
  align-self: center;
}

:deep(.biz-barrage-input .input-actions) {
  height: 32px;
  display: flex;
  align-items: center;
  margin-right: 8px;
  gap: 0;
}

:deep(.biz-barrage-input .emoji-picker__icon) {
  width: 16px;
  height: 16px;
  color: var(--preset-emoji-btn-color);
}

:deep(.biz-barrage-input .tiptap.ProseMirror) {
  flex: 1;
  min-height: 22px;
  max-height: 72px;
  color: var(--preset-chat-input-text);
  font-size: 14px;
  line-height: 22px;
  font-family: inherit;

  p.is-editor-empty:first-child::before {
    color: var(--preset-chat-input-placeholder);
    font-size: 14px;
  }
}

/* ── Audience Panel ── */

.audience-panel {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.audience-list-scroll {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  padding: 10px 10px 16px;
}

.audience-group {
  margin-bottom: 16px;

  &:last-child {
    margin-bottom: 0;
  }
}

.group-items {
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.audience-row {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 7px 8px;
  border-radius: 10px;
  transition: background 200ms ease;

  &:hover {
    background: var(--preset-audience-hover-bg);
  }
}

.audience-avatar-wrap {
  position: relative;
  flex-shrink: 0;
}

.audience-avatar {
  width: 44px;
  height: 44px;
  border-radius: 999px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 15px;
  font-weight: 700;
  overflow: hidden;
}

.audience-avatar-img {
  width: 100%;
  height: 100%;
  display: block;
  object-fit: cover;
}

.audience-info {
  display: flex;
  align-items: center;
  gap: 6px;
  min-width: 0;
  flex: 1;
}

.audience-name {
  font-size: 15px;
  font-weight: 500;
  color: var(--preset-audience-name);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;

  &.host {
    color: var(--preset-role-host-text);
    font-weight: 600;
  }

  &.self {
    color: var(--preset-role-self-text);
    font-weight: 600;
  }
}

.audience-role-badge {
  flex-shrink: 0;
  padding: 1px 6px;
  border-radius: 4px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.3px;
  line-height: 1.5;
}

.host-role {
  background: var(--preset-role-host-bg);
  color: var(--preset-role-host-text);
}

.self-role {
  background: var(--preset-role-self-bg);
  color: var(--preset-role-self-text);
}

.show-more-btn {
  width: 100%;
  padding: 8px;
  margin-top: 4px;
  border: 1px solid var(--preset-chat-surface-divider);
  border-radius: 8px;
  background: transparent;
  color: var(--preset-tab-badge-text);
  font-size: 12px;
  font-weight: 600;
  cursor: pointer;
  transition: all 200ms ease;

  &:hover {
    background: var(--preset-audience-hover-bg);
    border-color: var(--preset-primary);
    color: var(--preset-primary);
  }
}

.audience-empty {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 48px 16px;
  gap: 12px;
  color: var(--preset-list-empty);

  svg {
    opacity: 0.4;
  }

  span {
    font-size: 13px;
  }
}

/* ── Audience list enter/leave animation ── */

.audience-item-enter-active {
  transition: opacity 300ms ease, transform 300ms ease;
}

.audience-item-leave-active {
  transition: opacity 200ms ease, transform 200ms ease;
}

.audience-item-enter-from {
  opacity: 0;
  transform: translateX(-12px);
}

.audience-item-leave-to {
  opacity: 0;
  transform: translateX(12px);
}
</style>

<style lang="scss">
/*
 * Scrollbar styling strategy:
 *
 * Use ::-webkit-scrollbar pseudo-elements as the primary approach for
 * Chrome, Safari (all versions including 18.2+), and Edge.
 *
 * IMPORTANT: In Safari 18.2+ and Chrome 121+, setting the standard
 * `scrollbar-width` property on an element causes the browser to
 * completely disable ::-webkit-scrollbar styling. Therefore we must
 * NOT set scrollbar-width/scrollbar-color anywhere that WebKit-based
 * browsers can see it.
 *
 * For Firefox (which doesn't support ::-webkit-scrollbar), we use
 * `scrollbar-width: thin` scoped behind `@supports (-moz-appearance: none)`
 * so only Firefox applies it.
 */

/* --- Firefox-only standard scrollbar fallback --- */
@supports (-moz-appearance: none) {
  .biz-side-panel .chat-list {
    scrollbar-width: thin;
    scrollbar-color: #e0e4ea transparent;
  }

  .biz-side-panel .audience-list-scroll {
    scrollbar-width: thin;
    scrollbar-color: #e2e6ec transparent;
  }

  .biz-side-panel .biz-barrage-input .tiptap.ProseMirror {
    scrollbar-width: thin;
    scrollbar-color: #e0e4ea transparent;
  }

  html[tui-theme-mode="dark"] .biz-side-panel .chat-list,
  html[tui-theme-mode="dark"] .biz-side-panel .audience-list-scroll,
  html[tui-theme-mode="dark"] .biz-side-panel .biz-barrage-input .tiptap.ProseMirror {
    scrollbar-color: #253550 transparent;
  }
}

/* --- Chat list scrollbar (WebKit / Blink) --- */

.biz-side-panel .chat-list::-webkit-scrollbar {
  -webkit-appearance: none;
  width: 6px;
  height: 6px;
}

.biz-side-panel .chat-list::-webkit-scrollbar-track {
  background: transparent;
}

.style-preset-business .biz-side-panel .chat-list::-webkit-scrollbar-thumb,
.style-preset-business.tui-theme-light .biz-side-panel .chat-list::-webkit-scrollbar-thumb {
  background-color: #e0e4ea;
  border-radius: 999px;
}

html[tui-theme-mode="dark"] .style-preset-business .biz-side-panel .chat-list::-webkit-scrollbar-thumb,
.style-preset-business.tui-theme-dark .biz-side-panel .chat-list::-webkit-scrollbar-thumb {
  background-color: #253550;
  border-radius: 999px;
}

/* --- Audience list scrollbar (WebKit / Blink) --- */

.biz-side-panel .audience-list-scroll::-webkit-scrollbar {
  -webkit-appearance: none;
  width: 6px;
  height: 6px;
}

.biz-side-panel .audience-list-scroll::-webkit-scrollbar-track {
  background: transparent;
}

.style-preset-business .biz-side-panel .audience-list-scroll::-webkit-scrollbar-thumb,
.style-preset-business.tui-theme-light .biz-side-panel .audience-list-scroll::-webkit-scrollbar-thumb {
  background-color: #e2e6ec;
  border-radius: 999px;
}

html[tui-theme-mode="dark"] .style-preset-business .biz-side-panel .audience-list-scroll::-webkit-scrollbar-thumb,
.style-preset-business.tui-theme-dark .biz-side-panel .audience-list-scroll::-webkit-scrollbar-thumb {
  background-color: #253550;
  border-radius: 999px;
}

/* --- Barrage input scrollbar (WebKit / Blink) --- */

.biz-side-panel .biz-barrage-input .tiptap.ProseMirror::-webkit-scrollbar {
  -webkit-appearance: none;
  width: 6px;
  height: 6px;
}

.biz-side-panel .biz-barrage-input .tiptap.ProseMirror::-webkit-scrollbar-track {
  background: transparent;
}

.style-preset-business .biz-side-panel .biz-barrage-input .tiptap.ProseMirror::-webkit-scrollbar-thumb,
.style-preset-business.tui-theme-light .biz-side-panel .biz-barrage-input .tiptap.ProseMirror::-webkit-scrollbar-thumb {
  background-color: #e0e4ea;
  border-radius: 999px;
}

// Dark theme
html[tui-theme-mode="dark"] .style-preset-business .biz-side-panel .biz-barrage-input .tiptap.ProseMirror::-webkit-scrollbar-thumb,
.style-preset-business.tui-theme-dark .biz-side-panel .biz-barrage-input .tiptap.ProseMirror::-webkit-scrollbar-thumb {
  background-color: #253550;
  border-radius: 999px;
}
</style>
