<template>
  <live-main-view></live-main-view>
</template>

<script setup lang="ts">
import { onMounted, onUnmounted } from 'vue';
import { LiveMainView, liveRoom, RoomEvent, LanguageOption, ThemeOption } from '@tencentcloud/livekit-web-vue3';
import { onBeforeRouteLeave, useRoute } from 'vue-router';
import router from '@/router';
import i18n, { useI18n } from '../locales/index';
import { getLanguage, getTheme } from '../utils/utils';

const route = useRoute();
const { t } = useI18n();
const roomInfo = sessionStorage.getItem('tuiLive-roomInfo');
const userInfo = sessionStorage.getItem('tuiLive-userInfo');
const roomId = String(route.query.roomId);
liveRoom.setLanguage(getLanguage() as LanguageOption);
liveRoom.setTheme(getTheme() as ThemeOption);
let isAnchor = String(route.query.role) !== 'audience';
let isExpectedJump = false;

if (!roomId) {
  router.push({ path: 'home' });
} else if (!roomInfo) {
  if (isAnchor) {
    router.push({ path: 'home', query: { roomId } });
  }
}

async function handleAnchorInitLogic() {
  const { isSeatEnabled, roomParam, hasCreated } = JSON.parse(roomInfo as string);
  const { sdkAppId, userId, userSig, userName, avatarUrl } = JSON.parse(userInfo as string);
  try {
    await liveRoom.login({ sdkAppId, userId, userSig });
    await liveRoom.setSelfInfo({ userName, avatarUrl });
    if (!hasCreated) {
      await liveRoom.start(roomId, {
        roomName: `${userName || userId}`,
        isSeatEnabled,
        ...roomParam,
      });
      const newRoomInfo = { roomId, roomName: roomId, isSeatEnabled, roomParam, hasCreated: true };
      sessionStorage.setItem('tuiLive-roomInfo', JSON.stringify(newRoomInfo));
    } else {
      await liveRoom.join(roomId, roomParam);
    }
  } catch (error: any) {
    sessionStorage.removeItem('tuiLive-currentUserInfo');
  }
}

onMounted(async () => {
  if (isAnchor) {
    handleAnchorInitLogic();
  } else {
    await liveRoom.join(roomId);
  }
});

onBeforeRouteLeave((to: any, from: any, next: any) => {
  if (!isExpectedJump) {
    const message = isAnchor
      ? t('This action causes the room to be disbanded, does it continue?')
      : t('This action causes the room to be exited, does it continue?');
    if (window.confirm(message)) {
      if (isAnchor) {
        liveRoom?.dismiss();
      } else {
        liveRoom?.leave();
      }
      next();
    } else {
      next(false);
    }
  } else {
    next();
  }
});

const backToPage = (page: string, shouldClearUserInfo: boolean) => {
  sessionStorage.removeItem('tuiLive-roomInfo');
  shouldClearUserInfo && sessionStorage.removeItem('tuiLive-currentUserInfo');
  goToPage(page);
};
const backToHome = () => backToPage('home', false);
const backToHomeAndClearUserInfo = () => backToPage('home', true);
const changeLanguage = (language: LanguageOption) => {
  i18n.global.locale.value = language;
  localStorage.setItem('tuiLive-language', language);
};
const changeTheme = (theme: ThemeOption) => {
  localStorage.setItem('tuiLive-currentTheme', theme);
};
liveRoom.on(RoomEvent.ROOM_DISMISS, backToHome);
liveRoom.on(RoomEvent.ROOM_LEAVE, backToHome);
liveRoom.on(RoomEvent.KICKED_OUT, backToHome);
liveRoom.on(RoomEvent.ROOM_ERROR, backToHome);
liveRoom.on(RoomEvent.KICKED_OFFLINE, backToHome);
liveRoom.on(RoomEvent.USER_SIG_EXPIRED, backToHomeAndClearUserInfo);
liveRoom.on(RoomEvent.USER_LOGOUT, backToHomeAndClearUserInfo);
liveRoom.on(RoomEvent.LANGUAGE_CHANGED, changeLanguage);
liveRoom.on(RoomEvent.THEME_CHANGED, changeTheme);

onUnmounted(() => {
  liveRoom.off(RoomEvent.ROOM_DISMISS, backToHome);
  liveRoom.off(RoomEvent.ROOM_LEAVE, backToHome);
  liveRoom.off(RoomEvent.KICKED_OUT, backToHome);
  liveRoom.off(RoomEvent.ROOM_ERROR, backToHome);
  liveRoom.off(RoomEvent.KICKED_OFFLINE, backToHome);
  liveRoom.off(RoomEvent.USER_SIG_EXPIRED, backToHomeAndClearUserInfo);
  liveRoom.off(RoomEvent.USER_LOGOUT, backToHomeAndClearUserInfo);
  liveRoom.off(RoomEvent.LANGUAGE_CHANGED, changeLanguage);
  liveRoom.off(RoomEvent.THEME_CHANGED, changeTheme);
});

const goToPage = (routePath: string) => {
  isExpectedJump = true;
  router.replace({ path: routePath });
};
</script>

<style lang="scss">
#app {
  position: relative;
  width: 100%;
  height: 100%;
  font-family: 'PingFang SC';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
</style>
