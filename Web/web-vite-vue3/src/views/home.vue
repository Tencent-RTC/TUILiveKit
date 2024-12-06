<template>
  <pre-live-view
    :user-info="userInfo"
    :room-id="givenRoomId"
    :enable-scheduled-live="true"
    @on-create-room="handleCreateLive"
    @on-enter-room="handleEnterLive"
    @on-logout="handleLogOut"
    @on-update-user-name="handleUpdateUserName"
  ></pre-live-view>
</template>

<script setup lang="ts">
import { TUIRoomType } from "@tencentcloud/tuiroom-engine-js";
import { PreLiveView, liveRoom, RoomEvent, LanguageOption, ThemeOption } from '@tencentcloud/livekit-web-vue3';
import { getBasicInfo } from '@/config/basic-info-config';
import router from '@/router';
import { useRoute } from 'vue-router';
import { Ref, ref, reactive, onMounted, onUnmounted } from 'vue';
import i18n from '../locales/index';
import { getLanguage, getTheme } from  '../utils/utils';

const route = useRoute();
const givenRoomId: Ref<string> = ref((route.query.roomId) as string);

const userInfo = reactive({
  userId: '',
  userName: '',
  avatarUrl: '',
});


function setTUILiveData(roomOption: Record<string, any>) {
  sessionStorage.setItem('tuiLive-roomInfo', JSON.stringify({
    ...roomOption,
  }));
}

async function checkLiveExistWhenCreateLive(roomId: string) {
  try {
    await liveRoom.getRoomEngine()?.fetchRoomInfo({ roomId, roomType: TUIRoomType.kLive });
    return true;
  } catch (error: any) {
    return false;
  }
}

/**
 * Generate liveRoom number when creating a liveRoom
**/
async function generateRoomId(): Promise<string> {
  const roomId = String(Math.ceil(Math.random() * 1000000));
  const isLiveExist = await checkLiveExistWhenCreateLive(String(roomId));
  if (isLiveExist) return await generateRoomId();
  return roomId;
}

/**
 * Processing Click [Create Room]
**/
async function handleCreateLive(roomOption: Record<string, any>) {
  setTUILiveData(roomOption);
  const roomId = await generateRoomId();
  router.push({
    path: 'live',
    query: {
      roomId,
    },
  });
}

/**
 * Processing Click [Enter Room]
**/
async function handleEnterLive(roomOption: Record<string, any>) {
  setTUILiveData(roomOption);
  router.push({
    path: 'live',
    query: {
      roomId: roomOption.roomId,
    },
  });
}

function handleUpdateUserName(userName: string) {
  try {
    const currentUserInfo = JSON.parse(sessionStorage.getItem('tuiLive-userInfo') as string);
    currentUserInfo.userName = userName;
    sessionStorage.setItem('tuiLive-userInfo', JSON.stringify(currentUserInfo));
  } catch (error) {
    console.log('sessionStorage error', error);
  }
}

/**
 * Processing users click [Logout Login] in the upper left corner of the page
**/
async function handleLogOut() {
/**
 * The accessor handles the logout method
**/
}

async function handleInit() {
  sessionStorage.removeItem('tuiLive-roomInfo');
  sessionStorage.removeItem('tuiLive-userInfo');
  liveRoom.setLanguage(getLanguage() as LanguageOption);
  liveRoom.setTheme(getTheme() as ThemeOption);
  let currentUserInfo = null;
  if (sessionStorage.getItem('tuiLive-userInfo')) {
    currentUserInfo = JSON.parse(sessionStorage.getItem('tuiLive-userInfo') as string);
  } else {
    currentUserInfo = await getBasicInfo();
    currentUserInfo && sessionStorage.setItem('tuiLive-userInfo', JSON.stringify(currentUserInfo));
  }
  userInfo.userId = currentUserInfo?.userId;
  userInfo.userName = currentUserInfo?.userName;
  userInfo.avatarUrl = currentUserInfo?.avatarUrl;
  const { userId, sdkAppId, userSig } = currentUserInfo;
  await liveRoom.login({ sdkAppId, userId, userSig });
  if (givenRoomId.value) {
    router.push({
      path: 'live',
      query: {
        roomId: givenRoomId.value,
        role: 'audience'
      }
    })
  }
}

const changeLanguage = (language: LanguageOption) => {
  i18n.global.locale.value = language;
  localStorage.setItem('tuiLive-language', language);
};
const changeTheme = (theme: ThemeOption) => {
  localStorage.setItem('tuiLive-currentTheme', theme);
};
onMounted(() => {
  liveRoom.on(RoomEvent.LANGUAGE_CHANGED, changeLanguage);
  liveRoom.on(RoomEvent.THEME_CHANGED, changeTheme);
});

onUnmounted(() => {
  liveRoom.off(RoomEvent.LANGUAGE_CHANGED, changeLanguage);
  liveRoom.off(RoomEvent.THEME_CHANGED, changeTheme);
});

handleInit();

</script>
