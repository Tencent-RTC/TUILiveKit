<template>
  <div class="login-container">
    <Login
      class="login-widget" v-bind="{
        SDKAppID: SDKAPPID,
        generatorUserSig: genTestUserSig,
        onLoginCallback: handleLogin }"
    />
  </div>
</template>
<script setup lang="ts">
import Login from '../components/LoginUserID.vue';
import { useRouter, useRoute } from 'vue-router';
import { deepClone } from '../utils/utils';
import { SDKAPPID, genTestUserSig } from '../config/basic-info-config';

const router = useRouter();
const route = useRoute();

const handleLogin = (userInfo: {
  SDKAppID: string;
  userID: string;
  userSig: string;
}) => {
  localStorage.setItem('tuiLive-userInfo', JSON.stringify(userInfo));
  const currentQuery = deepClone(route.query);
  delete currentQuery.from;
  router.push({ path: route.query.from as string || '/live-list', query: currentQuery });
};

</script>

<style scoped>
.login-container {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 100%;
  background: url('../assets/login-back.png') no-repeat center center;
  background-color: black;
  background-size: cover;

  @media screen and (orientation: portrait), (orientation: landscape) {
    :deep(.phone-prefix) {
      min-width: 30px;
    }
  }
}
</style>
