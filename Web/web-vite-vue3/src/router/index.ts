import { watch } from 'vue';
import { createRouter, createWebHashHistory } from 'vue-router';
import { useLoginState } from 'tuikit-atomicx-vue3';
import { requestLeaveLive } from '@tencentcloud/livekit-web-vue3';
import { LiveListView, LivePlayerView, LivePusherView, LoginView } from '../pages';
import { RoutePath } from './routePath';

const router = createRouter({
  history: createWebHashHistory(),
  routes: [{
    path: RoutePath.Home,
    redirect: RoutePath.LiveList,
  }, {
    path: RoutePath.Login,
    component: LoginView,
  }, {
    path: RoutePath.LiveList,
    component: LiveListView,
  }, {
    path: RoutePath.LivePusher,
    component: LivePusherView,
  }, {
    path: RoutePath.LivePlayer,
    component: LivePlayerView,
    props: route => ({ liveId: String(route.query.liveId || '') }),
  }],
});

const { loginUserInfo } = useLoginState();

router.beforeEach((to) => {
  if (to.path === RoutePath.Login || loginUserInfo.value?.userId) {
    return true;
  }
  return { path: RoutePath.Login, query: { from: to.fullPath } };
});

const LIVE_ROUTES: string[] = [RoutePath.LivePusher, RoutePath.LivePlayer];

router.beforeEach((to, from) => {
  if (!LIVE_ROUTES.includes(from.path) || to.path === from.path || !loginUserInfo.value?.userId) {
    return true;
  }
  return requestLeaveLive();
});

watch(() => loginUserInfo.value?.userId, (userId) => {
  const { path, fullPath } = router.currentRoute.value;
  if (!userId && path !== RoutePath.Login) {
    router.replace({ path: RoutePath.Login, query: { from: fullPath } });
  }
});

export { RoutePath };
export default router;
