import { watch } from 'vue';
import { createRouter, createWebHashHistory } from 'vue-router';
import { useLoginState } from 'tuikit-atomicx-vue3';
import { LivePusherView } from '@tencentcloud/livekit-web-vue3';
import { LiveListView, LivePlayerView, LoginView } from '../pages';
import { RoutePath } from './routePath';
import { logout } from '../utils/login';

const logoUrl = new URL('../assets/logo.svg', import.meta.url).href;

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
    props: {
      logoUrl,
      brandName: 'LiveKit',
      onLeave: () => router.push(RoutePath.LiveList),
      onLogout: async () => {
        await logout();
        await router.push(RoutePath.Login);
      },
    },
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

watch(() => loginUserInfo.value?.userId, (userId) => {
  const { path, fullPath } = router.currentRoute.value;
  if (!userId && path !== RoutePath.Login) {
    router.replace({ path: RoutePath.Login, query: { from: fullPath } });
  }
});

export { RoutePath };
export default router;
