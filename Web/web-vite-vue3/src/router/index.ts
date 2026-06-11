import { createRouter, createWebHashHistory } from 'vue-router';
import { useLoginState } from 'tuikit-atomicx-vue3';
import Login from '@/views/login.vue';
import { isH5 } from '@/TUILiveKit/utils/environment';

const routes = [
  {
    path: '/',
    redirect: '/live-list',
  },
  {
    path: '/login',
    component: Login,
  },
  {
    path: '/live-list',
    component: () => import('@/views/live-list.vue'),
  },
  {
    path: '/live-player',
    component: () => import('@/views/live-player.vue'),
  },
  {
    path: '/live-pusher',
    component: () => import('@/views/live-pusher.vue'),
  },
  // Business style routes — isolated under /business prefix
  {
    path: '/business/live-player',
    component: () => import('@/scenes/business/views/business-live-player.vue'),
  },
  // Education style routes — isolated under /education prefix
  {
    path: '/education/live-player',
    component: () => import('@/scenes/education/views/education-live-player.vue'),
  },
];

const router = createRouter({
  history: createWebHashHistory(),
  routes,
});

type StylePreset = '' | 'business' | 'education';

function normalizeStylePreset(value: unknown): StylePreset {
  if (value === 'business' || value === 'education') {
    return value;
  }
  return '';
}

function getActiveStylePreset(routeQuery: Record<string, any>): StylePreset {
  if (isH5) {
    return '';
  }
  const queryPreset = normalizeStylePreset(routeQuery.stylePreset);
  if (queryPreset) {
    return queryPreset;
  }
  const stored = localStorage.getItem('tuikit-style-preset');
  return normalizeStylePreset(stored);
}

let restoreLoginPromise: Promise<void> | null = null;

async function restoreLoginIfNeeded(): Promise<void> {
  const { loginUserInfo, login } = useLoginState();
  if (loginUserInfo.value?.userId) {
    return;
  }
  if (restoreLoginPromise) {
    return restoreLoginPromise;
  }
  const stored = sessionStorage.getItem('tuiLive-userInfo');
  if (!stored) {
    return;
  }
  restoreLoginPromise = (async () => {
    try {
      const liveUserInfo = JSON.parse(stored);
      if (!liveUserInfo?.userID || !liveUserInfo?.userSig || !liveUserInfo?.SDKAppID) {
        sessionStorage.removeItem('tuiLive-userInfo');
        return;
      }
      await login({
        userId: liveUserInfo.userID,
        userSig: liveUserInfo.userSig,
        sdkAppId: liveUserInfo.SDKAppID,
        testEnv: localStorage.getItem('tuikit-live-env') === 'TestEnv',
      });
    } catch (error) {
      console.error('[router] Failed to restore login state:', error);
      sessionStorage.removeItem('tuiLive-userInfo');
    } finally {
      restoreLoginPromise = null;
    }
  })();
  return restoreLoginPromise;
}

router.beforeEach(async (to, _from, next) => {
  if (to.path === '/login') {
    next();
    return;
  }
  const userInfo = sessionStorage.getItem('tuiLive-userInfo');
  if (!userInfo) {
    next({ path: '/login', query: { ...to.query, from: to.path } });
    return;
  }

  await restoreLoginIfNeeded();
  if (!useLoginState().loginUserInfo.value?.userId) {
    next({ path: '/login', query: { ...to.query, from: to.path } });
    return;
  }

  if (isH5) {
    if (to.path === '/business/live-player' || to.path === '/education/live-player') {
      const query = { ...to.query };
      delete query.stylePreset;
      next({ path: '/live-player', query });
      return;
    }
    if (to.query.stylePreset) {
      const query = { ...to.query };
      delete query.stylePreset;
      next({ path: to.path, query });
      return;
    }
  }

  // Redirect to style route when style preset is active
  // (from URL param or UIKitProvider stylePreset prop)
  const activeStylePreset = getActiveStylePreset(to.query);
  if (to.path === '/live-player' && activeStylePreset) {
    const query = { ...to.query };
    delete query.stylePreset;
    next({ path: `/${activeStylePreset}/live-player`, query });
    return;
  }

  // Strip stylePreset param if already on style route to avoid re-render
  if ((to.path === '/business/live-player' || to.path === '/education/live-player') && to.query.stylePreset) {
    const query = { ...to.query };
    delete query.stylePreset;
    next({ path: to.path, query });
    return;
  }

  next();
});

export default router;
