import { createRouter, createWebHashHistory } from 'vue-router';
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

router.beforeEach((to, _from, next) => {
  if (to.path === '/login') {
    next();
    return;
  }
  const userInfo = sessionStorage.getItem('tuiLive-userInfo');
  if (!userInfo) {
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
