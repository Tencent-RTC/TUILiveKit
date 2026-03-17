import { createRouter, createWebHashHistory } from 'vue-router';
import Login from '@/views/login.vue';
import { isBusinessPresetFromUrl } from '@/business/composables/useBusinessPreset';

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
    component: () => import('@/business/views/business-live-player.vue'),
  },
];

const router = createRouter({
  history: createWebHashHistory(),
  routes,
});

function isBusinessPresetActive(routeQuery: Record<string, any>): boolean {
  return routeQuery.stylePreset === 'business' || isBusinessPresetFromUrl();
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

  // Redirect to business route when the business preset is active
  // (from URL param or UIKitProvider stylePreset prop)
  if (to.path === '/live-player' && isBusinessPresetActive(to.query)) {
    const query = { ...to.query };
    delete query.stylePreset;
    next({ path: '/business/live-player', query });
    return;
  }

  // Strip stylePreset param if already on the business route to avoid re-render
  if (to.path === '/business/live-player' && to.query.stylePreset) {
    const query = { ...to.query };
    delete query.stylePreset;
    next({ path: '/business/live-player', query });
    return;
  }

  next();
});

export default router;
