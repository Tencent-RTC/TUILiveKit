import { createRouter, createWebHashHistory } from 'vue-router';
import Login from '@/views/login.vue';
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
];

const router = createRouter({
  history: createWebHashHistory(),
  routes,
});

router.beforeEach((to, from, next) => {
  if (to.path === '/login') {
    next();
    return;
  }
  const userInfo = localStorage.getItem('tuiLive-userInfo');
  if (!userInfo) {
    if (routes.some(route => route.path === from.path) && from.path !== '/login') {
      next({ path: '/login', query: { from: to.path } });
    } else {
      next('/login');
    }
    return;
  }

  next();
});

export default router;
