import { createRouter, createWebHashHistory, RouteRecordRaw } from 'vue-router'
import EmptyView from '../views/EmptyView.vue';
import LiveStudioView from '../views/LiveStudioView.vue';
import ChildWindowView from '../views/ChildWindowView.vue';

const routes: Array<RouteRecordRaw> = [
  {
    path: '/',
    name: 'empty',
    component: EmptyView,
  }, 
  {
    path: '/main-window',
    name: 'main-window',
    component: LiveStudioView
  },
  {
    path: '/child-window',
    name: 'child-window',
    component: ChildWindowView,
  }
];

const router = createRouter({
  history: createWebHashHistory(),
  routes
});

window.ipcRenderer.on('window-type', (event: any, type: string) => {
  console.log(`[router] window type:${type}`);
  console.log(`[router] current href:${window.location.href}`);
  router.replace({ name: `${type}-window`});
});

export default router
