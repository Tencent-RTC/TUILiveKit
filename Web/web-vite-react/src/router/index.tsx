import type { ReactNode } from 'react';
import { createHashRouter, Navigate, useLocation } from 'react-router-dom';
import { useLoginState } from 'tuikit-atomicx-react';
import { LiveList, LivePusher } from '@tencentcloud/livekit-web-react';
import { LivePlayerView, LoginView } from '../pages';
import { RoutePath } from './routePath';
import { logout } from '../utils/login';

export { RoutePath };

const logoUrl = new URL('../assets/logo.svg', import.meta.url).href;

function RequireLogin({ children }: { children: ReactNode }) {
  const { loginUserInfo } = useLoginState();
  const { pathname, search } = useLocation();
  if (loginUserInfo?.userId) {
    return children;
  }
  return <Navigate to={`${RoutePath.Login}?from=${encodeURIComponent(pathname + search)}`} replace />;
}

export const router = createHashRouter([{
  path: RoutePath.Home,
  element: <Navigate to={RoutePath.LiveList} replace />,
}, {
  path: RoutePath.Login,
  element: <LoginView />,
}, {
  path: RoutePath.LiveList,
  element: (
    <RequireLogin>
      <LiveList
        logoUrl={logoUrl}
        brandName="LiveKit"
        onStartLive={() => router.navigate(RoutePath.LivePusher)}
        onLiveRoomClick={liveInfo => router.navigate({ pathname: RoutePath.LivePlayer, search: `?liveId=${liveInfo.liveId}` })}
        onLogout={async () => {
          await logout();
          await router.navigate(RoutePath.Login);
        }}
      />
    </RequireLogin>
  ),
}, {
  path: RoutePath.LivePusher,
  element: (
    <RequireLogin>
      <LivePusher
        logoUrl={logoUrl}
        brandName="LiveKit"
        onLeave={() => router.navigate(RoutePath.LiveList)}
        onLogout={async () => {
          await logout();
          await router.navigate(RoutePath.Login);
        }}
      />
    </RequireLogin>
  ),
}, {
  path: RoutePath.LivePlayer,
  element: <RequireLogin><LivePlayerView /></RequireLogin>,
}]);
