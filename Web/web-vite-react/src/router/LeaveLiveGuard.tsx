import { useEffect } from 'react';
import type { ReactNode } from 'react';
import { useBlocker } from 'react-router-dom';
import { useLoginState } from 'tuikit-atomicx-react';
import { requestLeaveLive } from '@tencentcloud/livekit-web-react';

export function LeaveLiveGuard({ children }: { children: ReactNode }) {
  const { loginUserInfo } = useLoginState();
  const isLoggedIn = !!loginUserInfo?.userId;
  const blocker = useBlocker(({ currentLocation, nextLocation }) => isLoggedIn && currentLocation.pathname !== nextLocation.pathname);

  useEffect(() => {
    if (blocker.state !== 'blocked') {
      return;
    }
    requestLeaveLive().then((canLeave) => {
      if (canLeave) {
        blocker.proceed();
      } else {
        blocker.reset();
      }
    });
  }, [blocker]);

  return children;
}
