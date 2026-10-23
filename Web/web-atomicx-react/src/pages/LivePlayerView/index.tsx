import { useNavigate, useSearchParams } from 'react-router-dom';
import { LivePlayerView as KitLivePlayerView } from '@tencentcloud/livekit-web-react';
import { RoutePath } from '../../router';
import { logout } from '../../utils/login';

export function LivePlayerView() {
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const liveId = searchParams.get('liveId') ?? '';

  return (
    <KitLivePlayerView
      liveId={liveId}
      onLeave={() => navigate(RoutePath.LiveList)}
      onLogout={async () => {
        await logout();
        await navigate(RoutePath.Login);
      }}
    />
  );
}
