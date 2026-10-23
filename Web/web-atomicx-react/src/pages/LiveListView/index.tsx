import { useNavigate } from 'react-router-dom';
import { Avatar, LiveList, useLoginState } from 'tuikit-atomicx-react';
import { useUIKit } from '@tencentcloud/uikit-base-component-react';
import { Button, ButtonSize, ButtonVariant } from '@tencentcloud/livekit-web-react';
import { RoutePath } from '../../router';
import { logout } from '../../utils/login';
import './LiveListView.scss';

export function LiveListView() {
  const navigate = useNavigate();
  const { t } = useUIKit();
  const { loginUserInfo } = useLoginState();

  const handleLogout = async () => {
    await logout();
    navigate(RoutePath.Login);
  };

  return (
    <div className="live-list-page" data-lk-theme="dark">
      <header className="live-list-header">
        <div className="live-list-brand">
          <span className="live-list-logo" />
          LiveKit
        </div>
        <div className="live-list-account">
          <Button
            className="live-list-start"
            variant={ButtonVariant.Primary}
            size={ButtonSize.Small}
            onClick={() => navigate(RoutePath.LivePusher)}
          >
            {t('Demo.StartLive')}
          </Button>
          <Avatar src={loginUserInfo?.avatarUrl} size={24} />
          <span className="live-list-user-name">{loginUserInfo?.userName || loginUserInfo?.userId}</span>
          <Button size={ButtonSize.Small} onClick={handleLogout}>{t('Demo.Logout')}</Button>
        </div>
      </header>
      <LiveList onLiveRoomClick={liveInfo => navigate({ pathname: RoutePath.LivePlayer, search: `?liveId=${liveInfo.liveId}` })} />
    </div>
  );
}
