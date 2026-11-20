import { useEffect } from 'react';
import type { ReactNode } from 'react';
import { useSearchParams } from 'react-router-dom';
import { useLiveListState } from 'tuikit-atomicx-react';

export function LiveIdQuerySync({ children }: { children: ReactNode }) {
  const { currentLive } = useLiveListState();
  const [searchParams, setSearchParams] = useSearchParams();
  const liveId = currentLive?.liveId || '';

  useEffect(() => {
    if ((searchParams.get('liveId') || '') !== liveId) {
      setSearchParams(liveId ? { liveId } : {}, { replace: true });
    }
  }, [liveId, searchParams, setSearchParams]);

  return children;
}
