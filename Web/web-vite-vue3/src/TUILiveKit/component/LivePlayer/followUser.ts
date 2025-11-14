import { ref, computed } from 'vue';
import { useRoomEngine } from 'tuikit-atomicx-vue3';
import { useUIKit, TUIToast, TOAST_TYPE } from '@tencentcloud/uikit-base-component-vue3';

const { t } = useUIKit();
const roomEngine = useRoomEngine();

const isFollowed = ref(false);
const followButtonText = computed(() => (isFollowed.value ? t('Followed') : t('Follow')));

export enum FollowType {
  None = 0,
  Fans = 1,
  Follow = 2,
  Both = 3,
}

export async function getFollowInfo(userId: string) {
  console.log('getFollowInfo, userId:', userId);
  const tim = roomEngine.instance?.getTIM();
  try {
    const followInfo = await tim?.checkFollowType([userId]);
    if (!followInfo) {
      console.error('Failed to get follow info');
      showErrorInfo(t('Failed to get follow info'));
      return;
    }
    const { data } = followInfo;
    if (data && data.length > 0) {
      isFollowed.value = data[0].followType === FollowType.Follow || data[0].followType === FollowType.Both;
    }
  } catch (error) {
    console.error('Failed to get follow info:', error);
    showErrorInfo(t('Failed to get follow info'));
  }
}

export async function followUser(userId: string) {
  if (isFollowed.value === true) {
    return;
  }
  const tim = roomEngine.instance?.getTIM();
  try {
    await tim?.followUser([userId]);
    isFollowed.value = true;
  } catch (error) {
    console.error('Failed to follow user:', error);
    showErrorInfo(t('Failed to follow user'));
  }
}

export async function unfollowUser(userId: string) {
  if (isFollowed.value === false) {
    return;
  }
  const tim = roomEngine.instance?.getTIM();
  try {
    await tim?.unfollowUser([userId]);
    isFollowed.value = false;
  } catch (error) {
    console.error('Failed to unfollow user:', error);
    showErrorInfo(t('Failed to unfollow user'));
  }
}

function showErrorInfo(error: string) {
  TUIToast({
    type: TOAST_TYPE.ERROR,
    message: error,
    duration: 3000,
  });
}

export { followButtonText, isFollowed };
