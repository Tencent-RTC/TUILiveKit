<template>
    <div class="tui-member-control">
        <div 
          class="tui-member-control-container"
          :class="{'danger': index === controlList.length - 1}"
          v-for="(item, index) in controlList"
          :key="index" 
          @click="item.fun()"
          
        >
            <svg-icon :icon="item.icon"></svg-icon>
            <span class="tui-member-control-options" >{{item.text}}</span>
        </div>
    </div>
</template>
<script setup lang="ts">
import { ref, defineProps, defineEmits } from 'vue';
import { useI18n } from '../../locales';
import SvgIcon from '../../common/base/SvgIcon.vue';
import ViewProfileIcon from '../../common/icons/ViewProfileIcon.vue';
import UnMuteIcon from '../../common/icons/UnMuteIcon.vue';
import CloseCameraIcon from '../../common/icons/CloseCameraIcon.vue';
import MicropositionIcon from '../../common/icons/MicropositionIcon.vue';
import CancelMikeIcon from '../../common/icons/CancelMikeIcon.vue';
import BlacklistIcon from '../../common/icons/BlacklistIcon.vue';
import KickedIcon from '../../common/icons/KickedIcon.vue';
import useGetRoomEngine from '../../utils/useRoomEngine';

const logger = console;

const logPrefix = '[LiveMemberControl]';

interface Props {
  userId: string;
}

const roomEngine = useGetRoomEngine();

const emit = defineEmits([
  "on-close",
]);
const props = defineProps<Props>();
const { t } = useI18n();
const controlList = ref([
  // {
  //   icon: ViewProfileIcon,
  //   text: t('View Profile'),
  //   fun: handleViewProfile,
  // },
  // {
  //   icon: MicropositionIcon,
  //   text: t('Move seat'),
  //   fun: handleMoveSeat,
  // },
  {
    icon: CancelMikeIcon,
    text: t('Kick seat'),
    fun: handleKickSeat,
  },
  // {
  //   icon: BlacklistIcon,
  //   text: t('Blacklist'),
  //   fun: handleBlackList,
  // },
  {
    icon: KickedIcon,
    text: t('Kicked off'),
    fun: handleKickOut,
  },
])

function handleViewProfile() {
  console.log(props.userId,'view profile')
}

function handleMoveSeat() {
  console.log('mobile seat')
}

function handleKickSeat() {
  window.mainWindowPort?.postMessage({
    key: "cancelWheatPosition",
    data: {
      userId: props.userId,
    }
  });
  logger.log(`${logPrefix}cancelWheatPosition`);
  emit('on-close');
}

function handleBlackList() {
  console.log('blackList')
}

async function handleKickOut() {
  window.mainWindowPort?.postMessage({
    key: "kickOut",
    data: {
      userId: props.userId,
    }
  });
  logger.log(`${logPrefix}kickOut`)
  emit('on-close');
}
</script>

<style lang="scss" scoped>
@import '../../assets/variable.scss';

.tui-member-control{
  width: 7.5rem;
	height: 4rem;
  border-radius: 0.25rem;
  position: absolute;
	right: 0;
	top: 2.5rem;
  z-index: 1;
	flex-shrink: 0;
	background-color: #2D323C;
	
  &-container{
    display: flex;
    height: 2rem;
    line-height: 2rem;
    padding-left: 1rem;
    cursor: pointer;

    &:hover {
      background-color: $color-black;
    }
  }
  &-options{
    padding-left:0.625rem;
    font-size: 0.75rem;
    font-style: normal;
    font-weight: 400;
  }

  .danger {
    color: $color-danger;
  }
}
</style>