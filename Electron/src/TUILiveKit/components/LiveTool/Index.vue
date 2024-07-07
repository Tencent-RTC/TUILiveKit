<template>
  <div class="tui-live-tool">
    <div class="tui-title">
      <span>
        {{t('Live Streaming Tools')}}
      </span>
      <div class="tui-title-left" @click="handleShowTools">
        <svg-icon :icon=ArrowDownIcon></svg-icon>
        <span class="tui-title-left-text">
          {{t('Put away')}}
        </span>
      </div>
    </div>
    <div class="tui-tool-list" v-if="isShowTools">
      <span class="tools-container" v-for="(item, index) in liveStreamToolsList" :key="index"  @click="item.fun()">
          <svg-icon :icon="item.icon"></svg-icon>
          <i class="text">
            {{item.text}}
          </i>
      </span>
    </div>
  </div>
</template>
<script setup lang="ts">
import { useI18n } from '../../locales';
import BeautyIcon from '../../common/icons/BeautyIcon.vue';
import PKIcon from '../../common/icons/PKIcon.vue';
import RecordIcon from '../../common/icons/RecordIcon.vue';
import SoundEffectIcon from '../../common/icons/SoundEffectIcon.vue';
import VoiceChatIcon from '../../common/icons/VoiceChatIcon.vue';
import VoiceEffectIcon from '../../common/icons/VoiceEffectIcon.vue';
import ArrowDownIcon from '../../common/icons/ArrowDownIcon.vue'
import SvgIcon from '../../common/base/SvgIcon.vue';
import { ref } from 'vue';
const { t } = useI18n();
const isShowTools = ref(true);
const liveStreamToolsList = ref([
  {
    icon: SoundEffectIcon,
    text: t('Sound effect'),
    fun: handleSoundEffect    
  },
  {
    icon: VoiceEffectIcon,
    text: t('Voice effect'),
    fun: handleVoiceEffect    
  },
  {
    icon: RecordIcon,
    text: t('Record'),
    fun: handleRecord  
  }, 
])


function handleSoundEffect() {
  console.log('SoundEffect') 
}

function handleVoiceEffect() {
  console.log('VoiceEffect')
}

function handleRecord() {
  console.log('Record')
}

const handleShowTools = () => {
  isShowTools.value = !isShowTools.value
}


</script>

<style scoped lang="scss">
.tui-title{
  display: flex;
  justify-content: space-between;
  cursor: pointer;
}
.tui-title-left{
  display: flex;
  align-items: center;
  cursor: pointer;
    &-text{
      text-align: center;
      font-size: 0.75rem;
      font-style: normal;
      font-weight: 400;
      line-height: 1.25rem; /* 166.667% */
      background: var(--G2, linear-gradient(180deg, #A4BBDB 0%, rgba(164, 187, 219, 0.80) 100%));
      background-clip: text;
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
    }
}
.tui-tool-list{
  display: flex;
  flex-wrap: wrap;
  padding-top: 0.75rem;
}
.tools-container{
  flex: 0 0 25%;
  box-sizing: border-box;
  text-align: center;
  display: flex;
  flex-direction: column;
  padding-top: 0.75rem;
  cursor: pointer;
}
.text{
  text-align: center;
  font-size: 0.75rem;
  font-style: normal;
  font-weight: 400;
  line-height: 1.25rem; /* 166.667% */
  background: var(--G2, linear-gradient(180deg, #A4BBDB 0%, rgba(164, 187, 219, 0.80) 100%));
  background-clip: text;
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  padding-top: 0.25rem;
}
</style>