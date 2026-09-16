<template>
  <Teleport to="body">
    <div v-if="visible && variant" class="pkg-page">
      <div class="stage">
        <button type="button" class="back" @click="close">
          <span class="back-arrow">‹</span>
          {{ t('Back') }}
        </button>

        <div class="shell">
          <!-- Left card -->
          <div class="left">
            <div class="badge">
              <!-- eslint-disable-next-line vue/no-v-html -->
              <svg viewBox="0 0 12 12" fill="#d99a2b" v-html="BADGE_ART[variant.badgeArt]"></svg>
              {{ t(variant.badgeText) }}
            </div>

            <h1 class="title">{{ t(variant.title) }}</h1>

            <!-- eslint-disable-next-line vue/no-v-html -->
            <p class="desc" v-html="t(variant.desc)"></p>

            <div class="feats">
              <div v-for="feat in variant.feats" :key="feat.title" class="feat">
                <div class="feat-ico">
                  <!-- eslint-disable-next-line vue/no-v-html -->
                  <svg viewBox="0 0 24 24" v-html="FEAT_ART[feat.art]"></svg>
                </div>
                <div class="feat-txt">
                  <div class="feat-t">{{ t(feat.title) }}</div>
                  <div class="feat-s">{{ t(feat.sub) }}</div>
                </div>
              </div>
            </div>
          </div>

          <!-- Right card -->
          <div class="side">
            <div class="illus">
              <!-- eslint-disable-next-line vue/no-v-html -->
              <svg viewBox="0 0 150 84" width="278" height="155" v-html="illustration"></svg>
            </div>

            <div class="body">
              <template v-if="variant.hero">
                <div class="hero-row">
                  <span class="hero-num">{{ variant.hero.num }}</span>
                  <span class="hero-label">{{ t(variant.hero.label) }}</span>
                  <span class="hero-tag">{{ t(variant.hero.tag) }}</span>
                </div>
                <p class="hero-sub">{{ t(variant.hero.sub) }}</p>
              </template>

              <div v-if="variant.status" class="status-head">
                <div class="status-ico" :class="`si-${variant.status.tone}`">
                  <!-- eslint-disable-next-line vue/no-v-html -->
                  <svg
                    viewBox="0 0 24 24"
                    width="21"
                    height="21"
                    fill="none"
                    :stroke="toneStroke(variant.status.tone)"
                    stroke-width="2"
                    v-html="STATUS_ART[variant.status.art]"
                  ></svg>
                </div>
                <div class="status-txt">
                  <div class="status-t">{{ t(variant.status.title) }}</div>
                  <div class="status-s">{{ t(variant.status.sub) }}</div>
                </div>
              </div>

              <div v-if="variant.tips" class="tip-block">
                <template v-for="(tip, index) in variant.tips" :key="tip.title">
                  <div v-if="index > 0" class="tip-divider"></div>
                  <div class="tip-row">
                    <span class="tip-ico">
                      <!-- eslint-disable-next-line vue/no-v-html -->
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        :stroke="toneStroke(tip.tone)"
                        stroke-width="2"
                        v-html="TIP_ART[tip.art]"
                      ></svg>
                    </span>
                    <div class="tip-txt">
                      <div class="tip-t">{{ t(tip.title) }}</div>
                      <div class="tip-s">{{ t(tip.sub) }}</div>
                    </div>
                  </div>
                </template>
              </div>

              <button type="button" class="btn btn-pri" @click="openLink(variant.primary.link)">
                {{ t(variant.primary.text) }}
              </button>

              <button
                v-if="variant.secondary"
                type="button"
                class="btn btn-out"
                @click="openLink(variant.secondary.link)"
              >
                {{ t(variant.secondary.text) }}
              </button>

              <div v-if="variant.accordion" class="acc" :class="{ open: accordionOpen }">
                <div class="acc-head" @click="accordionOpen = !accordionOpen">
                  <span class="acc-title">
                    <!-- eslint-disable-next-line vue/no-v-html -->
                    <svg viewBox="0 0 24 24" v-html="HELP_ART"></svg>
                    {{ t(variant.accordion.title) }}
                  </span>
                  <span class="acc-chev">▾</span>
                </div>
                <div class="acc-body">
                  <div class="acc-inner">
                    <div v-for="item in variant.accordion.items" :key="item" class="acc-item">
                      <span class="acc-dot"></span>{{ t(item) }}
                    </div>
                  </div>
                </div>
              </div>

              <div class="link" @click="openLink('demo')">
                <div class="link-ico">
                  <!-- eslint-disable-next-line vue/no-v-html -->
                  <svg viewBox="0 0 24 24" v-html="DEMO_ART"></svg>
                </div>
                <div class="link-txt">
                  <div class="link-t">{{ t(variant.demo.title) }}</div>
                  <div class="link-s">{{ t(variant.demo.sub) }}</div>
                </div>
                <span class="link-arrow">›</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </Teleport>
</template>

<script setup lang="ts">
import { computed, onBeforeUnmount, ref, watch } from 'vue';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import {
  BADGE_ART,
  DEMO_ART,
  FEAT_ART,
  HELP_ART,
  ILLUSTRATION_ART,
  STATUS_ART,
  TIP_ART,
  withLabel,
} from './packageErrorArt';
import {
  PACKAGE_ERROR_VARIANTS,
  resolvePackageLink,
  type PackageLinkKey,
} from './packageErrorPresets';
import { usePackageErrorPage } from './usePackageErrorPage';

const { t, language } = useUIKit();
const { visible, variantId, closePackageErrorPage } = usePackageErrorPage();

const accordionOpen = ref(false);

const variant = computed(() => (
  variantId.value ? PACKAGE_ERROR_VARIANTS[variantId.value] : null
));

const illustration = computed(() => {
  if (!variant.value) {
    return '';
  }
  const art = ILLUSTRATION_ART[variant.value.illustration];
  const label = variant.value.illustrationLabel;
  return label ? withLabel(art, t(label)) : art;
});

function toneStroke(tone: 'amber' | 'blue'): string {
  return tone === 'amber' ? '#dda63f' : '#5b87f5';
}

function openLink(key: PackageLinkKey): void {
  window.open(resolvePackageLink(key, language.value), '_blank', 'noopener,noreferrer');
}

function close(): void {
  closePackageErrorPage();
}

// The page owns the whole viewport, so the document behind it must not keep
// its own scrollbar.
let previousOverflow = '';

watch(visible, (isVisible) => {
  if (isVisible) {
    accordionOpen.value = false;
    previousOverflow = document.body.style.overflow;
    document.body.style.overflow = 'hidden';
  } else {
    document.body.style.overflow = previousOverflow;
  }
});

onBeforeUnmount(() => {
  if (visible.value) {
    document.body.style.overflow = previousOverflow;
  }
});
</script>

<style scoped>
/* Faithful port of the design prototype (combined-pages.html). Colors and
   metrics are intentionally literal rather than themed, because this page is a
   standalone guidance surface shown on top of the app. */

.pkg-page {
  position: fixed;
  inset: 0;
  z-index: 2000;
  overflow: auto;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'PingFang SC', 'Hiragino Sans GB', 'Microsoft YaHei', sans-serif;
  color: #16181d;
  text-align: left;
  background-color: #f7f9fc;
  background-image:
    linear-gradient(rgba(190, 203, 225, 0.14) 1px, transparent 1px),
    linear-gradient(90deg, rgba(190, 203, 225, 0.14) 1px, transparent 1px);
  background-size: 28px 28px;
  -webkit-font-smoothing: antialiased;
}

.pkg-page *,
.pkg-page *::before,
.pkg-page *::after {
  box-sizing: border-box;
}

.stage {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 100vh;
  padding: 48px 40px 72px;
}

.back {
  position: absolute;
  top: 26px;
  left: 30px;
  display: inline-flex;
  align-items: center;
  height: 34px;
  padding: 0 14px 0 11px;
  font-family: inherit;
  font-size: 13px;
  color: #5b6272;
  cursor: pointer;
  background: rgba(255, 255, 255, 0.9);
  border: 1px solid #eceff5;
  border-radius: 999px;
  gap: 5px;
  transition: all 0.18s;
}

.back:hover {
  color: #16181d;
  border-color: #cdd5e3;
}

.back-arrow {
  font-size: 17px;
  line-height: 1;
}

/* ===== Two equal-height cards ===== */
.shell {
  display: flex;
  align-items: stretch;
  width: 100%;
  max-width: 1240px;
  gap: 26px;
}

/* ===== Left card ===== */
.left {
  display: flex;
  flex: 1;
  flex-direction: column;
  justify-content: center;
  min-width: 0;
  padding: 52px 52px 52px 54px;
  background: #fff;
  border: 1px solid #eaeef6;
  border-radius: 20px;
  box-shadow: 0 12px 40px rgba(28, 40, 70, 0.07), 0 2px 6px rgba(28, 40, 70, 0.03);
}

.badge {
  display: inline-flex;
  align-self: flex-start;
  align-items: center;
  padding: 7px 14px;
  margin-bottom: 22px;
  font-size: 12.5px;
  color: #8a6420;
  letter-spacing: 0.2px;
  background: #fdf5e6;
  border: 1px solid #f2dfb4;
  border-radius: 999px;
  gap: 6px;
}

.badge svg {
  width: 12px;
  height: 12px;
}

.title {
  margin: 0 0 17px;
  font-size: 34px;
  font-weight: 800;
  line-height: 1.3;
  color: #16181d;
  letter-spacing: -0.6px;
}

.desc {
  max-width: 540px;
  margin: 0 0 34px;
  font-size: 14.5px;
  line-height: 2;
  color: #9aa1b1;
}

.desc :deep(strong) {
  font-weight: 600;
  color: #3d4351;
}

.feats {
  display: grid;
  max-width: 560px;
  grid-template-columns: repeat(2, 1fr);
  gap: 14px;
}

.feat {
  display: flex;
  align-items: flex-start;
  padding: 17px 18px;
  background: #fbfcfe;
  border: 1px solid #eef1f7;
  border-radius: 13px;
  gap: 13px;
  transition: all 0.2s;
}

.feat:hover {
  background: #fafcff;
  border-color: #d5e2ff;
  box-shadow: 0 3px 14px rgba(37, 99, 235, 0.07);
}

.feat-ico {
  display: flex;
  flex-shrink: 0;
  align-items: center;
  justify-content: center;
  width: 34px;
  height: 34px;
  background: #eff4ff;
  border-radius: 10px;
}

.feat-ico svg {
  width: 16px;
  height: 16px;
  fill: none;
  stroke: #5b87f5;
  stroke-width: 1.8;
}

.feat-txt {
  min-width: 0;
  padding-top: 2px;
}

.feat-t {
  margin-bottom: 5px;
  font-size: 14px;
  font-weight: 700;
  color: #2b3038;
}

.feat-s {
  font-size: 12.5px;
  line-height: 1.5;
  color: #b8bec9;
}

/* ===== Right card ===== */
.side {
  display: flex;
  flex-direction: column;
  flex-shrink: 0;
  width: 440px;
  overflow: hidden;
  background: #fff;
  border: 1px solid #eaeef6;
  border-radius: 20px;
  box-shadow: 0 12px 40px rgba(28, 40, 70, 0.07), 0 2px 6px rgba(28, 40, 70, 0.03);
}

.illus {
  display: flex;
  flex-shrink: 0;
  align-items: center;
  justify-content: center;
  height: 200px;
  background-color: #fcfdff;
  background-image:
    linear-gradient(rgba(190, 203, 225, 0.15) 1px, transparent 1px),
    linear-gradient(90deg, rgba(190, 203, 225, 0.15) 1px, transparent 1px);
  background-size: 22px 22px;
  border-bottom: 1px solid #eff2f8;
}

.body {
  display: flex;
  flex: 1;
  flex-direction: column;
  justify-content: center;
  padding: 30px 30px 32px;
}

.body > *:last-child {
  margin-bottom: 0;
}

.hero-row {
  display: flex;
  align-items: center;
  margin-bottom: 9px;
  gap: 10px;
}

.hero-num {
  font-size: 44px;
  font-weight: 800;
  line-height: 1;
  color: #16181d;
  letter-spacing: -1.4px;
}

.hero-label {
  font-size: 15.5px;
  font-weight: 500;
  color: #5b6272;
}

.hero-tag {
  padding: 4.5px 12px;
  margin-left: auto;
  font-size: 12px;
  font-weight: 500;
  color: #4a7ff0;
  background: #eff4ff;
  border: 1px solid #d5e2ff;
  border-radius: 999px;
}

.hero-sub {
  margin: 0 0 22px;
  font-size: 13px;
  line-height: 1.7;
  color: #b8bec9;
}

/* Buttons */
.btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 100%;
  font-family: inherit;
  cursor: pointer;
  border: 1px solid transparent;
  border-radius: 11px;
  gap: 8px;
  transition: all 0.18s;
}

.btn-pri {
  height: 52px;
  margin-bottom: 11px;
  font-size: 15.5px;
  font-weight: 700;
  color: #fff;
  background: #2563eb;
}

.btn-pri:hover {
  background: #1d4ed8;
}

.btn-out {
  height: 47px;
  margin-bottom: 18px;
  font-size: 14.5px;
  font-weight: 500;
  color: #4a5160;
  background: #fff;
  border-color: #e4e8f0;
}

.btn-out:hover {
  color: #16181d;
  background: #fcfdff;
  border-color: #cdd5e3;
}

/* Link card */
.link {
  display: flex;
  align-items: center;
  padding: 15px 16px;
  cursor: pointer;
  background: #fbfcfe;
  border: 1px solid #eef1f7;
  border-radius: 12px;
  gap: 13px;
  transition: all 0.18s;
}

.link:hover {
  background: #f6f9fd;
  border-color: #e0e7f3;
}

.link-ico {
  display: flex;
  flex-shrink: 0;
  align-items: center;
  justify-content: center;
  width: 38px;
  height: 38px;
  background: #eff4ff;
  border-radius: 10px;
}

.link-ico svg {
  width: 18px;
  height: 18px;
  fill: none;
  stroke: #5b87f5;
  stroke-width: 1.8;
}

.link-txt {
  flex: 1;
  min-width: 0;
}

.link-t {
  font-size: 14px;
  font-weight: 700;
  color: #333944;
}

.link-s {
  margin-top: 3px;
  font-size: 12px;
  color: #b8bec9;
}

.link-arrow {
  flex-shrink: 0;
  font-size: 17px;
  color: #ccd2dd;
}

/* Status block */
.status-head {
  display: flex;
  align-items: flex-start;
  margin-bottom: 20px;
  gap: 13px;
}

.status-ico {
  display: flex;
  flex-shrink: 0;
  align-items: center;
  justify-content: center;
  width: 42px;
  height: 42px;
  border-radius: 12px;
}

.si-amber {
  background: #fdf3e3;
}

.si-blue {
  background: #eff4ff;
}

.status-txt {
  flex: 1;
}

.status-t {
  margin-bottom: 5px;
  font-size: 15.5px;
  font-weight: 700;
  color: #2b3038;
}

.status-s {
  font-size: 13px;
  line-height: 1.65;
  color: #b8bec9;
}

/* Accordion */
.acc {
  margin-bottom: 14px;
  overflow: hidden;
  background: #fbfcfe;
  border: 1px solid #eef1f7;
  border-radius: 12px;
}

.acc-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 14px 16px;
  cursor: pointer;
  user-select: none;
}

.acc-head:hover {
  background: #f6f9fd;
}

.acc-title {
  display: flex;
  align-items: center;
  font-size: 13.5px;
  font-weight: 600;
  color: #4a5160;
  gap: 9px;
}

.acc-title svg {
  width: 15px;
  height: 15px;
  fill: none;
  stroke: #9aa1b1;
  stroke-width: 1.8;
}

.acc-chev {
  font-size: 12px;
  color: #c3c9d4;
  transition: transform 0.22s;
}

.acc.open .acc-chev {
  transform: rotate(180deg);
}

.acc-body {
  max-height: 0;
  overflow: hidden;
  transition: max-height 0.28s ease;
}

.acc.open .acc-body {
  max-height: 300px;
}

.acc-inner {
  padding: 0 16px 15px;
}

.acc-item {
  display: flex;
  align-items: flex-start;
  padding: 8px 0;
  font-size: 13px;
  line-height: 1.7;
  color: #9aa1b1;
  border-top: 1px dashed #eef1f6;
  gap: 10px;
}

.acc-item:first-child {
  border-top: none;
}

.acc-dot {
  flex-shrink: 0;
  width: 5px;
  height: 5px;
  margin-top: 9px;
  background: #cdd4e0;
  border-radius: 50%;
}

/* Tip block */
.tip-block {
  padding: 4px 16px;
  margin-bottom: 20px;
  background: #fbfcfe;
  border: 1px solid #eef1f7;
  border-radius: 12px;
}

.tip-row {
  display: flex;
  align-items: flex-start;
  padding: 15px 0;
  gap: 12px;
}

.tip-ico {
  display: flex;
  flex-shrink: 0;
  align-items: center;
  justify-content: center;
  width: 22px;
  height: 22px;
  margin-top: 1px;
}

.tip-ico svg {
  width: 18px;
  height: 18px;
}

.tip-txt {
  flex: 1;
  min-width: 0;
}

.tip-t {
  margin-bottom: 5px;
  font-size: 13.5px;
  font-weight: 700;
  color: #2b3038;
}

.tip-s {
  font-size: 12.5px;
  line-height: 1.7;
  color: #9aa1b1;
}

.tip-divider {
  height: 1px;
  background: #eef1f6;
}

@media (max-width: 1180px) {
  .shell {
    flex-direction: column;
    max-width: 560px;
    gap: 20px;
  }

  .side {
    width: 100%;
  }

  .left {
    padding: 40px 34px 36px;
  }

  .feats,
  .desc {
    max-width: none;
  }
}

@media (max-width: 560px) {
  .stage {
    padding: 74px 18px 40px;
  }

  .feats {
    grid-template-columns: 1fr;
  }

  .title {
    font-size: 27px;
  }
}
</style>
