<template>
  <span class="field-hint">
    <button
      type="button"
      class="field-hint-trigger"
      :aria-label="content"
      tabindex="0"
    >
      <svg width="14" height="14" viewBox="0 0 24 24" fill="none" aria-hidden="true">
        <circle cx="12" cy="12" r="9.2" stroke="currentColor" stroke-width="1.7" />
        <path
          d="M9.6 9.2a2.45 2.45 0 1 1 3.4 2.26c-.63.28-1 .78-1 1.42v.52"
          stroke="currentColor"
          stroke-width="1.7"
          stroke-linecap="round"
        />
        <circle cx="12" cy="16.5" r="0.95" fill="currentColor" />
      </svg>
    </button>
    <span class="field-hint-bubble" role="tooltip">{{ content }}</span>
  </span>
</template>

<script setup lang="ts">
defineProps<{ content: string }>();
</script>

<style scoped>
.field-hint {
  position: relative;
  display: inline-flex;
  align-items: center;
}

.field-hint-trigger {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 16px;
  height: 16px;
  padding: 0;
  color: #9aa4b2;
  cursor: help;
  background: transparent;
  border: none;
  border-radius: 50%;
  transition: color 0.2s ease;
}

.field-hint-trigger:hover,
.field-hint-trigger:focus-visible {
  color: #1c66e5;
}

.field-hint-trigger:focus-visible {
  outline: 2px solid rgba(28, 102, 229, 0.4);
  outline-offset: 1px;
}

.field-hint-trigger svg {
  display: block;
}

.field-hint-bubble {
  position: absolute;
  bottom: calc(100% + 10px);
  left: -10px;
  z-index: 20;
  box-sizing: border-box;
  width: max-content;
  max-width: 264px;
  padding: 9px 12px;
  font-size: 12px;
  font-weight: 400;
  line-height: 1.66;
  color: #4a5462;
  text-align: left;
  letter-spacing: -0.002em;
  visibility: hidden;
  opacity: 0;
  background: #fff;
  border: 1px solid rgba(20, 32, 56, 0.1);
  border-radius: 10px;
  box-shadow:
    0 1px 2px rgba(20, 32, 56, 0.04),
    0 10px 26px -10px rgba(20, 32, 56, 0.18);
  transform: translateY(3px);
  transition: opacity 0.18s ease, transform 0.18s ease, visibility 0.18s ease;
}

/* Two stacked triangles so the arrow keeps the 1px border outline. */
.field-hint-bubble::before,
.field-hint-bubble::after {
  position: absolute;
  width: 0;
  height: 0;
  content: "";
  border-right: 6px solid transparent;
  border-left: 6px solid transparent;
}

.field-hint-bubble::before {
  top: 100%;
  left: 13px;
  border-top: 7px solid rgba(20, 32, 56, 0.12);
}

.field-hint-bubble::after {
  top: calc(100% - 1px);
  left: 13px;
  border-top: 6px solid #fff;
}

.field-hint-trigger:hover + .field-hint-bubble,
.field-hint-trigger:focus-visible + .field-hint-bubble {
  visibility: visible;
  opacity: 1;
  transform: none;
}

@media (max-width: 480px) {
  .field-hint-bubble {
    max-width: 208px;
  }
}
</style>
