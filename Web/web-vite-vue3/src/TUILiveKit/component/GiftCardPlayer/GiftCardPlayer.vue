<template>
  <div class="gift-card-player">
    <TransitionGroup name="gift-card">
      <div
        v-for="item in displayList"
        :key="item.id"
        class="gift-card-player__item"
      >
        <GiftCard
          :sender="item.gift.sender"
          :gift-info="item.gift.giftInfo"
          :gift-count="item.gift.giftCount"
        />
      </div>
    </TransitionGroup>
  </div>
</template>

<script setup lang="ts">
import { useGiftCardPlayer } from './useGiftCardPlayer';
import GiftCard from './GiftCard.vue';

// Bullet-head gift cards: up to 6 stacked, each auto-hides 2.5s after its
// last combo tap; same sender+gift within 2s folds into a growing ×N.
const { displayList } = useGiftCardPlayer({
  displayDuration: 2500,
  maxDisplayCount: 6,
  comboWindow: 2000,
});
</script>

<style lang="scss" scoped>
.gift-card-player {
  position: absolute;
  top: 32%;
  left: 12px;
  z-index: 10;
  display: flex;
  flex-direction: column;
  gap: 10px;
  pointer-events: none;

  &__item {
    transition: opacity 0.3s ease, transform 0.3s ease;
  }
}

// Bullet-head entrance: cards fly in from the left edge.
.gift-card-enter-active {
  transition: opacity 0.4s ease-out, transform 0.4s ease-out;
}

.gift-card-leave-active {
  position: absolute;
  width: 100%;
  transition: opacity 0.4s ease-in, transform 0.4s ease-in;
}

.gift-card-enter-from {
  opacity: 0;
  transform: translateX(-120%);
}

.gift-card-enter-to {
  opacity: 1;
  transform: translateX(0);
}

.gift-card-leave-from {
  opacity: 1;
  transform: translateY(0);
}

.gift-card-leave-to {
  opacity: 0;
  transform: translateY(-30px);
}

.gift-card-move {
  transition: transform 0.3s ease;
}
</style>
