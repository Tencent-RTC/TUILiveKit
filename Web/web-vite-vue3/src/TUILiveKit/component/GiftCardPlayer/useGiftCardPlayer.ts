/**
 * Gift card player hook for the demo bullet-head gift animation.
 * @description Subscribes to gift messages and folds consecutive gifts from the
 * same sender + same gift into a single "combo" card whose ×N grows live,
 * so a rapid tap combo reads as one continuously incrementing bubble.
 */
import { ref, onMounted, onUnmounted } from 'vue';
import { useLiveGiftState, LiveGiftEvents } from 'tuikit-atomicx-vue3';
import type { Gift } from 'tuikit-atomicx-vue3';

export interface GiftCardItem {
  id: string;
  gift: Gift;
  lastUpdateTs: number;
  timer?: ReturnType<typeof setTimeout>;
}

interface UseGiftCardPlayerOptions {
  displayDuration?: number; // How long a (combo) card stays before auto-hide (ms)
  maxDisplayCount?: number; // Max cards shown at once (oldest evicted)
  comboWindow?: number; // Same sender+gift within this window folds into combo (ms)
}

function useGiftCardPlayer(options: UseGiftCardPlayerOptions = {}) {
  const {
    displayDuration = 3000,
    maxDisplayCount = 4,
    comboWindow = 2000,
  } = options;
  const { subscribeEvent, unsubscribeEvent } = useLiveGiftState();

  const displayList = ref<GiftCardItem[]>([]);

  const removeGiftCard = (id: string) => {
    const index = displayList.value.findIndex(item => item.id === id);
    if (index === -1) return;
    const item = displayList.value[index];
    if (item.timer) clearTimeout(item.timer);
    displayList.value.splice(index, 1);
  };

  const scheduleHide = (item: GiftCardItem) => {
    if (item.timer) clearTimeout(item.timer);
    if (Number.isFinite(displayDuration)) {
      item.timer = setTimeout(() => removeGiftCard(item.id), displayDuration);
    }
  };

  // Locate an existing card that is still within the combo window for the
  // same sender and the same gift, so successive taps fold into one bubble.
  const findComboCard = (gift: Gift): GiftCardItem | undefined => {
    const now = Date.now();
    return displayList.value.find(item =>
      item.gift.sender.userId === gift.sender.userId &&
      item.gift.giftInfo.giftID === gift.giftInfo.giftID &&
      now - item.lastUpdateTs <= comboWindow,
    );
  };

  const addGiftToDisplay = (gift: Gift) => {
    if (maxDisplayCount <= 0) return;

    const combo = findComboCard(gift);
    if (combo) {
      // Fold into the existing combo: grow the count and refresh the hide timer.
      // A fresh object reference also re-triggers the card's ×N pop animation.
      combo.gift = { ...combo.gift, giftCount: combo.gift.giftCount + (gift.giftCount || 1) };
      combo.lastUpdateTs = Date.now();
      scheduleHide(combo);
      return;
    }

    // Evict the oldest card when at capacity.
    if (displayList.value.length >= maxDisplayCount) {
      removeGiftCard(displayList.value[0].id);
    }

    const item: GiftCardItem = {
      id: `${gift.sender.userId}-${gift.giftInfo.giftID}-${Date.now()}`,
      gift,
      lastUpdateTs: Date.now(),
    };
    scheduleHide(item);
    displayList.value.push(item);
  };

  const onReceiveGift = (gift: Gift) => addGiftToDisplay(gift);

  onMounted(() => {
    subscribeEvent(LiveGiftEvents.ON_RECEIVE_GIFT_MESSAGE, onReceiveGift);
  });

  onUnmounted(() => {
    unsubscribeEvent(LiveGiftEvents.ON_RECEIVE_GIFT_MESSAGE, onReceiveGift);
    displayList.value.forEach(item => item.timer && clearTimeout(item.timer));
    displayList.value = [];
  });

  return { displayList };
}

export { useGiftCardPlayer };
