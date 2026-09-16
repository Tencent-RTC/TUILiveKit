import { ref } from 'vue';
import {
  getPackageErrorVariantId,
  type PackageErrorVariantId,
} from './packageErrorPresets';

export interface PackageErrorPageOptions {
  /**
   * Runs when the page is dismissed. Call sites use it to unwind whatever the
   * failed action left behind, e.g. leaving the room they could not join.
   */
  onClose?: () => void;
}

const visible = ref(false);
const variantId = ref<PackageErrorVariantId | null>(null);
let closeHandler: (() => void) | null = null;

export function usePackageErrorPage() {
  function openPackageErrorPage(
    id: PackageErrorVariantId,
    options?: PackageErrorPageOptions,
  ): void {
    variantId.value = id;
    closeHandler = options?.onClose || null;
    visible.value = true;
  }

  /**
   * Show the full page for a package-limit error.
   *
   * @returns `true` when the code is a package-limit error and the page took
   * over the reporting, `false` when the caller should fall back to its own
   * error reporting.
   */
  function openPackageErrorByCode(
    code: number,
    options?: PackageErrorPageOptions,
  ): boolean {
    const id = getPackageErrorVariantId(code);
    if (!id) {
      return false;
    }
    openPackageErrorPage(id, options);
    return true;
  }

  function closePackageErrorPage(): void {
    visible.value = false;
    const handler = closeHandler;
    closeHandler = null;
    handler?.();
  }

  return {
    visible,
    variantId,
    openPackageErrorPage,
    openPackageErrorByCode,
    closePackageErrorPage,
  };
}
