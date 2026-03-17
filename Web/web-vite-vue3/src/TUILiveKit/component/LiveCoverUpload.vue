<template>
  <div class="live-cover-upload">
    <input
      ref="coverFileInputRef"
      class="cover-file-input"
      type="file"
      :accept="acceptedCoverFileTypes"
      @change="handleCoverFileChange"
    >
    <div v-if="uploadEnabled" class="cover-card-list">
      <div
        class="cover-card cover-card-landscape"
        :class="{ active: coverType === 'landscape', filled: hasLandscapeCover }"
        @click="handleCardClick('landscape')"
        @dragover.prevent="handleDragOver"
        @drop.prevent="handleDrop($event, 'landscape')"
      >
        <div class="cover-card-tag">{{ t('Landscape cover') }}</div>
        <img
          v-if="hasLandscapeCover"
          class="cover-preview-image"
          :src="coverUrlModel"
          alt="landscape cover preview"
        >
        <div v-else class="cover-placeholder">
          <div class="cover-placeholder-title">{{ t('Click to upload cover image') }}</div>
        </div>
        <button
          v-if="hasLandscapeCover"
          class="cover-remove-btn"
          type="button"
          :title="t('Remove cover')"
          @click.stop="handleRemoveCover"
        >
          <IconClose :size="12" />
        </button>
      </div>

      <div
        class="cover-card cover-card-portrait"
        :class="{ active: coverType === 'portrait', filled: hasPortraitCover }"
        @click="handleCardClick('portrait')"
        @dragover.prevent="handleDragOver"
        @drop.prevent="handleDrop($event, 'portrait')"
      >
        <div class="cover-card-tag">{{ t('Portrait cover') }}</div>
        <img
          v-if="hasPortraitCover"
          class="cover-preview-image"
          :src="coverUrlModel"
          alt="portrait cover preview"
        >
        <div v-else class="cover-placeholder">
          <div class="cover-placeholder-title">{{ t('Click to upload cover image') }}</div>
        </div>
        <button
          v-if="hasPortraitCover"
          class="cover-remove-btn"
          type="button"
          :title="t('Remove cover')"
          @click.stop="handleRemoveCover"
        >
          <IconClose :size="12" />
        </button>
      </div>
    </div>

    <div class="cover-tip">{{ coverUploadHint }}</div>

    <template v-if="!uploadEnabled">
      <TUIInput
        v-model="coverUrlModel"
        :placeholder="t('Cover URL')"
        :spellcheck="false"
      />
      <div class="cover-tip cover-tip-warning">
        {{ t('Upload is unavailable. Please enter cover URL manually') }}
      </div>
    </template>
  </div>
</template>

<script setup lang="ts">
import { computed, ref, watch } from 'vue';
import { IconClose, TUIInput, TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import {
  uploadImageFile,
  UPLOAD_ALLOWED_MIME_TYPES,
  UPLOAD_MAX_FILE_SIZE_MB,
} from '../../api/upload';

const LANDSCAPE_COVER_RATIO = 16 / 9;
const PORTRAIT_COVER_RATIO = 9 / 16;
const COVER_RATIO_TOLERANCE = 0.03;

type CoverType = 'landscape' | 'portrait';

const props = withDefaults(defineProps<{
  modelValue?: string;
  uploadEnabled?: boolean;
  maxSizeMb?: number;
  allowedMimeTypes?: string[];
  coverType?: CoverType;
}>(), {
  modelValue: '',
  uploadEnabled: false,
  maxSizeMb: UPLOAD_MAX_FILE_SIZE_MB,
  coverType: 'landscape',
  allowedMimeTypes: () => [...UPLOAD_ALLOWED_MIME_TYPES],
});

const emit = defineEmits<{
  (event: 'update:modelValue', value: string): void;
  (event: 'update:coverType', value: CoverType): void;
  (event: 'upload-success', payload: { url: string }): void;
}>();

const { t } = useUIKit();
const coverFileInputRef = ref<HTMLInputElement>();
const isUploading = ref(false);
const pendingCoverType = ref<CoverType>('landscape');
const detectCoverTypeTaskId = ref(0);

const coverUrlModel = computed({
  get: () => props.modelValue,
  set: (value: string) => emit('update:modelValue', value),
});
const acceptedCoverFileTypes = computed(() => props.allowedMimeTypes.join(','));
const coverType = computed(() => props.coverType);
const uploadEnabled = computed(() => props.uploadEnabled);
const coverUploadHint = computed(() =>
  t('Please select landscape (16:9) or portrait (9:16) cover, file size cannot exceed {size}MB')
    .replace('{size}', String(props.maxSizeMb))
);
const hasLandscapeCover = computed(() => props.coverType === 'landscape' && !!props.modelValue);
const hasPortraitCover = computed(() => props.coverType === 'portrait' && !!props.modelValue);

function parseUploadErrorMessage(error: unknown) {
  const err = error as { response?: { data?: { message?: string } }; message?: string };
  return err?.response?.data?.message || err?.message || t('Upload failed, please try again');
}

function getExpectedRatio(type: CoverType) {
  return type === 'portrait' ? PORTRAIT_COVER_RATIO : LANDSCAPE_COVER_RATIO;
}

function getRatioText(type: CoverType) {
  return type === 'portrait' ? '9:16' : '16:9';
}

function resolveCoverTypeByRatio(width: number, height: number): CoverType {
  const ratio = width / height;
  const landscapeDiff = Math.abs(ratio - LANDSCAPE_COVER_RATIO);
  const portraitDiff = Math.abs(ratio - PORTRAIT_COVER_RATIO);
  return landscapeDiff <= portraitDiff ? 'landscape' : 'portrait';
}

function isAspectRatioValid(width: number, height: number, type: CoverType) {
  if (!width || !height) {
    return false;
  }
  return Math.abs(width / height - getExpectedRatio(type)) <= COVER_RATIO_TOLERANCE;
}

async function detectCoverTypeFromUrl(url: string): Promise<CoverType | null> {
  return new Promise((resolve) => {
    const image = new Image();
    image.onload = () => {
      const width = image.naturalWidth || image.width;
      const height = image.naturalHeight || image.height;
      if (!width || !height) {
        resolve(null);
        return;
      }
      resolve(resolveCoverTypeByRatio(width, height));
    };
    image.onerror = () => resolve(null);
    image.src = url;
  });
}

async function getImageSize(file: File): Promise<{ width: number; height: number }> {
  return new Promise((resolve, reject) => {
    const imageUrl = URL.createObjectURL(file);
    const image = new Image();
    image.onload = () => {
      resolve({
        width: image.naturalWidth || image.width,
        height: image.naturalHeight || image.height,
      });
      URL.revokeObjectURL(imageUrl);
    };
    image.onerror = () => {
      reject(new Error('Failed to load image'));
      URL.revokeObjectURL(imageUrl);
    };
    image.src = imageUrl;
  });
}

async function validateCoverFile(file: File, type: CoverType): Promise<string> {
  if (!props.allowedMimeTypes.includes(file.type)) {
    return t('Unsupported image format');
  }
  if (file.size > props.maxSizeMb * 1024 * 1024) {
    return t('File size cannot exceed {size}MB').replace('{size}', String(props.maxSizeMb));
  }
  const { width, height } = await getImageSize(file);
  if (!isAspectRatioValid(width, height, type)) {
    return t('Please upload a {ratio} image').replace('{ratio}', getRatioText(type));
  }
  return '';
}

function switchCoverType(type: CoverType) {
  if (props.coverType === type) {
    return;
  }
  emit('update:coverType', type);
  emit('update:modelValue', '');
}

function showUploadUnavailableTip() {
  TUIToast.info({
    message: t('Upload is unavailable. Please enter cover URL manually'),
  });
}

function triggerFileSelect(type: CoverType) {
  pendingCoverType.value = type;
  coverFileInputRef.value?.click();
}

function handleCardClick(type: CoverType) {
  if (isUploading.value) {
    return;
  }
  switchCoverType(type);
  if (!props.uploadEnabled) {
    showUploadUnavailableTip();
    return;
  }
  triggerFileSelect(type);
}

function handleDragOver(event: DragEvent) {
  if (!props.uploadEnabled && event.dataTransfer) {
    event.dataTransfer.dropEffect = 'none';
  }
}

async function handleDrop(event: DragEvent, type: CoverType) {
  if (isUploading.value) {
    return;
  }
  switchCoverType(type);
  if (!props.uploadEnabled) {
    showUploadUnavailableTip();
    return;
  }
  const selectedFile = event.dataTransfer?.files?.[0];
  if (!selectedFile) {
    return;
  }
  await processUploadFile(selectedFile, type);
}

async function processUploadFile(selectedFile: File, type: CoverType) {
  try {
    const validateErrorMessage = await validateCoverFile(selectedFile, type);
    if (validateErrorMessage) {
      TUIToast.error({
        message: validateErrorMessage,
      });
      return;
    }

    if (!props.uploadEnabled) {
      showUploadUnavailableTip();
      return;
    }

    isUploading.value = true;
    const uploadResult = await uploadImageFile({
      file: selectedFile,
      type: 'cover',
    });
    emit('update:modelValue', uploadResult.url);
    emit('upload-success', { url: uploadResult.url });
  } catch (error: unknown) {
    TUIToast.error({
      message: parseUploadErrorMessage(error),
    });
  } finally {
    isUploading.value = false;
  }
}

async function handleCoverFileChange(event: Event) {
  const input = event.target as HTMLInputElement;
  const selectedFile = input.files?.[0];
  input.value = '';
  if (!selectedFile || isUploading.value) {
    return;
  }
  await processUploadFile(selectedFile, pendingCoverType.value);
}

function handleRemoveCover() {
  emit('update:modelValue', '');
}

async function syncCoverTypeByCoverUrl(coverUrl: string) {
  const taskId = ++detectCoverTypeTaskId.value;
  const detectedType = await detectCoverTypeFromUrl(coverUrl);
  if (!detectedType || taskId !== detectCoverTypeTaskId.value) {
    return;
  }
  if (detectedType !== props.coverType) {
    emit('update:coverType', detectedType);
  }
}

watch(
  [() => props.modelValue, () => props.coverType],
  ([nextCoverUrl, nextCoverType], [prevCoverUrl, prevCoverType]) => {
    if (!nextCoverUrl) {
      detectCoverTypeTaskId.value += 1;
      return;
    }
    const isInitialRun = prevCoverUrl === undefined || prevCoverType === undefined;
    const coverUrlChangedFromEmpty = !prevCoverUrl && !!nextCoverUrl;
    const coverTypeChanged = nextCoverType !== prevCoverType;
    if (!isInitialRun && !coverUrlChangedFromEmpty && !coverTypeChanged) {
      return;
    }
    void syncCoverTypeByCoverUrl(nextCoverUrl);
  },
  { immediate: true },
);
</script>

<style scoped lang="scss">
@import '../style/index.scss';

.live-cover-upload {
  width: 100%;
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.cover-file-input {
  display: none;
}

.cover-card-list {
  display: flex;
  align-items: flex-start;
  gap: 10px;
}

.cover-card {
  position: relative;
  overflow: hidden;
  border-radius: 10px;
  border: 1px dashed var(--stroke-color-primary);
  background: var(--bg-color-dialog, rgba(255, 255, 255, 0.02));
  cursor: pointer;
  transition: border-color .2s ease, box-shadow .2s ease;
}

.cover-card.active {
  border-style: solid;
  border-color: var(--button-color-primary, #3778ff);
  box-shadow: 0 0 0 1px rgba(55, 120, 255, 0.25);
}

.cover-card.filled {
  border-style: solid;
}

.cover-card-landscape {
  width: 216px;
  height: 122px;
}

.cover-card-portrait {
  width: 100px;
  height: 178px;
}

.cover-card-tag {
  position: absolute;
  top: 8px;
  left: 8px;
  z-index: 2;
  padding: 2px 8px;
  border-radius: 10px;
  background: rgba(0, 0, 0, 0.35);
  color: #fff;
  font-size: 11px;
  line-height: 16px;
}

.cover-preview-image {
  width: 100%;
  height: 100%;
  object-fit: cover;
}

.cover-placeholder {
  width: 100%;
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 8px;
  text-align: center;
}

.cover-placeholder-title {
  @include text-size-12;
  width: 100%;
  line-height: 16px;
  white-space: normal;
  word-break: break-word;
  overflow-wrap: anywhere;
  color: $text-color2;
}

.cover-card-portrait .cover-placeholder-title {
  // Keep the portrait placeholder copy at two lines, matching the Electron layout more closely.
  width: 56px;
}

.cover-remove-btn {
  position: absolute;
  top: 8px;
  right: 8px;
  z-index: 2;
  width: 22px;
  height: 22px;
  border: none;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  background: rgba(0, 0, 0, 0.45);
  color: #fff;
  cursor: pointer;
}

.cover-tip {
  @include text-size-12;
  color: $text-color2;
}

.cover-tip-warning {
  color: var(--text-color-warning, #ff9c3e);
}
</style>
