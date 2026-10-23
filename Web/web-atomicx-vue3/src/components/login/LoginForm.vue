<template>
  <div class="login-form-container">
    <form class="login-form" @submit.prevent="submitForm">
      <template v-if="!credentialsPreset">
        <div class="form-row">
          <div class="label-row">
            <label for="sdkappid">SDKAppID</label>
            <FieldHint :content="t('Demo.LoginForm.SDKAppIDHint')" />
          </div>
          <div class="input-container">
            <input
              id="sdkappid"
              v-model="form.sdkAppId"
              type="text"
              inputmode="numeric"
              class="input-field"
              :class="{ 'input-error': invalid.sdkAppId }"
              :placeholder="t('Demo.LoginForm.EnterSDKAppID')"
              autocomplete="off"
            />
          </div>
        </div>

        <div class="form-row">
          <div class="label-row">
            <label for="secretkey">SDKSecretKey</label>
            <FieldHint :content="t('Demo.LoginForm.SDKSecretKeyHint')" />
          </div>
          <div class="input-container">
            <input
              id="secretkey"
              v-model="form.secretKey"
              :type="isSecretVisible ? 'text' : 'password'"
              class="input-field secret-field"
              :class="{ 'input-error': invalid.secretKey }"
              :placeholder="t('Demo.LoginForm.EnterSDKSecretKey')"
              autocomplete="off"
              spellcheck="false"
            />
            <button
              type="button"
              class="toggle-secret"
              @click="isSecretVisible = !isSecretVisible"
            >
              {{ isSecretVisible ? t('Demo.LoginForm.Hide') : t('Demo.LoginForm.Show') }}
            </button>
          </div>
        </div>
      </template>

      <div class="form-row">
        <div class="label-row">
          <label for="userid">userID</label>
          <FieldHint :content="t('Demo.LoginForm.UserIDHint')" />
        </div>
        <div class="input-container">
          <input
            id="userid"
            ref="userIdInputRef"
            v-model="form.userID"
            type="text"
            class="input-field"
            :class="{ 'input-error': invalid.userID }"
            :placeholder="t('Demo.LoginForm.EnterUserID')"
            autocomplete="off"
          />
        </div>
      </div>

      <p v-if="!credentialsPreset" class="form-hint">
        {{ t('Demo.LoginForm.SecretKeyLocalOnly') }}
      </p>

      <TUIButton type="primary" class="submit-button" :loading="loading">
        {{ t('Demo.Login') }}
      </TUIButton>
    </form>
  </div>
</template>

<script setup lang="ts">
import { nextTick, reactive, ref, watch } from 'vue';
import { TUIButton, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import FieldHint from './FieldHint.vue';
import { USER_ID_PATTERN, useLoginPrompts } from './loginPrompts';
import type { LoginFormPayload } from './types';
import { openDocLink } from '../../utils/docLinks';

const { t, language } = useUIKit();
const { promptSDKAppIDMissing, promptUserIdInvalid } = useLoginPrompts();

const props = withDefaults(defineProps<{
  sdkAppId?: number;
  secretKey?: string;
  userId?: string;
  credentialsPreset?: boolean;
  loading?: boolean;
}>(), {
  sdkAppId: 0,
  secretKey: '',
  userId: '',
  credentialsPreset: false,
  loading: false,
});

const emit = defineEmits<{
  submit: [payload: LoginFormPayload];
}>();

const form = reactive({
  sdkAppId: props.sdkAppId ? String(props.sdkAppId) : '',
  secretKey: props.secretKey,
  userID: props.userId,
});

const invalid = reactive({ sdkAppId: false, secretKey: false, userID: false });

const isSecretVisible = ref(false);
const userIdInputRef = ref<HTMLInputElement>();

watch(() => form.sdkAppId, () => {
  invalid.sdkAppId = false;
});

watch(() => form.secretKey, () => {
  invalid.secretKey = false;
});

watch(() => form.userID, () => {
  invalid.userID = false;
});

function focusUserIdInput() {
  nextTick(() => {
    userIdInputRef.value?.focus();
    userIdInputRef.value?.select();
  });
}

function submitForm() {
  const sdkAppId = Number(String(form.sdkAppId).trim());
  const secretKey = form.secretKey.trim();
  const userID = form.userID.trim();

  invalid.sdkAppId = false;
  invalid.secretKey = false;
  invalid.userID = false;

  if (!props.credentialsPreset) {
    invalid.sdkAppId = !sdkAppId || !Number.isInteger(sdkAppId) || sdkAppId <= 0;
    invalid.secretKey = !secretKey;
    if (invalid.sdkAppId || invalid.secretKey) {
      promptSDKAppIDMissing({ onConfirm: () => openDocLink('console', language.value) });
      return;
    }
  }

  if (!USER_ID_PATTERN.test(userID)) {
    invalid.userID = true;
    promptUserIdInvalid({ onConfirm: focusUserIdInput });
    return;
  }

  emit('submit', { sdkAppId, secretKey, userID });
}
</script>

<style scoped>
.login-form-container {
  display: flex;
  flex-direction: column;
  width: 100%;
}

.login-form {
  display: flex;
  flex-direction: column;
  gap: 18px;
}

.form-row {
  display: flex;
  flex-direction: column;
  align-items: stretch;
}

label {
  width: auto;
  padding-right: 0;
  font-size: 12.5px;
  font-weight: 500;
  color: #4a5462;
  text-align: left;
  letter-spacing: -0.002em;
}

.label-row {
  display: flex;
  align-items: center;
  margin-bottom: 7px;
  gap: 5px;
}

.input-container {
  position: relative;
  flex: 1;
}

.input-field {
  box-sizing: border-box;
  width: 100%;
  height: 40px;
  padding: 0 12px;
  font-size: 14px;
  line-height: 20px;
  color: #1a2029;
  background-color: #fff;
  border: 1px solid rgba(20, 32, 56, 0.16);
  border-radius: 8px;
  transition: border-color 0.2s ease, box-shadow 0.2s ease, background-color 0.2s ease;
}

.input-field:hover:not(:disabled) {
  border-color: rgba(28, 102, 229, 0.45);
}

.input-field:focus {
  background-color: #fff;
  border-color: #1c66e5;
  outline: none;
  box-shadow: 0 0 0 3px rgba(28, 102, 229, 0.14);
}

.input-field::placeholder {
  font-size: 13.5px;
  color: #9aa4b2;
}

.secret-field {
  padding-right: 62px;
}

.input-error {
  border-color: #cf3b3a;
}

.input-error:focus {
  box-shadow: 0 0 0 3px rgba(207, 59, 58, 0.12);
}

.toggle-secret {
  position: absolute;
  top: 0;
  right: 0;
  box-sizing: border-box;
  width: 62px;
  height: 40px;
  padding: 0;
  font-size: 12.5px;
  color: #1c66e5;
  text-align: center;
  cursor: pointer;
  background: transparent;
  border: none;
}

.toggle-secret:hover {
  color: #1559cc;
}

.form-hint {
  margin: -4px 0 0;
  font-size: 12px;
  line-height: 1.6;
  color: #7d8794;
}

.submit-button {
  width: 100%;
  height: 40px;
}
</style>
