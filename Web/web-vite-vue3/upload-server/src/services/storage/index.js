const { Config } = require('../../config');
const CosProvider = require('./CosProvider');
const CustomProvider = require('./CustomProvider');
const OssProvider = require('./OssProvider');

const PROVIDER_MAP = {
  cos: CosProvider,
  custom: CustomProvider,
  oss: OssProvider,
};

let activeProvider = null;

function getStorageProvider() {
  if (activeProvider) {
    return activeProvider;
  }

  const providerName = String(Config.StorageProvider || 'cos').toLowerCase();
  const ProviderClass = PROVIDER_MAP[providerName];

  if (!ProviderClass) {
    // eslint-disable-next-line no-console
    console.warn(`[storage] unknown provider "${providerName}", fallback to "cos"`);
    activeProvider = new CosProvider();
  } else {
    activeProvider = new ProviderClass();
  }

  // eslint-disable-next-line no-console
  console.info(
    `[storage] provider "${activeProvider.name}" ${activeProvider.isEnabled() ? 'enabled' : 'disabled'}`
  );

  return activeProvider;
}

function isUploadEnabled() {
  const provider = getStorageProvider();
  return provider ? provider.isEnabled() : false;
}

function getUploadConfig() {
  const provider = getStorageProvider();
  if (!provider) {
    return { enabled: false, provider: 'none' };
  }
  return provider.getConfig();
}

function generateKey(type, originalName) {
  const provider = getStorageProvider();
  if (!provider) {
    throw new Error('Storage provider is not configured');
  }
  return provider.generateKey(type, originalName);
}

async function uploadFile(fileBuffer, key, contentType) {
  const provider = getStorageProvider();
  if (!provider || !provider.isEnabled()) {
    throw new Error('Storage provider is disabled');
  }
  return provider.uploadFile(fileBuffer, key, contentType);
}

async function deleteFile(key) {
  const provider = getStorageProvider();
  if (!provider || !provider.isEnabled()) {
    throw new Error('Storage provider is disabled');
  }
  return provider.deleteFile(key);
}

module.exports = {
  getStorageProvider,
  isUploadEnabled,
  getUploadConfig,
  generateKey,
  uploadFile,
  deleteFile,
  PROVIDER_MAP,
};
