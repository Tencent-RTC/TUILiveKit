const path = require('path');
const crypto = require('crypto');

class StorageProvider {
  constructor(name) {
    this.name = name;
  }

  isEnabled() {
    throw new Error('StorageProvider.isEnabled() must be implemented');
  }

  getConfig() {
    return {
      enabled: this.isEnabled(),
      provider: this.name,
    };
  }

  generateKey(type, originalName, pathPrefix = '') {
    const ext = path.extname(originalName) || '.png';
    const timestamp = Date.now();
    const random = typeof crypto.randomUUID === 'function'
      ? crypto.randomUUID().replace(/-/g, '')
      : crypto.randomBytes(16).toString('hex');
    const dirMap = {
      cover: 'covers',
      'gift-icon': 'gifts/icons',
      'gift-animation': 'gifts/animations',
    };
    const subDir = dirMap[type] || 'uploads';
    const normalizedPrefix = pathPrefix ? `${pathPrefix.replace(/\/+$/, '')}/` : '';
    return `${normalizedPrefix}${subDir}/${timestamp}_${random}${ext}`;
  }

  async uploadFile(_fileBuffer, _key, _contentType) {
    throw new Error('StorageProvider.uploadFile() must be implemented');
  }

  async deleteFile(_key) {
    // Optional for concrete providers. Should resolve without payload.
  }
}

module.exports = StorageProvider;
