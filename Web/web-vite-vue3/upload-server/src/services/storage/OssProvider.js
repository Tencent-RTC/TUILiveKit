const OSS = require('ali-oss');
const StorageProvider = require('./StorageProvider');
const { Config } = require('../../config');

class OssProvider extends StorageProvider {
  constructor() {
    super('oss');
    this._instance = null;
  }

  isEnabled() {
    const { AccessKeyId, AccessKeySecret, Bucket, Endpoint } = Config.Oss;
    return !!(AccessKeyId && AccessKeySecret && Bucket && Endpoint);
  }

  _normalizeEndpoint() {
    return String(Config.Oss.Endpoint || '')
      .replace(/^https?:\/\//, '')
      .replace(/\/+$/, '');
  }

  _encodeKey(key) {
    return String(key)
      .split('/')
      .map(segment => encodeURIComponent(segment))
      .join('/');
  }

  _getClient() {
    if (this._instance) {
      return this._instance;
    }

    if (!this.isEnabled()) {
      return null;
    }

    const { AccessKeyId, AccessKeySecret, Bucket } = Config.Oss;
    const normalizedEndpoint = this._normalizeEndpoint();

    this._instance = new OSS({
      accessKeyId: AccessKeyId,
      accessKeySecret: AccessKeySecret,
      bucket: Bucket,
      endpoint: `https://${normalizedEndpoint}`,
    });
    return this._instance;
  }

  generateKey(type, originalName) {
    return super.generateKey(type, originalName, Config.Oss.PathPrefix);
  }

  _buildAccessUrl(key) {
    const { Bucket } = Config.Oss;
    const normalizedEndpoint = this._normalizeEndpoint();
    const encodedKey = this._encodeKey(key);
    return `https://${Bucket}.${normalizedEndpoint}/${encodedKey}`;
  }

  async uploadFile(fileBuffer, key, contentType) {
    const client = this._getClient();
    if (!client) {
      throw new Error('OSS provider is not configured');
    }

    try {
      await client.put(key, fileBuffer, {
        mime: contentType,
      });
    } catch (error) {
      throw new Error(`OSS upload failed for key "${key}": ${error?.message || 'unknown error'}`);
    }

    return {
      url: this._buildAccessUrl(key),
      key,
    };
  }

  async deleteFile(key) {
    const client = this._getClient();
    if (!client) {
      throw new Error('OSS provider is not configured');
    }
    try {
      await client.delete(key);
    } catch (error) {
      throw new Error(`OSS delete failed for key "${key}": ${error?.message || 'unknown error'}`);
    }
  }
}

module.exports = OssProvider;
