const { Blob } = require('buffer');
const StorageProvider = require('./StorageProvider');
const { Config } = require('../../config');

class CustomProvider extends StorageProvider {
  constructor() {
    super('custom');
  }

  isEnabled() {
    return !!(Config.Custom && Config.Custom.UploadUrl);
  }

  generateKey(type, originalName) {
    return super.generateKey(type, originalName, Config.Custom.PathPrefix);
  }

  _getNestedValue(source, dotPath) {
    return String(dotPath || 'data.url')
      .split('.')
      .reduce((current, key) => (current != null ? current[key] : undefined), source);
  }

  _parseAuthHeader() {
    const header = Config.Custom.AuthHeader;
    if (!header) {
      return null;
    }
    const separatorIndex = header.indexOf(':');
    if (separatorIndex <= 0) {
      return null;
    }
    const headerName = header.slice(0, separatorIndex).trim();
    const headerValue = header.slice(separatorIndex + 1).trim();
    if (!headerName || !headerValue) {
      return null;
    }
    return [headerName, headerValue];
  }

  async uploadFile(fileBuffer, key, contentType) {
    const { UploadUrl, UploadField, ResponseUrlField, AccessDomain } = Config.Custom;
    if (!UploadUrl) {
      throw new Error('Custom provider is not configured: missing CUSTOM_UPLOAD_URL');
    }

    if (typeof fetch !== 'function' || typeof FormData === 'undefined') {
      throw new Error('Node runtime does not support fetch/FormData');
    }

    const formData = new FormData();
    const fileName = key.split('/').pop() || 'upload_file';
    const blob = new Blob([fileBuffer], { type: contentType });
    formData.append(UploadField || 'file', blob, fileName);
    formData.append('key', key);

    const headers = {};
    const parsedAuthHeader = this._parseAuthHeader();
    if (parsedAuthHeader) {
      const [headerName, headerValue] = parsedAuthHeader;
      headers[headerName] = headerValue;
    }

    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 30000);
    let response;
    try {
      response = await fetch(UploadUrl, {
        method: 'POST',
        body: formData,
        headers,
        signal: controller.signal,
      });
    } finally {
      clearTimeout(timeout);
    }

    if (!response.ok) {
      throw new Error(`Custom upload failed: HTTP ${response.status}`);
    }

    const result = await response.json();
    const url = this._getNestedValue(result, ResponseUrlField || 'data.url');

    if (url) {
      return { url, key };
    }

    if (AccessDomain) {
      return {
        url: `${AccessDomain.replace(/\/+$/, '')}/${key}`,
        key,
      };
    }

    throw new Error('Custom upload response does not contain URL');
  }

  async deleteFile(_key) {
    // This provider cannot infer delete endpoint contract.
  }
}

module.exports = CustomProvider;
