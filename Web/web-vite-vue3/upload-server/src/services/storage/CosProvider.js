const COS = require('cos-nodejs-sdk-v5');
const StorageProvider = require('./StorageProvider');
const { Config } = require('../../config');

class CosProvider extends StorageProvider {
  constructor() {
    super('cos');
    this._instance = null;
  }

  _getCosInstance() {
    if (this._instance) {
      return this._instance;
    }
    const { SecretId, SecretKey } = Config.Cos;
    if (!SecretId || !SecretKey) {
      return null;
    }
    this._instance = new COS({ SecretId, SecretKey });
    return this._instance;
  }

  isEnabled() {
    const { SecretId, SecretKey, Bucket, Region } = Config.Cos;
    return !!(SecretId && SecretKey && Bucket && Region);
  }

  generateKey(type, originalName) {
    return super.generateKey(type, originalName, Config.Cos.PathPrefix);
  }

  _buildAccessUrl(key) {
    const { Bucket, Region, CdnDomain } = Config.Cos;
    if (CdnDomain) {
      const domain = CdnDomain.replace(/\/+$/, '');
      return `${domain}/${key}`;
    }
    return `https://${Bucket}.cos.${Region}.myqcloud.com/${key}`;
  }

  uploadFile(fileBuffer, key, contentType) {
    return new Promise((resolve, reject) => {
      const cos = this._getCosInstance();
      if (!cos) {
        reject(new Error('COS provider is not configured'));
        return;
      }

      const { Bucket, Region } = Config.Cos;
      cos.putObject(
        {
          Bucket,
          Region,
          Key: key,
          Body: fileBuffer,
          ContentType: contentType,
        },
        (error) => {
          if (error) {
            reject(error);
            return;
          }
          resolve({
            url: this._buildAccessUrl(key),
            key,
          });
        }
      );
    });
  }

  deleteFile(key) {
    return new Promise((resolve, reject) => {
      const cos = this._getCosInstance();
      if (!cos) {
        reject(new Error('COS provider is not configured'));
        return;
      }

      const { Bucket, Region } = Config.Cos;
      cos.deleteObject({ Bucket, Region, Key: key }, (error) => {
        if (error) {
          reject(error);
          return;
        }
        resolve();
      });
    });
  }
}

module.exports = CosProvider;
