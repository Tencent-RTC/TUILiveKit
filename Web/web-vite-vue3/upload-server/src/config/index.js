const path = require('path');
const dotenv = require('dotenv');

dotenv.config({ path: path.resolve(__dirname, '../../.env') });

const DEFAULT_HOST = '0.0.0.0';
const DEFAULT_PORT = 3071;

function parsePort(value, fallback) {
  const parsed = Number(value);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

const Config = {
  Host: process.env.HOST || DEFAULT_HOST,
  Port: parsePort(process.env.PORT, DEFAULT_PORT),
  StorageProvider: process.env.STORAGE_PROVIDER || 'cos',
  Cos: {
    SecretId: process.env.COS_SECRET_ID || '',
    SecretKey: process.env.COS_SECRET_KEY || '',
    Bucket: process.env.COS_BUCKET || '',
    Region: process.env.COS_REGION || '',
    CdnDomain: process.env.COS_CDN_DOMAIN || '',
    PathPrefix: process.env.COS_PATH_PREFIX || '',
  },
  Custom: {
    UploadUrl: process.env.CUSTOM_UPLOAD_URL || '',
    AccessDomain: process.env.CUSTOM_ACCESS_DOMAIN || '',
    UploadField: process.env.CUSTOM_UPLOAD_FIELD || 'file',
    ResponseUrlField: process.env.CUSTOM_RESPONSE_URL_FIELD || 'data.url',
    AuthHeader: process.env.CUSTOM_AUTH_HEADER || '',
    PathPrefix: process.env.CUSTOM_PATH_PREFIX || '',
  },
  Oss: {
    AccessKeyId: process.env.OSS_ACCESS_KEY_ID || '',
    AccessKeySecret: process.env.OSS_ACCESS_KEY_SECRET || '',
    Bucket: process.env.OSS_BUCKET || '',
    Endpoint: process.env.OSS_ENDPOINT || '',
    PathPrefix: process.env.OSS_PATH_PREFIX || '',
  },
};

module.exports = { Config };
