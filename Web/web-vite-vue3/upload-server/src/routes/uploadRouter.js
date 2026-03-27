const { Router } = require('express');
const multer = require('multer');
const {
  isUploadEnabled,
  getUploadConfig,
  generateKey,
  uploadFile,
} = require('../services/storage');
const { asyncHandler } = require('../middleware/asyncHandler');
const logger = require('../utils/logger');

const MAX_FILE_SIZE_MB = 2;
const COVER_ALLOWED_MIME_TYPES = [
  'image/jpeg',
  'image/png',
  'image/gif',
  'image/webp',
];
const ALLOWED_MIME_TYPES = [
  ...COVER_ALLOWED_MIME_TYPES,
  'image/svg+xml',
];
const ALLOWED_UPLOAD_TYPES = [
  'cover',
  'gift-icon',
  'gift-animation',
];
const ALLOWED_MIME_TYPES_BY_UPLOAD_TYPE = {
  cover: COVER_ALLOWED_MIME_TYPES,
  'gift-icon': ALLOWED_MIME_TYPES,
  'gift-animation': ALLOWED_MIME_TYPES,
};

function resolveUploadType(type) {
  if (!type || !ALLOWED_UPLOAD_TYPES.includes(type)) {
    return null;
  }
  return type;
}

function getAllowedMimeTypesByType(type) {
  return ALLOWED_MIME_TYPES_BY_UPLOAD_TYPE[type] || COVER_ALLOWED_MIME_TYPES;
}

function getUnsupportedMimeMessage(type) {
  if (type === 'cover') {
    return 'Only JPG/PNG/GIF/WebP images are supported for cover';
  }
  return 'Only JPG/PNG/GIF/WebP/SVG images are supported';
}

const upload = multer({
  storage: multer.memoryStorage(),
  limits: { fileSize: MAX_FILE_SIZE_MB * 1024 * 1024 },
  fileFilter: (req, file, callback) => {
    const uploadType = resolveUploadType(req.body?.type);
    const allowedMimeTypes = uploadType ? getAllowedMimeTypesByType(uploadType) : ALLOWED_MIME_TYPES;
    if (allowedMimeTypes.includes(file.mimetype)) {
      callback(null, true);
      return;
    }
    const error = new Error(getUnsupportedMimeMessage(uploadType));
    logger.warn('UPLOAD_FILE_FILTER', error.message, {
      type: uploadType || req.body?.type,
      mimetype: file.mimetype,
      originalname: file.originalname,
    });
    callback(error);
  },
});

function handleMulterError(error, _req, res, next) {
  if (error instanceof multer.MulterError) {
    logger.error('UPLOAD_MULTER', error, {
      field: error.field,
      code: error.code,
    });
    if (error.code === 'LIMIT_FILE_SIZE') {
      res.status(400).json({
        code: -1,
        message: `File size cannot exceed ${MAX_FILE_SIZE_MB}MB`,
      });
      return;
    }
    res.status(400).json({
      code: -1,
      message: error.message,
    });
    return;
  }

  if (error) {
    logger.error('UPLOAD_ERROR', error);
    res.status(400).json({
      code: -1,
      message: error.message || 'Upload failed',
    });
    return;
  }

  next();
}

const uploadRouter = Router();

uploadRouter.get('/upload/config', (_req, res) => {
  const config = getUploadConfig();
  res.json({
    code: 0,
    message: 'success',
    data: {
      enabled: Boolean(config.enabled),
      provider: config.provider || 'none',
    },
  });
});

uploadRouter.post(
  '/upload/image',
  upload.single('file'),
  asyncHandler(
    async (req, res) => {
      if (!isUploadEnabled()) {
        const message = 'Storage provider is not configured';
        logger.warn('UPLOAD_IMAGE', message);
        res.status(400).json({ code: -1, message });
        return;
      }

      if (!req.file) {
        const message = 'Please select a file';
        logger.warn('UPLOAD_IMAGE', message, { body: req.body });
        res.status(400).json({ code: -1, message });
        return;
      }

      const type = resolveUploadType(req.body?.type);
      if (!type) {
        const message = 'Missing or invalid upload type';
        logger.warn('UPLOAD_TYPE_INVALID', message, {
          type: req.body?.type,
          originalname: req.file.originalname,
        });
        res.status(400).json({ code: -1, message });
        return;
      }
      const allowedMimeTypes = getAllowedMimeTypesByType(type);
      if (!allowedMimeTypes.includes(req.file.mimetype)) {
        const message = getUnsupportedMimeMessage(type);
        logger.warn('UPLOAD_MIME_TYPE_NOT_ALLOWED', message, {
          type,
          mimetype: req.file.mimetype,
          originalname: req.file.originalname,
        });
        res.status(400).json({ code: -1, message });
        return;
      }
      const key = generateKey(type, req.file.originalname);
      const result = await uploadFile(req.file.buffer, key, req.file.mimetype);

      res.json({
        code: 0,
        message: 'success',
        data: {
          url: result.url,
          key: result.key,
          size: req.file.size,
          mimetype: req.file.mimetype,
          provider: getUploadConfig().provider,
        },
      });
    },
    'upload_image',
    'local'
  )
);

module.exports = {
  uploadRouter,
  handleMulterError,
  MAX_FILE_SIZE_MB,
  ALLOWED_MIME_TYPES,
};
