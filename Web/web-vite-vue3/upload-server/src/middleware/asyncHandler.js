const logger = require('../utils/logger');

class ValidationError extends Error {
  constructor(message, format = 'local') {
    super(message);
    this.name = 'ValidationError';
    this.format = format;
  }
}

function asyncHandler(fn, label = 'request', defaultFormat = 'local') {
  return (req, res, next) => {
    Promise.resolve(fn(req, res, next)).catch((error) => {
      logger.apiError(label, req, error);

      const format =
        error instanceof ValidationError
          ? error.format
          : error && error.ErrorCode !== undefined
            ? 'trtc'
            : defaultFormat;

      const message = error?.ErrorMessage || error?.ErrorInfo || error?.message || `${label} failed`;

      if (format === 'trtc') {
        res.json({ ErrorCode: -1, ErrorMessage: message });
        return;
      }
      res.json({ code: -1, message });
    });
  };
}

module.exports = { asyncHandler, ValidationError };
