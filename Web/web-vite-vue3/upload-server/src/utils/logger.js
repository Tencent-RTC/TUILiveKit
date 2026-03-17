function formatTime() {
  return new Date().toISOString().replace('T', ' ').slice(0, 19);
}

const logger = {
  error(context, error, details = {}) {
    const timestamp = formatTime();
    const message = error instanceof Error ? `${error.message}\nStack: ${error.stack}` : String(error);
    // eslint-disable-next-line no-console
    console.error(`[${timestamp}] [ERROR] [${context}] ${message}`);
    if (Object.keys(details).length > 0) {
      // eslint-disable-next-line no-console
      console.error(`[${timestamp}] [ERROR] [${context}] Details:`, JSON.stringify(details, null, 2));
    }
  },
  warn(context, message, details = {}) {
    const timestamp = formatTime();
    // eslint-disable-next-line no-console
    console.warn(`[${timestamp}] [WARN] [${context}] ${message}`);
    if (Object.keys(details).length > 0) {
      // eslint-disable-next-line no-console
      console.warn(`[${timestamp}] [WARN] [${context}] Details:`, JSON.stringify(details, null, 2));
    }
  },
  info(context, message, details = {}) {
    const timestamp = formatTime();
    // eslint-disable-next-line no-console
    console.log(`[${timestamp}] [INFO] [${context}] ${message}`);
    if (Object.keys(details).length > 0) {
      // eslint-disable-next-line no-console
      console.log(`[${timestamp}] [INFO] [${context}] Details:`, JSON.stringify(details, null, 2));
    }
  },
  apiError(apiName, req, error) {
    const timestamp = formatTime();
    // eslint-disable-next-line no-console
    console.error(`[${timestamp}] [API_ERROR] [${apiName}] request failed`);
    // eslint-disable-next-line no-console
    console.error(
      `[${timestamp}] [API_ERROR] [${apiName}] Request Info:`,
      JSON.stringify(
        {
          method: req.method,
          path: req.path,
          query: req.query,
          body: req.body,
          headers: {
            'content-type': req.headers['content-type'],
            'user-agent': req.headers['user-agent'],
          },
        },
        null,
        2
      )
    );
    if (error.response) {
      // eslint-disable-next-line no-console
      console.error(`[${timestamp}] [API_ERROR] [${apiName}] Response Status: ${error.response.status}`);
      // eslint-disable-next-line no-console
      console.error(
        `[${timestamp}] [API_ERROR] [${apiName}] Response Data:`,
        JSON.stringify(error.response.data, null, 2)
      );
      return;
    }
    // eslint-disable-next-line no-console
    console.error(`[${timestamp}] [API_ERROR] [${apiName}] Error:`, error.message || error);
  },
};

module.exports = logger;
