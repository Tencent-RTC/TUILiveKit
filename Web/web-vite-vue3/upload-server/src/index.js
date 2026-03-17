const express = require('express');
const { Config } = require('./config');
const { uploadRouter, handleMulterError } = require('./routes/uploadRouter');
const logger = require('./utils/logger');

function createServer() {
  const app = express();

  app.use((req, res, next) => {
    res.header('Access-Control-Allow-Origin', '*');
    res.header('Access-Control-Allow-Methods', 'GET,POST,PUT,DELETE,OPTIONS');
    res.header('Access-Control-Allow-Headers', 'Content-Type, Authorization');
    if (req.method === 'OPTIONS') {
      res.status(204).send('');
      return;
    }
    next();
  });

  app.use(express.json({ limit: '2mb' }));
  app.use('/api', uploadRouter);

  app.get('/api/test', (_req, res) => {
    res.json({
      code: 0,
      message: 'success',
      data: {
        status: 'ok',
      },
    });
  });

  app.use(handleMulterError);

  app.use((error, req, res, _next) => {
    logger.apiError('upload_server_global_error', req, error);
    res.status(500).json({
      code: -1,
      message: error?.message || 'Internal server error',
    });
  });

  return app;
}

function start() {
  const app = createServer();
  const protocol = 'http://';
  app.listen(Config.Port, Config.Host, () => {
    logger.info('UPLOAD_SERVER', `running on ${protocol}${Config.Host}:${Config.Port}`);
  });
}

start();
