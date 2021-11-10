module.exports = {
  root: true,
  env: {
    node: true,
  },
  extends: [
    'eslint-config-tencent',
    'plugin:vue/essential',
    'eslint:recommended',
  ],
  parserOptions: {
    parser: 'babel-eslint',
  },
  rules: {},
};
