# TUI LiveKit Upload Server

This is the shared upload server used by the TUI LiveKit Electron and Web demos.

## Start

```bash
npm install
npm run start
```

## Configure

Copy `.env.example` to `.env`, then configure one storage provider:

- `cos`
- `oss`
- `custom`

The service exposes:

- `GET /api/upload/config`
- `POST /api/upload/image`
