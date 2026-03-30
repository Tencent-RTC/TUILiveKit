import axios from 'axios';

const DEFAULT_UPLOAD_SERVER_BASE_URL = import.meta.env.VUE_APP_UPLOAD_SERVER_BASE_URL
  || import.meta.env.VITE_UPLOAD_SERVER_BASE_URL
  || 'http://127.0.0.1:3071';

export const UPLOAD_MAX_FILE_SIZE_MB = 2;
export const UPLOAD_ALLOWED_MIME_TYPES = [
  'image/jpeg',
  'image/png',
  'image/gif',
  'image/webp',
];

const uploadHttp = axios.create({
  baseURL: DEFAULT_UPLOAD_SERVER_BASE_URL,
  timeout: 15_000,
});

export interface UploadConfig {
  enabled: boolean;
  provider?: string;
}

interface UploadResponseData {
  url: string;
  key?: string;
  size?: number;
  mimetype?: string;
  provider?: string;
}

export async function fetchUploadConfig(): Promise<UploadConfig> {
  try {
    const response = await uploadHttp.get('/api/upload/config');
    if (response?.data?.code === 0 && response?.data?.data) {
      return response.data.data as UploadConfig;
    }
  } catch (error) {
    console.warn('[upload] fetchUploadConfig failed:', error);
  }
  return {
    enabled: false,
    provider: 'none',
  };
}

export async function uploadImageFile(params: {
  file: File;
  type?: 'cover' | 'gift-icon' | 'gift-animation';
}): Promise<UploadResponseData> {
  const formData = new FormData();
  // Keep `type` before `file` so the server can resolve per-type MIME rules
  // as early as possible during multipart parsing.
  formData.append('type', params.type || 'cover');
  formData.append('file', params.file);

  const response = await uploadHttp.post('/api/upload/image', formData, {
    headers: {
      'Content-Type': 'multipart/form-data',
    },
    timeout: 20_000,
  });

  if (response?.data?.code !== 0 || !response?.data?.data?.url) {
    throw new Error(response?.data?.message || 'Upload failed');
  }

  return response.data.data as UploadResponseData;
}
