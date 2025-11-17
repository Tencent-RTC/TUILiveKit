export const LIVE_ERROR_MAP = {
  'not support seat,  please upgrade your package on the console': "not support seat, please upgrade your package on the console",
  'the length of room name must be less than 100': "the length of room name must be less than 100"
}

const ERROR_MESSAGE_HEADER = 'error_message:'

export function parseLiveErrorMessage(error: string) {
  if(error.includes(ERROR_MESSAGE_HEADER)) {
    const index_start = error.indexOf(ERROR_MESSAGE_HEADER) + ERROR_MESSAGE_HEADER.length;
    const index_end = error.indexOf(', request_id:');
    const message = error.substring(index_start, index_end);
    return LIVE_ERROR_MAP[message as keyof typeof LIVE_ERROR_MAP] || '';
  }
  return '';
}
