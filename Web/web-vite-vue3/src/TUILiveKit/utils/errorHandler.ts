import {
  ErrorParseResult,
  RawError,
  OriginalErrorCode,
  ERROR_UNKNOWN,
  PARAM_ILLEGAL_MESSAGE_MAP,
  PAYMENT_LIMIT_MESSAGE_MAP,
  ERROR_CODE_MAP,
} from '../types/error';

export type { ErrorParseResult } from '../types/error';

/**
 * Normalize unknown error to RawError shape
 * @param error - Unknown error from catch block
 * @returns Normalized RawError object
 */
function normalizeRawError(error: unknown): RawError {
  if (error && typeof error === 'object' && 'code' in error && 'message' in error) {
    return { code: (error as RawError).code, message: (error as RawError).message };
  }
  if (error instanceof Error) {
    return { message: error.message };
  }
  return {};
}

class ErrorHandler {
  parseError(error: unknown): ErrorParseResult {
    const raw = normalizeRawError(error);

    // Some IM errors embed the code inside the message string
    // (e.g. "error_code:100026, error_message:group info secure check fail!").
    // Extract it so parseByKnownCode can match against ERROR_CODE_MAP.
    // Use matchAll to collect every occurrence — a message may contain
    // multiple error_code segments, and we want the first one that
    // resolves to a known mapping rather than blindly taking the first.
    if (raw.code === null || raw.code === undefined) {
      const matches = typeof raw.message === 'string'
        ? [...raw.message.matchAll(/error_code:(\d+)/g)]
        : [];
      for (const match of matches) {
        const code = Number(match[1]);
        if (code in ERROR_CODE_MAP) {
          raw.code = code;
          break;
        }
      }
      // Fall back to the first match if none mapped to a known code,
      // so parseByKnownCode still gets a chance to report it.
      if ((raw.code === null || raw.code === undefined) && matches.length > 0) {
        raw.code = Number(matches[0][1]);
      }
    }

    if (raw.code !== null && raw.code !== undefined) {
      switch (raw.code) {
        case OriginalErrorCode.PARAM_ILLEGAL:
          return this.parseByMessageMap(raw, PARAM_ILLEGAL_MESSAGE_MAP);
        case OriginalErrorCode.PAYMENT_LIMIT:
          return this.parseByMessageMap(raw, PAYMENT_LIMIT_MESSAGE_MAP);
        default:
          return this.parseByKnownCode(raw);
      }
    } else if (raw.message) {
      return {
        code: ERROR_UNKNOWN.code,
        message: raw.message,
      };
    } else {
      return ERROR_UNKNOWN;
    }
  }

  /**
   * Parse error by matching message content against a message map
   * @param raw - Normalized raw error
   * @param messageMap - Map of message patterns to ErrorParseResult
   * @returns Matched ErrorParseResult or unknown error with original message
   */
  private parseByMessageMap(
    raw: RawError,
    messageMap: Record<string, ErrorParseResult>
  ): ErrorParseResult {
    const message = raw.message || '';
    for (const key in messageMap) {
      if (message.includes(key)) {
        return messageMap[key];
      }
    }
    return {
      code: ERROR_UNKNOWN.code,
      message: raw.message || ERROR_UNKNOWN.message,
    };
  }

  /**
   * Parse error by known error code mapping
   * @param raw - Normalized raw error
   * @returns Mapped ErrorParseResult or unknown error with original message
   */
  private parseByKnownCode(raw: RawError): ErrorParseResult {
    if (raw.code !== undefined && raw.code !== null) {
      const mappedError = ERROR_CODE_MAP[raw.code];
      if (mappedError) {
        return mappedError;
      }
    }
    return {
      code: ERROR_UNKNOWN.code,
      message: raw.message || ERROR_UNKNOWN.message,
    };
  }
}

export const errorHandler = new ErrorHandler();
