/**
 * Error parse result type
 */
export type ErrorParseResult = {
  code: number;
  message: string;
};

/**
 * Raw error shape from SDK/catch block
 */
export interface RawError {
  code?: number | null;
  message?: string;
}

/**
 * Internal error codes for Live Demo
 * Range: 40001 - 40999
 */
export enum LiveErrorCode {
  // Unknown error
  UNKNOWN = 40001,

  // 100002 sub-errors (room configuration)
  PACKAGE_NOT_SUPPORT_LIVE = 40002,
  ROOM_NAME_TOO_LONG = 40003,
  ROOM_ID_TOO_LONG = 40004,
  ROOM_ID_INVALID_FORMAT = 40005,

  // Signature errors
  SIGNATURE_EXPIRED = 40006,
  SIGNATURE_INVALID = 40008,
  USER_ID_MISMATCH = 40009,

  // Payment errors
  NO_ACTIVE_PACKAGE = 40010,
  NETWORK_TIMEOUT = 40012,
  ROOM_ALREADY_EXISTS = 40015,
  ROOM_ID_NOT_EXIST = 40019,

  // 101011 sub-errors (payment limits)
  ROOM_MEMBER_COUNT_LIMIT = 40030,
  ROOM_COUNT_LIMIT = 40031,
}

/**
 * Original error codes from server/SDK
 */
export enum OriginalErrorCode {
  // Signature related
  SIGNATURE_EXPIRED = 70001,
  SIGNATURE_INVALID = 70003,
  USER_ID_MISMATCH = 70013,

  // Network
  NETWORK_TIMEOUT = 2801,

  // Room related
  PARAM_ILLEGAL = 100002,
  ROOM_ID_NOT_EXIST = 100004,
  NO_PAYMENT_INFO = 100007,
  ROOM_ID_OCCUPIED = 100010,

  // Payment limits
  PAYMENT_LIMIT = 101011,
}

/**
 * Unknown error constant
 */
export const ERROR_UNKNOWN: ErrorParseResult = {
  code: LiveErrorCode.UNKNOWN,
  message: 'Unknown error.',
};

/**
 * PARAM_ILLEGAL (100002) error message mapping
 * Sub-errors for room parameter validation
 */
export const PARAM_ILLEGAL_MESSAGE_MAP: Record<string, ErrorParseResult> = {
  'not support seat, please update your pay package on the console': {
    code: LiveErrorCode.PACKAGE_NOT_SUPPORT_LIVE,
    message: 'Your current package does not support live streaming.',
  },
  'the length of room name must be less than 100': {
    code: LiveErrorCode.ROOM_NAME_TOO_LONG,
    message: 'The room name cannot exceed 100 bytes. Each Chinese character occupies 2 bytes.',
  },
  'the length of room id must be less than 48': {
    code: LiveErrorCode.ROOM_ID_TOO_LONG,
    message: 'The room ID cannot exceed 48 characters.',
  },
  'group id is not printable!': {
    code: LiveErrorCode.ROOM_ID_INVALID_FORMAT,
    message: 'The room ID can only contain letters, numbers, and underscores.',
  },
};

/**
 * PAYMENT_LIMIT (101011) error message mapping
 * Sub-errors for package quota limits
 */
export const PAYMENT_LIMIT_MESSAGE_MAP: Record<string, ErrorParseResult> = {
  'The number of members in this room has reached the max limit of the payment': {
    code: LiveErrorCode.ROOM_MEMBER_COUNT_LIMIT,
    message: 'The number of members in this room has reached the limit',
  },
  'the max member count exceeds the limit of payment, please adjust the parameter': {
    code: LiveErrorCode.ROOM_MEMBER_COUNT_LIMIT,
    message: 'The number of members in this room has reached the limit',
  },
  'the max member count exceeds the max limit of payment, please adjust the parameter': {
    code: LiveErrorCode.ROOM_MEMBER_COUNT_LIMIT,
    message: 'The number of members in this room has reached the limit',
  },
  'the number of current room exceeds the maximum limit': {
    code: LiveErrorCode.ROOM_COUNT_LIMIT,
    message: 'The number of rooms has reached the limit of the payment',
  },
};

/**
 * General error code mapping
 */
export const ERROR_CODE_MAP: Record<number, ErrorParseResult> = {
  [OriginalErrorCode.SIGNATURE_EXPIRED]: {
    code: LiveErrorCode.SIGNATURE_EXPIRED,
    message: 'The user signature has expired. It is recommended to set the signature validity period to at least 24 hours.',
  },
  [OriginalErrorCode.SIGNATURE_INVALID]: {
    code: LiveErrorCode.SIGNATURE_INVALID,
    message: 'Invalid signature. Please regenerate it.',
  },
  [OriginalErrorCode.USER_ID_MISMATCH]: {
    code: LiveErrorCode.USER_ID_MISMATCH,
    message: 'The user ID does not match the user signature. Please check.',
  },
  [OriginalErrorCode.NO_PAYMENT_INFO]: {
    code: LiveErrorCode.NO_ACTIVE_PACKAGE,
    message: 'No active package found or account is in arrears. Please proceed to the Tencent Cloud console to make a purchase.',
  },
  [OriginalErrorCode.NETWORK_TIMEOUT]: {
    code: LiveErrorCode.NETWORK_TIMEOUT,
    message: 'Network timeout',
  },
  [OriginalErrorCode.ROOM_ID_OCCUPIED]: {
    code: LiveErrorCode.ROOM_ALREADY_EXISTS,
    message: 'This room already exists, and you are the owner.',
  },
  [OriginalErrorCode.ROOM_ID_NOT_EXIST]: {
    code: LiveErrorCode.ROOM_ID_NOT_EXIST,
    message: 'Room is not existed.',
  },
};
