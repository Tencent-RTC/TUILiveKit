declare class LibGenerateTestUserSig {
  constructor(sdkAppId: number, secretKey: string, expireTime: number);
  genTestUserSig(userId: string): string;
}

export default LibGenerateTestUserSig;
