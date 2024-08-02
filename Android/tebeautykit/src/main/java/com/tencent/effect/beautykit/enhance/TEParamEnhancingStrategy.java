package com.tencent.effect.beautykit.enhance;

import com.tencent.effect.beautykit.model.TEUIProperty;

/**
 * Interface Definition for Beauty Enhancement Mode
 */
public interface TEParamEnhancingStrategy {


    TEUIProperty.TESDKParam enhanceParam(TEUIProperty.TESDKParam param);
}
