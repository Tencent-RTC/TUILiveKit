//
// Copyright (c) 2024 Tencent.
//
//  TUIBeautyExtension.m
//  TEBeautyKit
//
//  Created by jackyixue on 2024/7/15.
//

#import "TEBeautyExtension.h"

#import "TUICore.h"

#import <TEBeautyKit/TEBeautyKit-Swift.h>

@interface TUIBeautyExtension()

@end

@implementation TUIBeautyExtension

+ (void)load {
    [TUICore registerExtension:@"TUICore_TEBeautyExtension_GetBeautyPanel" object:[TEBeautyService shared]];
    [TUICore registerService:@"TUICore_TEBeautyService" object:[TEBeautyService shared]];
}



@end
