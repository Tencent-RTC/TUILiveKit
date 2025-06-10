//
//  NSObject+Login.m
//  LiveStreamCore
//
//  Created by krabyu on 2024/10/28.
//

#import "NSObject+Login.h"

@implementation NSObject (Login)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wundeclared-selector"
+ (void) load {
    if ([self respondsToSelector:@selector(liveKitExtensionLoad)]) {
        [self performSelector:@selector(liveKitExtensionLoad)];
    }
}
#pragma clang diagnostic pop
@end
