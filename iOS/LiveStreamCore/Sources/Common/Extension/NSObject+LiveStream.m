//
//  NSObject+LiveStream.m
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/30.
//

#import "NSObject+LiveStream.h"

@implementation NSObject (LiveStream)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wundeclared-selector"
+ (void) load {
    if ([self respondsToSelector:@selector(liveStreamExtensionLoad)]) {
        [self performSelector:@selector(liveStreamExtensionLoad)];
    }
}
#pragma clang diagnostic pop
@end
