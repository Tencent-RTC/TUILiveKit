//
//  NSObject+VoiceRoom.m
//  SeatGridView
//
//  Created by krabyu on 2024/10/28.
//

#import "NSObject+VoiceRoom.h"

@implementation NSObject (VoiceRoom)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wundeclared-selector"
+ (void) load {
    if ([self respondsToSelector:@selector(voiceRoomExtensionLoad)]) {
        [self performSelector:@selector(voiceRoomExtensionLoad)];
    }
}
#pragma clang diagnostic pop
@end

