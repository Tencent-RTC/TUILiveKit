//
//  UIImage+Additions.m
//  Created by Joan Martin.
//  Take a look to my repos at http://github.com/vilanovi
//
// Copyright (c) 2013 Joan Martin, vilanovi@gmail.com.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE

#import "UIImage+Additions.h"

@interface NSString (MD5Hashing)

- (NSString*)md5;

@end

#import <CommonCrypto/CommonDigest.h>

@implementation NSString (MD5Hashing)

- (NSString *)md5
{
    const char *cStr = [self UTF8String];
    unsigned char result[16];
    
    CC_MD5(cStr, (CC_LONG)strlen(cStr), result);
    
    return [NSString stringWithFormat:
			@"%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x",
			result[0], result[1], result[2], result[3],
			result[4], result[5], result[6], result[7],
			result[8], result[9], result[10], result[11],
			result[12], result[13], result[14], result[15]
			];
}

@end

const UICornerInset UICornerInsetZero = {0.0f, 0.0f, 0.0f, 0.0f};

NSString* stringFromUICornerInset(UICornerInset cornerInset)
{
    return [NSString stringWithFormat:@"UICornerInset <topLeft:%f> <topRight:%f> <bottomLeft:%f> <bottomRight:%f>",cornerInset.topLeft, cornerInset.topRight, cornerInset.bottomLeft, cornerInset.bottomRight];
}

static NSCache * gImageCache = nil;

static NSString * gUIImageName = @"gUIImageName";
static NSString * gUIImageResizableImage = @"gUIImageResizableImage";
static NSString * gUIImageColors = @"gUIImageColors";
static NSString * gUIImageTintColor = @"gUIImageTintColor";
static NSString * gUIImageTintStyle = @"gUIImageTintStyle";
static NSString * gUIImageCornerInset = @"gUIImageCornerInset";
static NSString * gUIImageGradientDirection = @"gUIImageGradientDirection";
static NSString * gUIImageSize = @"gUIImageSize";

@implementation UIImage (Additions)

+ (UIImage*)imageWithColor:(UIColor*)color size:(CGSize)size
{
    return [self imageWithColor:color size:size cornerInset:UICornerInsetZero];
}

+ (UIImage*)imageWithColor:(UIColor*)color size:(CGSize)size cornerRadius:(CGFloat)cornerRadius
{
    return [self imageWithColor:color size:size cornerInset:uiCornerInsetMake(cornerRadius, cornerRadius, cornerRadius, cornerRadius)];
}

+ (UIImage*)imageWithColor:(UIColor*)color size:(CGSize)size cornerInset:(UICornerInset)cornerInset
{
    return [self imageWithColor:color size:size cornerInset:cornerInset saveInCache:YES];
}

+ (UIImage*)resizableImageWithColor:(UIColor*)color
{
    return [self resizableImageWithColor:color cornerInset:UICornerInsetZero];
}

+ (UIImage*)resizableImageWithColor:(UIColor*)color cornerRadius:(CGFloat)cornerRadius
{
    return [self resizableImageWithColor:color cornerInset:uiCornerInsetMake(cornerRadius, cornerRadius, cornerRadius, cornerRadius)];
}

+ (UIImage*)resizableImageWithColor:(UIColor*)color cornerInset:(UICornerInset)cornerInset
{
    if (!color)
        return nil;
    
    NSDictionary *descriptors =  @{gUIImageColors : @[color],
                                   gUIImageResizableImage : @YES,
                                   gUIImageCornerInset : [NSValue valueWithUICornerInset:cornerInset]};
    
    UIImage *image = [self cachedImageWithDescriptors:descriptors];
    
    if (image)
        return image;
    
    CGSize size = CGSizeMake(MAX(cornerInset.topLeft, cornerInset.bottomLeft) + MAX(cornerInset.topRight, cornerInset.bottomRight) + 1,
                             MAX(cornerInset.topLeft, cornerInset.topRight) + MAX(cornerInset.bottomLeft, cornerInset.bottomRight) + 1);
    
    UIEdgeInsets capInsets = UIEdgeInsetsMake(MAX(cornerInset.topLeft, cornerInset.topRight),
                                              MAX(cornerInset.topLeft, cornerInset.bottomLeft),
                                              MAX(cornerInset.bottomLeft, cornerInset.bottomRight),
                                              MAX(cornerInset.topRight, cornerInset.bottomRight));
    
    image = [[self imageWithColor:color size:size cornerInset:cornerInset] resizableImageWithCapInsets:capInsets];
    
    [self cacheImage:image withDescriptors:descriptors];
    
    return image;
}

+ (UIImage*)blackColorImage
{
    return [self resizableImageWithColor:[UIColor blackColor]];
}

+ (UIImage*)darkGrayColorImage
{
    return [self resizableImageWithColor:[UIColor darkGrayColor]];
}

+ (UIImage*)lightGrayColorImage
{
    return [self resizableImageWithColor:[UIColor lightGrayColor]];
}

+ (UIImage*)whiteColorImage
{
    return [self resizableImageWithColor:[UIColor whiteColor]];
}

+ (UIImage*)grayColorImage
{
    return [self resizableImageWithColor:[UIColor grayColor]];
}

+ (UIImage*)redColorImage
{
    return [self resizableImageWithColor:[UIColor redColor]];
}

+ (UIImage*)greenColorImage
{
    return [self resizableImageWithColor:[UIColor greenColor]];
}

+ (UIImage*)blueColorImage
{
    return [self resizableImageWithColor:[UIColor blueColor]];
}

+ (UIImage*)cyanColorImage
{
    return [self resizableImageWithColor:[UIColor cyanColor]];
}

+ (UIImage*)yellowColorImage
{
    return [self resizableImageWithColor:[UIColor yellowColor]];
}

+ (UIImage*)magentaColorImage
{
    return [self resizableImageWithColor:[UIColor magentaColor]];
}

+ (UIImage*)orangeColorImage
{
    return [self resizableImageWithColor:[UIColor orangeColor]];
}

+ (UIImage*)purpleColorImage
{
    return [self resizableImageWithColor:[UIColor purpleColor]];
}

+ (UIImage*)brownColorImage
{
    return [self resizableImageWithColor:[UIColor brownColor]];
}

+ (UIImage*)clearColorImage
{
    return [self resizableImageWithColor:[UIColor clearColor]];
}

+ (UIImage*)imageNamed:(NSString *)name tintColor:(UIColor*)color style:(UIImageTintedStyle)tintStyle
{
    if (!name)
        return nil;

    UIImage *image = [UIImage imageNamed:name];
    
    if (!image)
        return nil;
    
    if (!color)
        return image;
    
    NSDictionary *descriptors =  @{gUIImageName : name,
                                   gUIImageTintColor : color,
                                   gUIImageTintStyle : @(tintStyle)};
    
    UIImage *tintedImage = [self cachedImageWithDescriptors:descriptors];
    
    if (!tintedImage)
    {
        tintedImage = [image tintedImageWithColor:color style:tintStyle];
        [self cacheImage:tintedImage withDescriptors:descriptors];
    }
    
    return tintedImage;
}

- (UIImage*)tintedImageWithColor:(UIColor*)color style:(UIImageTintedStyle)tintStyle
{
    if (!color)
        return self;
    
    CGFloat scale = self.scale;
    CGSize size = CGSizeMake(scale * self.size.width, scale * self.size.height);
    
    UIGraphicsBeginImageContext(size);
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    
    CGContextTranslateCTM(context, 0, size.height);
    CGContextScaleCTM(context, 1.0, -1.0);
    
    CGRect rect = CGRectMake(0, 0, size.width, size.height);
    // ---

    if (tintStyle == UIImageTintedStyleOverAlpha)
    {
        [color setFill];
        CGContextFillRect(context, rect);
    }
    
    // draw alpha-mask
    CGContextSetBlendMode(context, kCGBlendModeNormal);
    CGContextDrawImage(context, rect, self.CGImage);

    if (tintStyle == UIImageTintedStyleKeepingAlpha)
    {
        CGContextSetBlendMode(context, kCGBlendModeSourceIn);
        [color setFill];
        CGContextFillRect(context, rect);
    }
    
    // ---
    CGImageRef bitmapContext = CGBitmapContextCreateImage(context);
    
    UIImage *coloredImage = [UIImage imageWithCGImage:bitmapContext scale:scale orientation:UIImageOrientationUp];
    
    CGImageRelease(bitmapContext);
    
    UIGraphicsEndImageContext();
    
    return coloredImage;
}

- (UIImage*)imageWithRoundedBounds
{    
    CGSize size = self.size;
    CGFloat radius = MIN(size.width, size.height) / 2.0;
    return [self imageWithCornerRadius:radius];
}

- (UIImage*)imageWithCornerRadius:(CGFloat)cornerRadius
{
    return [self imageWithCornerInset:uiCornerInsetMake(cornerRadius, cornerRadius, cornerRadius, cornerRadius)];
}

- (UIImage *)imageWithCornerInset:(UICornerInset)cornerInset
{
    if (![self isValidCornerInset:cornerInset])
        return nil;
    
    CGFloat scale = self.scale;
    
    CGRect rect = CGRectMake(0.0f, 0.0f, scale*self.size.width, scale*self.size.height);
        
    cornerInset.topRight *= scale;
    cornerInset.topLeft *= scale;
    cornerInset.bottomLeft *= scale;
    cornerInset.bottomRight *= scale;
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef context = CGBitmapContextCreate(NULL, rect.size.width, rect.size.height, 8, 0,
     colorSpace, (CGBitmapInfo)kCGImageAlphaPremultipliedLast);
    CGColorSpaceRelease(colorSpace);
    
    if (context == NULL)
        return nil;
    
    CGFloat minx = CGRectGetMinX(rect), midx = CGRectGetMidX(rect), maxx = CGRectGetMaxX(rect);
    CGFloat miny = CGRectGetMinY(rect), midy = CGRectGetMidY(rect), maxy = CGRectGetMaxY(rect);
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, minx, midy);
    CGContextAddArcToPoint(context, minx, miny, midx, miny, cornerInset.bottomLeft);
    CGContextAddArcToPoint(context, maxx, miny, maxx, midy, cornerInset.bottomRight);
    CGContextAddArcToPoint(context, maxx, maxy, midx, maxy, cornerInset.topRight);
    CGContextAddArcToPoint(context, minx, maxy, minx, midy, cornerInset.topLeft);
    CGContextClosePath(context);
    CGContextClip(context);
    
    CGContextDrawImage(context, rect, self.CGImage);
    
    CGImageRef bitmapImageRef = CGBitmapContextCreateImage(context);
    
    CGContextRelease(context);
    
    UIImage *newImage = [UIImage imageWithCGImage:bitmapImageRef scale:scale orientation:UIImageOrientationUp];
    
    CGImageRelease(bitmapImageRef);

    return newImage;
}

- (BOOL)isValidCornerInset:(UICornerInset)cornerInset
{
    CGSize size = self.size;
    
    BOOL isValid = YES;
    
    if (cornerInset.topLeft + cornerInset.topRight > size.width)
        isValid = NO;
    
    else if (cornerInset.topRight + cornerInset.bottomRight > size.height)
        isValid = NO;
    
    else if (cornerInset.bottomRight + cornerInset.bottomLeft > size.width)
        isValid = NO;
    
    else if (cornerInset.bottomLeft + cornerInset.topLeft > size.height)
        isValid = NO;
    
    return isValid;
}

- (UIImage*)imageAddingImage:(UIImage*)image
{
    CGSize size1 = self.size;
    CGSize size2 = image.size;
    
    CGPoint offset = CGPointMake(floorf((size1.width - size2.width)/2.0),
                                 floorf((size1.height - size2.height)/2.0));
    return [self imageAddingImage:image offset:offset];
}

- (UIImage*)imageAddingImage:(UIImage*)image offset:(CGPoint)offset
{
    CGSize size = self.size;
    CGFloat scale = self.scale;
    
    size.width *= scale;
    size.height *= scale;
    
    UIGraphicsBeginImageContext(size);
    
    [self drawInRect:CGRectMake( 0, 0, size.width, size.height)];
    
    [image drawInRect:CGRectMake(scale * offset.x, scale * offset.y, image.size.width * scale, image.size.height * scale)];
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGImageRef bitmapContext = CGBitmapContextCreateImage(context);
    UIImage *destImage = [UIImage imageWithCGImage:bitmapContext scale:image.scale orientation:UIImageOrientationUp];
    UIGraphicsEndImageContext();
    CGImageRelease(bitmapContext);
    
    return destImage;
}

#pragma mark Private Methods

+ (NSCache*)cache
{
    if (!gImageCache)
        gImageCache = [[NSCache alloc] init];
    
    return gImageCache;
}

+ (UIImage*)cachedImageWithDescriptors:(NSDictionary*)descriptors
{
    return [[self cache] objectForKey:[self keyForImageWithDescriptors:descriptors]];
}

+ (void)cacheImage:(UIImage*)image withDescriptors:(NSDictionary*)descriptors
{
    NSString *key = [self keyForImageWithDescriptors:descriptors];
    [[self cache] setObject:image forKey:key];
}

+ (NSString*)keyForImageWithDescriptors:(NSDictionary*)descriptors
{
    NSMutableString *string = [NSMutableString string];
    
    NSString *imageName = [descriptors valueForKey:gUIImageName];
    [string appendFormat:@"<%@:%@>",gUIImageName,(imageName == nil)?@"":imageName];
    [string appendFormat:@"<%@:%@>",gUIImageSize, NSStringFromCGSize([[descriptors valueForKey:gUIImageSize] CGSizeValue])];
    [string appendFormat:@"<%@:%d>",gUIImageResizableImage,[[descriptors valueForKey:gUIImageResizableImage] boolValue]];
    
    [string appendFormat:@"<%@:",gUIImageColors];
    NSArray *colors = [descriptors valueForKey:gUIImageColors];
    for (UIColor *color in colors)
        [string appendFormat:@"%ld",(long)color.hash];
    [string appendFormat:@">"];
    
    [string appendFormat:@"<%@:%ld>",gUIImageTintColor,(long)[[descriptors valueForKey:gUIImageTintColor] hash]];
    [string appendFormat:@"<%@:%ld>",gUIImageTintStyle,(long)[[descriptors valueForKey:gUIImageTintStyle] integerValue]];
    [string appendFormat:@"<%@:%@>",gUIImageCornerInset,stringFromUICornerInset([[descriptors valueForKey:gUIImageCornerInset] uicornerInsetValue])];
    [string appendFormat:@"<%@:%ld>",gUIImageGradientDirection,(long)[[descriptors valueForKey:gUIImageGradientDirection] integerValue]];
    
    return [string md5];
}

+ (UIImage*)imageWithColor:(UIColor*)color size:(CGSize)size cornerInset:(UICornerInset)cornerInset saveInCache:(BOOL)save
{
    NSDictionary *descriptors =  @{gUIImageColors : @[color],
                                   gUIImageSize : [NSValue valueWithCGSize:size],
                                   gUIImageCornerInset : [NSValue valueWithUICornerInset:cornerInset]};

    UIImage *image = [self cachedImageWithDescriptors:descriptors];
    
    if (image)
        return image;
    
    CGFloat scale = [[UIScreen mainScreen] scale];
    CGRect rect = CGRectMake(0.0f, 0.0f, scale*size.width, scale*size.height);
    
    cornerInset.topRight *= scale;
    cornerInset.topLeft *= scale;
    cornerInset.bottomLeft *= scale;
    cornerInset.bottomRight *= scale;
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef context = CGBitmapContextCreate(NULL, rect.size.width, rect.size.height, 8, 0,
     colorSpace, (CGBitmapInfo)kCGImageAlphaPremultipliedLast);
    
    CGColorSpaceRelease(colorSpace);
    
    if (context == NULL)
        return nil;
    
    CGFloat minx = CGRectGetMinX(rect), midx = CGRectGetMidX(rect), maxx = CGRectGetMaxX(rect);
    CGFloat miny = CGRectGetMinY(rect), midy = CGRectGetMidY(rect), maxy = CGRectGetMaxY(rect);
    
    CGContextBeginPath(context);
    CGContextSetGrayFillColor(context, 1.0, 0.0); // <-- Alpha color in background
    CGContextAddRect(context, rect);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathFill);
    
    CGContextSetFillColorWithColor(context, [color CGColor]); // <-- Color to fill
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, minx, midy);
    CGContextAddArcToPoint(context, minx, miny, midx, miny, cornerInset.bottomLeft);
    CGContextAddArcToPoint(context, maxx, miny, maxx, midy, cornerInset.bottomRight);
    CGContextAddArcToPoint(context, maxx, maxy, midx, maxy, cornerInset.topRight);
    CGContextAddArcToPoint(context, minx, maxy, minx, midy, cornerInset.topLeft);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathFill);
    
    CGImageRef bitmapContext = CGBitmapContextCreateImage(context);
    
    CGContextRelease(context);
    
    UIImage *theImage = [UIImage imageWithCGImage:bitmapContext scale:scale orientation:UIImageOrientationUp];
    
    CGImageRelease(bitmapContext);
    
    if (save)
        [self cacheImage:theImage withDescriptors:descriptors];
    
    return theImage;
}

+ (UIImage*)imageWithGradient:(NSArray*)colors size:(CGSize)size direction:(UIImageGradientDirection)direction
{
    
    NSDictionary *descriptors = @{gUIImageColors: colors,
                                  gUIImageSize: [NSValue valueWithCGSize:size],
                                  gUIImageGradientDirection: @(direction)};
    
    UIImage *image = [self cachedImageWithDescriptors:descriptors];
    if (image)
        return image;
    
    CGRect rect = CGRectMake(0, 0, size.width, size.height);
    
    UIGraphicsBeginImageContextWithOptions(rect.size, NO, 0.0);
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    
    // Create Gradient
    NSMutableArray *cgColors = [NSMutableArray arrayWithCapacity:colors.count];
    for (UIColor *color in colors)
        [cgColors addObject:(id)color.CGColor];
    
    CGColorSpaceRef space = CGColorSpaceCreateDeviceRGB();
    CGGradientRef gradient = CGGradientCreateWithColors(space, (__bridge CFArrayRef)cgColors, NULL);
    
    // Apply gradient
    CGPoint startPoint = CGPointZero;
    CGPoint endPoint = CGPointZero;
    
    if (direction == UIImageGradientDirectionVertical)
        endPoint = CGPointMake(0, rect.size.height);
    
    else if (direction == UIImageGradientDirectionHorizontal)
        endPoint = CGPointMake(rect.size.width, 0);

    CGContextDrawLinearGradient(context, gradient, startPoint, endPoint, 0);
    image = UIGraphicsGetImageFromCurrentImageContext();
    
    // Clean memory & End context
    UIGraphicsEndImageContext();
    CGGradientRelease(gradient);
    CGColorSpaceRelease(space);
    
    [self cacheImage:image withDescriptors:descriptors];
    
    return image;
}

+ (UIImage*)resizableImageWithGradient:(NSArray*)colors size:(CGSize)size direction:(UIImageGradientDirection)direction
{
    if ((size.width == 0.0f && direction == UIImageGradientDirectionHorizontal) ||
        (size.height == 0.0f && direction == UIImageGradientDirectionVertical) ||
        (size.height == 0.0f && size.width == 0.0f))
        return nil;
    
    NSDictionary *descriptors = @{gUIImageColors: colors,
                                  gUIImageSize: [NSValue valueWithCGSize:size],
                                  gUIImageGradientDirection: @(direction),
                                  gUIImageResizableImage: @YES};
    
    UIImage *image = [self cachedImageWithDescriptors:descriptors];
    if (image)
        return image;
    
    CGSize imageSize = CGSizeMake(1.0f, 1.0f);
    
    UIEdgeInsets insets = UIEdgeInsetsZero;
    
    if (direction == UIImageGradientDirectionVertical)
    {
        imageSize.height = size.height;
        insets = UIEdgeInsetsMake(0.0f, 1.0f, 0.0f, 1.0f);
    }
    else if (direction == UIImageGradientDirectionHorizontal)
    {
        imageSize.width = size.width;
        insets = UIEdgeInsetsMake(1.0f, 0.0f, 1.0f, 0.0f);
    }
    
    return [[self imageWithGradient:colors size:imageSize direction:direction] resizableImageWithCapInsets:insets];
}


- (UIImage *)imageWithTintColor:(UIColor *)tintColor
{
    return [self imageWithTintColor:tintColor blendMode:kCGBlendModeDestinationIn];
}

- (UIImage *)imageWithGradientTintColor:(UIColor *)tintColor
{
    return [self imageWithTintColor:tintColor blendMode:kCGBlendModeOverlay];
}

- (UIImage *)imageWithTintColor:(UIColor *)tintColor blendMode:(CGBlendMode)blendMode
{
    if (!tintColor) {
        return self;
    }
    
    //We want to keep alpha, set opaque to NO; Use 0.0f for scale to use the scale factor of the device’s main screen.
    UIGraphicsBeginImageContextWithOptions(self.size, NO, [[UIScreen mainScreen] scale]);
    [tintColor setFill];
    CGRect bounds = CGRectMake(0, 0, self.size.width, self.size.height);
    UIRectFill(bounds);
    
    //Draw the tinted image in context
    [self drawInRect:bounds blendMode:blendMode alpha:1.0f];
    
    if (blendMode != kCGBlendModeDestinationIn)
    {
        [self drawInRect:bounds blendMode:kCGBlendModeDestinationIn alpha:1.0f];
    }
    
    UIImage *tintedImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    
    return tintedImage;
}

@end

#pragma mark - Categories

@implementation NSValue (UICornerInset)

+ (NSValue*)valueWithUICornerInset:(UICornerInset)cornerInset
{
    CGRect rect = CGRectMake(cornerInset.topLeft, cornerInset.topRight, cornerInset.bottomLeft, cornerInset.bottomRight);
    return [NSValue valueWithCGRect:rect];
    
    //    UICornerInset inset = cornerInset;
    //    return [[NSValue alloc] initWithBytes:&inset objCType:@encode(struct __UICornerInset)];
}

- (UICornerInset)uicornerInsetValue
{
    CGRect rect = [self CGRectValue];
    return uiCornerInsetMake(rect.origin.x, rect.origin.y, rect.size.width, rect.size.height);
    
    //    UICornerInset cornerInset;
    //    [self getValue:&cornerInset];
    //    return cornerInset;
}

@end
