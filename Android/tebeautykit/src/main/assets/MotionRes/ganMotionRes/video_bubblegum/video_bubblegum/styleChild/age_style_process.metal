//  Copyright © 2019 tencent. All rights reserved.

#include <metal_stdlib>

using namespace metal;

kernel void age_style_preprocess_image(texture2d<half, access::read> src_bgra       [[texture(0)]],
                                       device half4 *dst0                              [[buffer(0)]],
                                       ushort2 gid                                     [[thread_position_in_grid]])
{
    if (any(gid >= ushort2(src_bgra.get_width(), src_bgra.get_height())))
        return;
    const half4 in = src_bgra.read(uint2(gid));
    auto out = dst0 + (int)gid.y * src_bgra.get_width() + (int)gid.x;
    
    *out = in.xyzw*255.0h;
}

kernel void age_style_postprocess(texture2d<half, access::write> dst_bgra0    [[texture(0)]],
                                  texture2d<half, access::read> src_bgra      [[texture(1)]],
                                  texture2d<half, access::write> dst_warp     [[texture(2)]],
                                  texture2d<half, access::read> mat           [[texture(3)]],
                                  texture2d<half, access::read> face_mask     [[texture(4)]],
                                  const device half4 *src0                    [[buffer(0)]],
                                  const device half4 *src1                    [[buffer(1)]],
                                  ushort2 gid                                 [[thread_position_in_grid]])
{
    if (any(gid >= ushort2(dst_bgra0.get_width(), dst_bgra0.get_height())))
        return;
    half4 in0  = src0[(int)gid.y * dst_bgra0.get_width() + (int)gid.x] / 255.0h;
    dst_bgra0.write(in0, uint2(gid));
    half4 warp  = src1[(int)gid.y * dst_bgra0.get_width() + (int)gid.x] * mat.read(uint2(gid)).r;
    warp = (warp * 0.5 + 0.5) * 255.0;
    
    half2 a = floor(warp.xy) / 255.0;
    half2 b = fract(warp.xy);
    dst_warp.write(half4(a,b), uint2(gid));
}
