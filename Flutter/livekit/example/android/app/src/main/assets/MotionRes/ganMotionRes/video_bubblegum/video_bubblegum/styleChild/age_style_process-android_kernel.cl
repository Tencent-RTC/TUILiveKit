#pragma OPENCL EXTENSION cl_khr_fp16 : enable
__kernel void age_style_preprocess_image(__private const int global_size_dim0,
                                 __private const int global_size_dim1,
                                 __write_only image2d_t input_ptr,
                                 __private const int channel_up_4,
                                 __private const int height,
                                 __private const int width,
                                 __global uchar *input_camera) {
  __const sampler_t SAMPLER =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;
  int image_width_idx = get_global_id(0);
  int image_height_idx = get_global_id(1);

  if (image_width_idx >= global_size_dim0 ||
      image_height_idx >= global_size_dim1) {
      return;
  }
  const int channel_cl = image_width_idx / width;
  const int offset_input = (image_height_idx * width + image_width_idx % width)*4;
  uchar4 input_camera4 = vload4(0, input_camera + offset_input);
  int2 coord = (int2)(image_width_idx, image_height_idx);
  half4 values4 = convert_half4(input_camera4);
  write_imageh(input_ptr, coord, values4);
};
__kernel void age_style_postprocess(__private const int global_size_dim0,
                                 __private const int global_size_dim1,
                                 __read_only image2d_t input_ptr,
                                 __private const int channel_up_4,
                                 __private const int height,
                                 __private const int width,
                                  __read_only image2d_t input_ptr_2,
                                 __private const int channel_up_4_2,
                                 __private const int height_2,
                                 __private const int width_2,
                                 __global uchar *output,
                                 __global uchar *output_2,
                                 __global uchar *inputmatte) {
  __const sampler_t SAMPLER =
      CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP | CLK_FILTER_NEAREST;
  int image_width_idx = get_global_id(0);
  int image_height_idx = get_global_id(1);
  if (image_width_idx >= global_size_dim0 ||
      image_height_idx >= global_size_dim1) {
    return;
  }
  const int batch_idx = image_height_idx / height;
  const int height_idx = image_height_idx % height;
  const int width_idx = image_width_idx % width;
  int channel_block_idx = image_width_idx / width;

  int buffer_offset =
      (((batch_idx * channel_up_4 + channel_block_idx) * height + height_idx) *
           width +
       width_idx) *
      4;
  int warp_offset_x = (((batch_idx * channel_up_4 + channel_block_idx) * height + height_idx) * width + width_idx)*4;
  int warp_offset_y = warp_offset_x + height * width;
  int2 coord = (int2)(image_width_idx, image_height_idx);
  uchar4 values = convert_uchar4_sat(read_imageh(input_ptr, SAMPLER, coord));
  vstore4(values, 0, output + buffer_offset);
  float4 mattevalues = convert_float4(vload4(0, inputmatte + buffer_offset))/255.0f;
  half4 warphalf = read_imageh(input_ptr_2, SAMPLER, coord) * (half4)(mattevalues.x, mattevalues.x, mattevalues.x, mattevalues.x);
  warphalf = (warphalf * 0.5h + 0.5h) * 255.0h;
  half2 a = floor(warphalf.xy);
  half2 b = fract(warphalf.xy, &a) * 255.0h;
  output_2[warp_offset_x] = convert_uchar(clamp(a.x, 0.0h, 255.0h));
  output_2[warp_offset_x + 1] = convert_uchar(clamp(a.y, 0.0h, 255.0h));
  output_2[warp_offset_x + 2] = convert_uchar(clamp(b.x, 0.0h, 255.0h));
  output_2[warp_offset_x + 3] = convert_uchar(clamp(b.y, 0.0h, 255.0h));
};