#
# Be sure to run `pod lib lint TEBeautyKit.podspec' to ensure this is a
# valid spec before submitting.
#
# Any lines starting with a # are optional, but their use is encouraged
# To learn more about a Podspec see https://guides.cocoapods.org/syntax/podspec.html
#

Pod::Spec.new do |s|
  s.name             = 'TEBeautyKit'
  s.version          = '1.4.0'
  s.summary          = 'A short description of TEBeautyKit.'

# This description is used to generate tags and improve search results.
#   * Think: What does it do? Why did you write it? What is the focus?
#   * Try to keep it short, snappy and to the point.
#   * Write the description between the DESC delimiters below.
#   * Finally, don't worry about the indent, CocoaPods strips it!

  s.description      = <<-DESC
TODO: Add long description of the pod here.
                       DESC

  s.homepage         = 'https://github.com/originleeli@tencent.com/TEBeautyKit'
  s.license          = { :type => 'MIT', :file => 'LICENSE' }
  s.author           = { 'originleeli@tencent.com' => 'originleeli@tencent.com' }
  s.source           = { :http => 'https://liteav.sdk.qcloud.com/app/tuikit/download/release/1.4/TEBeautyKit_iOS_1.4.0.zip' }

  s.ios.deployment_target = '13.0'
  s.static_framework = true

  s.dependency 'TUICore'
  s.dependency 'TXLiteAVSDK_Professional'
  
  s.default_subspec = 'Default'

  s.subspec 'Default' do |default|
    default.dependency 'TencentEffect_S1-07'
    default.dependency 'Masonry'
    default.dependency 'SSZipArchive'
    default.dependency 'AFNetworking'
  
    default.source_files         = ['TEBeautyKit/Classes/**/*.{m,h}','TEBeautyKit/Classes/*.{m,h}']
    default.resource             = ['TEBeautyKit/Assets/bundle/*.bundle',]
    default.public_header_files  = ['TEBeautyKit/Classes/**/*.h','TEBeautyKit/Classes/*.h']
  end

  s.subspec 'RTCube' do |rtcube|
    rtcube.dependency 'TencentEffect'
    rtcube.dependency 'SnapKit'

    rtcube.source_files              = 'Source/TEBeautyKit/*.{m,h,swift}', 'Source/Extension/*.swift'
    rtcube.resource_bundles          = {
      'BeautyKitBundle'              => ['Resources/localized/*.xcstrings', 'Resources/Beauty/*.json']
    }
  end


end
