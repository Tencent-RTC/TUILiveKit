#
# Be sure to run `pod lib lint TCEffectPlayerKit.podspec' to ensure this is a
# valid spec before submitting.
#
# Any lines starting with a # are optional, but their use is encouraged
# To learn more about a Podspec see https://guides.cocoapods.org/syntax/podspec.html
#

Pod::Spec.new do |s|
  s.name             = 'TCEffectPlayerKit'
  s.version          = '1.0.0'
  s.summary          = 'A short description of TCEffectPlayerKit.'
  s.homepage         = 'https://github.com/originleeli@tencent.com/TCEffectPlayerKit'
  s.license          = { :type => 'MIT', :file => 'LICENSE' }
  s.author           = { 'originleeli@tencent.com' => 'originleeli@tencent.com' }
  s.source           = { :http => 'https://liteav.sdk.qcloud.com/app/tuikit/download/release/1.0/TCEffectPlayerKit.zip' }
  s.ios.deployment_target = '13.0'
  s.ios.framework    = ['AVFoundation','Security']
  s.ios.library = 'z', 'c++'
  s.requires_arc = true
  s.static_framework = true
  s.dependency 'TUICore'
  s.vendored_frameworks = 'TCEffectPlayerKit/Frameworks/libtcpag.xcframework', 'TCEffectPlayerKit/Frameworks/TCMediaX.xcframework', 'TCEffectPlayerKit/Frameworks/TCEffectPlayer.xcframework'
  s.source_files = 'TCEffectPlayerKit/Classes/**/*.*'
  s.pod_target_xcconfig = {
    'OTHER_LDFLAGS' => '-ObjC'
  }
 s.resources = 'TCEffectPlayerKit/Frameworks/TCMediaX.xcframework/TCMediaX-Privacy.bundle', 'TCEffectPlayerKit/Frameworks/TCMediaX.xcframework/TCEffectPlayer-Privacy.bundle'
  
end
