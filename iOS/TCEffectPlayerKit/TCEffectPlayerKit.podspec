#
# Be sure to run `pod lib lint TCEffectPlayerKit.podspec' to ensure this is a
# valid spec before submitting.
#
# Any lines starting with a # are optional, but their use is encouraged
# To learn more about a Podspec see https://guides.cocoapods.org/syntax/podspec.html
#

Pod::Spec.new do |s|
  s.name             = 'TCEffectPlayerKit'
  s.version          = '3.0.0'
  s.summary          = 'A plugin of TCEffectPlayer for TUILiveKit.'
  s.homepage         = 'https://github.com/Tencent-RTC/TUILiveKit'
  s.license          = { :type => 'MIT', :file => 'LICENSE' }
  s.author           = 'trtc.io'
  s.source           = { :path => './' }
  s.ios.deployment_target = '13.0'
  s.static_framework = true
  
  s.dependency 'TUICore'

  s.default_subspec = 'Default'
  
  s.subspec 'Default' do |default|
      default.dependency 'TCMediaX'
      default.dependency 'TCEffectPlayer'
      default.dependency 'YTCommonXMagic'
      default.source_files = 'Classes/**/*.*'
  end
  
  s.subspec 'Beauty' do |beauty|
      beauty.dependency 'TCMediaX'
      beauty.dependency 'TCEffectPlayer'
      beauty.source_files = 'Classes/**/*.*'
  end
  
end
