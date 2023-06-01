#
# Be sure to run `pod lib lint TUIAudioEffect.podspec' to ensure this is a
# valid spec before submitting.
#
# Any lines starting with a # are optional, but their use is encouraged
# To learn more about a Podspec see https://guides.cocoapods.org/syntax/podspec.html
#

Pod::Spec.new do |spec|
  spec.name             = 'TUIAudioEffect'
  spec.version          = '1.0.0'
  spec.platform     = :ios
  spec.ios.deployment_target = '9.0'
  spec.license      = { :type => 'MIT', :file => 'LICENSE' }
  
  spec.homepage     = 'https://cloud.tencent.com/document/product/269/3794'
  spec.documentation_url = 'https://cloud.tencent.com/document/product/269/9147'
  spec.author           = 'tencent video cloud'
  spec.summary          = 'TUIAudioEffect'
  spec.source           = { :git => '', :tag => "#{spec.version}" }
  spec.xcconfig     = { 'VALID_ARCHS' => 'armv7 arm64 x86_64' }

  spec.requires_arc = true
  spec.static_framework = true
  spec.pod_target_xcconfig = {
        'EXCLUDED_ARCHS[sdk=iphonesimulator*]' => 'arm64'
      }
  spec.user_target_xcconfig = { 'EXCLUDED_ARCHS[sdk=iphonesimulator*]' => 'arm64' }
      
  spec.dependency 'Masonry'
  spec.dependency 'TUICore'
  
  spec.default_subspec = 'Live'
  
  spec.subspec 'Live' do |live|
    live.dependency 'TXLiteAVSDK_Live'
    live.xcconfig = { 'HEADER_SEARCH_PATHS' => '${SRCROOT}/../SDK/TXLiteAVSDK_Live.framework/Headers/'}
    live.ios.framework = ['AVFoundation', 'Accelerate', 'AssetsLibrary']
    live.library = 'c++', 'resolv', 'sqlite3'
    live.source_files = 'Source/Localized/**/*.{h,m,mm}', 'Source/TUIExtension/**/*.{h,m,mm}', 'Source/Model/**/*.{h,m,mm,c}', 'Source/View/**/*.{h,m,mm}', 'Source/Presenter/**/*.{h,m,mm}', 'Source/TUIAudioEffectViewKit_Live/*.{h,m,mm}'
    live.resource_bundles = {
      'TUIAudioEffectKitBundle' => ['Resource/Localized/**/*.strings', 'Resource/*.xcassets']
    }
  end

  spec.subspec 'TRTC' do |trtc|
    trtc.dependency 'TXLiteAVSDK_TRTC'
    trtc.xcconfig = { 'HEADER_SEARCH_PATHS' => '${SRCROOT}/../SDK/TXLiteAVSDK_TRTC.framework/Headers/'}
    trtc.ios.framework = ['AVFoundation', 'Accelerate', 'AssetsLibrary']
    trtc.library = 'c++', 'resolv', 'sqlite3'
    trtc.source_files = 'Source/Localized/**/*.{h,m,mm}', 'Source/TUIExtension/**/*.{h,m,mm}', 'Source/Model/**/*.{h,m,mm,c}', 'Source/View/**/*.{h,m,mm}', 'Source/Presenter/**/*.{h,m,mm}', 'Source/TUIAudioEffectViewKit_TRTC/*.{h,m,mm}'
    trtc.resource_bundles = {
      'TUIAudioEffectKitBundle' => ['Resource/Localized/**/*.strings', 'Resource/*.xcassets']
    }
  end
  
  spec.subspec 'Professional' do |professional|
    professional.dependency 'TXLiteAVSDK_Professional'
    professional.xcconfig = { 'HEADER_SEARCH_PATHS' => '${SRCROOT}/../SDK/TXLiteAVSDK_Professional.framework/Headers/'}
    professional.ios.framework = ['AVFoundation', 'Accelerate', 'AssetsLibrary']
    professional.library = 'c++', 'resolv', 'sqlite3'
    professional.source_files = 'Source/Localized/**/*.{h,m,mm}', 'Source/TUIExtension/**/*.{h,m,mm}', 'Source/Model/**/*.{h,m,mm,c}', 'Source/View/**/*.{h,m,mm}', 'Source/Presenter/**/*.{h,m,mm}', 'Source/TUIAudioEffectViewKit_Professional/*.{h,m,mm}'
    professional.resource_bundles = {
      'TUIAudioEffectKitBundle' => ['Resource/Localized/**/*.strings', 'Resource/*.xcassets']
    }
  end
  
  spec.subspec 'Enterprise' do |enterprise|
    enterprise.dependency 'TXLiteAVSDK_Enterprise'
    enterprise.xcconfig = { 'HEADER_SEARCH_PATHS' => '${SRCROOT}/../SDK/TXLiteAVSDK_Enterprise.framework/Headers/'}
    enterprise.ios.framework = ['AVFoundation', 'Accelerate', 'AssetsLibrary']
    enterprise.library = 'c++', 'resolv', 'sqlite3'
    enterprise.source_files = 'Source/Localized/**/*.{h,m,mm}', 'Source/TUIExtension/**/*.{h,m,mm}', 'Source/Model/**/*.{h,m,mm,c}', 'Source/View/**/*.{h,m,mm}', 'Source/Presenter/**/*.{h,m,mm}', 'Source/TUIAudioEffectViewKit_Enterprise/*.{h,m,mm}'
    enterprise.resource_bundles = {
      'TUIAudioEffectKitBundle' => ['Resource/Localized/**/*.strings', 'Resource/*.xcassets']
    }
  end

end
