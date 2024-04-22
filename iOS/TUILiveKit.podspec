
Pod::Spec.new do |spec|
  spec.name                  = 'TUILiveKit'
  spec.version               = '1.0.0'
  spec.platform              = :ios
  spec.ios.deployment_target = '13.0'
  spec.license               = { :type => 'MIT', :file => 'LICENSE' }
  spec.homepage              = 'https://trtc.io/'
  spec.documentation_url     = 'https://trtc.io/document'
  spec.authors               = 'trtc.io'
  spec.summary               = 'trtc.io for Live streaming Solution..'
  
  spec.static_framework = true
  spec.xcconfig      = { 'VALID_ARCHS' => 'armv7 arm64 x86_64' }
  spec.swift_version = '5.0'

  spec.source                = { :git => 'https://github.com/tencentyun/TUILiveKit.git',
:tag => "v#{spec.version}" }

  spec.dependency 'SnapKit'
  spec.dependency 'TUICore'
  spec.dependency 'Kingfisher', '<= 6.3.1'
  spec.dependency 'SVGAPlayer', '~> 2.5.7'
  spec.dependency 'Protobuf', '~> 3.22.1'

  spec.default_subspec = 'TRTC'
  
  spec.subspec 'Professional' do |professional|
    professional.dependency 'RTCRoomEngine/Professional', '>= 2.3.0'
    professional.pod_target_xcconfig = {'OTHER_SWIFT_FLAGS' => '-D TXLiteAVSDK_Professional', 'GCC_PREPROCESSOR_DEFINITIONS' => 'TXLiteAVSDK_Professional=1'}
    professional.source_files = 'Source/**/*'
    professional.resource_bundles = {
      'TUILiveKitBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.strings']
    }
  end
  
  spec.subspec 'TRTC' do |trtc|
    trtc.dependency 'RTCRoomEngine/TRTC', '>= 2.3.0'
    trtc.pod_target_xcconfig = {'OTHER_SWIFT_FLAGS' => '-D TXLiteAVSDK_TRTC', 'GCC_PREPROCESSOR_DEFINITIONS' => '$(inherited) COCOAPODS=1 TXLiteAVSDK_TRTC=1'}
    trtc.source_files = 'Source/**/*'
    trtc.resource_bundles = {
      'TUILiveKitBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.strings']
    }
  end
  
end
