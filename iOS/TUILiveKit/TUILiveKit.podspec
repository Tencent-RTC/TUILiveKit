
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

  spec.source                = { :path => './' }

  spec.dependency 'SnapKit'
  spec.dependency 'TUICore'
  spec.dependency 'Kingfisher'
  spec.dependency 'SVGAPlayer', '~> 2.5.7'
  spec.dependency 'Protobuf', '~> 3.22.1'
  spec.dependency 'RTCCommon', '>= 1.1.0'
  spec.dependency "ESPullToRefresh"

  spec.default_subspec = 'Professional'
  
  spec.subspec 'Professional' do |professional|
    professional.dependency 'RTCRoomEngine/Professional', '>= 2.9.0'
    professional.dependency 'LiveStreamCore/Professional'

    professional.source_files = 'Sources/**/*', 'VideoLiveKit.swift', 'VoiceRoomKit.swift', 'VoiceRoomDefine.swift'
    professional.resource_bundles = {
      'TUILiveKitBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.xcstrings']
    }
  end
  
  spec.subspec 'TRTC' do |trtc|
    trtc.dependency 'RTCRoomEngine/TRTC', '>= 2.9.0'
    trtc.dependency 'LiveStreamCore/TRTC'

    trtc.source_files = 'Sources/**/*', 'VideoLiveKit.swift', 'VoiceRoomKit.swift', 'VoiceRoomDefine.swift'
    trtc.resource_bundles = {
      'TUILiveKitBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.xcstrings']
    }
  end
  
end
