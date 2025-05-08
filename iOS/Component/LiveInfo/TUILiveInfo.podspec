
Pod::Spec.new do |spec|
  spec.name                  = 'TUILiveInfo'
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
  spec.dependency 'RTCCommon', '>= 1.2.1'
  spec.dependency 'Kingfisher'

  spec.default_subspec = 'Professional'
  
  spec.subspec 'Professional' do |professional|
    professional.dependency 'RTCRoomEngine/Professional', '>= 2.9.0'
    professional.dependency 'TUILiveResources/Professional'

    professional.source_files = 'Sources/**/*'
    professional.resource_bundles = {
      'TUILiveInfoBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.xcstrings']
    }
  end
  
  spec.subspec 'TRTC' do |trtc|
    trtc.dependency 'RTCRoomEngine/TRTC', '>= 2.9.0'
    trtc.dependency 'TUILiveResources/TRTC'

    trtc.source_files = 'Sources/**/*'
    trtc.resource_bundles = {
      'TUILiveInfoBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.xcstrings']
    }
  end
  
end
