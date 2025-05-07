
Pod::Spec.new do |spec|
  spec.name                  = 'TUILiveResources'
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

  spec.default_subspec = 'Professional'
  
  spec.subspec 'Professional' do |professional|
    professional.dependency 'SnapKit'
    professional.dependency 'TUICore'
    professional.dependency 'RTCCommon', '>= 1.1.0'
    professional.dependency 'RTCRoomEngine/Professional', '>= 2.9.0'

    professional.source_files = 'Sources/**/*'
    professional.resource_bundles = {
      'TUILiveResourcesBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.xcstrings']
    }
  end
  
  spec.subspec 'TRTC' do |trtc|
    trtc.dependency 'SnapKit'
    trtc.dependency 'TUICore'
    trtc.dependency 'RTCCommon', '>= 1.1.0'
    trtc.dependency 'RTCRoomEngine/TRTC', '>= 2.9.0'

    trtc.source_files = 'Sources/**/*'
    trtc.resource_bundles = {
      'TUILiveResourcesBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.xcstrings']
    }
  end
  
end
