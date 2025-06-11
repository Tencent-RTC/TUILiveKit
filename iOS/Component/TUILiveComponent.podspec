
Pod::Spec.new do |spec|
  spec.name                  = 'TUILiveComponent'
  spec.version               = '1.0.0'
  spec.platform              = :ios
  spec.ios.deployment_target = '13.0'
  spec.license               = { :type => 'MIT', :file => 'LICENSE' }
  spec.homepage              = 'https://trtc.io/'
  spec.documentation_url     = 'https://trtc.io/document'
  spec.authors               = 'trtc.io'
  spec.summary               = 'trtc.io for Live streaming Solution..'
  
  spec.static_framework = true
  spec.swift_version = '5.0'

  spec.source                = { :path => './' }

  spec.default_subspec = 'Professional'
  
  spec.subspec 'Professional' do |professional|
    professional.dependency 'RTCRoomEngine/Professional'
    professional.dependency 'SnapKit'
    professional.dependency 'TUICore'
    professional.dependency 'RTCCommon', '>= 1.2.1'
    professional.dependency 'Kingfisher'
    professional.dependency 'SVGAPlayer'

    professional.source_files = '*/Sources/**/*'
    professional.resource_bundles = {
      'TUILiveComponentBundle' => ['*/Resources/*.xcassets', '*/Resources/Localized/**/*.xcstrings']
    }
  end
  
  spec.subspec 'TRTC' do |trtc|
    trtc.dependency 'RTCRoomEngine/TRTC'
    trtc.dependency 'SnapKit'
    trtc.dependency 'TUICore'
    trtc.dependency 'RTCCommon', '>= 1.2.1'
    trtc.dependency 'Kingfisher'
    trtc.dependency 'SVGAPlayer'

    trtc.source_files = '*/Sources/**/*'
    trtc.resource_bundles = {
      'TUILiveComponentBundle' => ['*/Resources/*.xcassets', '*/Resources/Localized/**/*.xcstrings']
    }
  end
  
end
