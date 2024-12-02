Pod::Spec.new do |s|
  s.name             = 'LiveStreamCore'
  s.version          = '0.1.0'
  s.summary          = 'CoreView of LiveRoom.'
  s.homepage         = 'https://trtc.io/'
  s.license          = { :type => 'MIT', :file => 'LICENSE' }
  s.author           = 'trtc.io' 
  s.source           = { :path => './' }
  s.static_framework = true

  s.ios.deployment_target = '13.0'
  
  s.dependency 'TUICore'
  s.dependency 'SnapKit'
  s.dependency 'RTCCommon', '>= 1.1.0'
  
  s.default_subspec = 'Professional'
  
  s.subspec 'Professional' do |professional|
    professional.dependency 'RTCRoomEngine/Professional'
    professional.source_files = 'Sources/**/*'
    
    professional.resource_bundles = {
      'LiveStreamCoreBundle' => ['Resources/Localized/**/*.xcstrings', 'Resources/*.json']
    }
  end
    
  s.subspec 'TRTC' do |trtc|
    trtc.dependency 'RTCRoomEngine/TRTC'
    trtc.source_files = 'Sources/**/*'
    
    trtc.resource_bundles = {
      'LiveStreamCoreBundle' => ['Resources/Localized/**/*.xcstrings', 'Resources/*.json']
    }
  end
  
end
