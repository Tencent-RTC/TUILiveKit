Pod::Spec.new do |s|
  s.name             = 'SeatGridView'
  s.version          = '0.1.0'
  s.summary          = 'CoreView of VoiceRoom'
  s.homepage         = 'https://trtc.io/'
  s.license          = { :type => 'MIT', :file => 'LICENSE' }
  s.author           = 'trtc.io'
  s.source           = { :path => './'}
  s.static_framework = true
  s.ios.deployment_target = '13.0'
  s.swift_version = '5.5'
  
  s.dependency 'TUICore'
  s.dependency 'SnapKit'
  s.dependency 'RTCCommon', '>= 1.1.0'
  s.dependency 'Kingfisher', '<= 6.3.1'
  
  s.default_subspec = 'Professional'
  
  s.subspec 'Professional' do |professional|
    professional.dependency 'RTCRoomEngine/Professional'
    professional.source_files = 'Sources/**/*'
    
    professional.resource_bundles = {
      'SeatGridViewBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.strings']
    }
  end
    
  s.subspec 'TRTC' do |trtc|
    trtc.dependency 'RTCRoomEngine/TRTC'
    trtc.source_files = 'Sources/**/*'
    
    trtc.resource_bundles = {
      'SeatGridViewBundle' => ['Resources/*.xcassets', 'Resources/Localized/**/*.xcstrings']
    }
  end
  
end
