Pod::Spec.new do |s|
  s.name             = 'AtomicX'
  s.version          = '1.0.0'
  s.summary          = 'A collection of UI components and utilities for AtomicX.'
  s.description      = <<-DESC
  AtomicX provides a set of reusable UI components and utilities designed for modern iOS applications.
                       DESC
  s.homepage         = 'https://trtc.io/'
  s.license          = { :type => 'MIT', :file => 'LICENSE' }
  s.author           = 'trtc.io'
  s.source           = { :path => './' }
  s.ios.deployment_target = '13.0'

  # Main spec
  s.source_files     = 'Sources/**/*.{swift,h,m}'
  s.swift_version    = '5.0'
  s.frameworks       = 'UIKit', 'Foundation'

  s.dependency 'RTCRoomEngine/Professional'
  s.dependency 'TXLiteAVSDK_Professional','>= 12.8'
  s.dependency 'SnapKit'
  s.dependency 'RTCCommon'
  s.dependency 'TUICore'
  s.dependency 'Kingfisher'
  s.static_framework = true
  s.resource_bundles = {
    'AtomicXBundle' => ['Resources/assets/**/*.{xcassets,json,png}', 'Resources/strings/**/*.xcstrings' , 'Resources/KTVResource**/*.{vtt,mp3,json,flac}']
  }
end
