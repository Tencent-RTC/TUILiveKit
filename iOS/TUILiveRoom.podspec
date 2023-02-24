Pod::Spec.new do |spec|
  spec.name         = 'TUILiveRoom'
  spec.version      = '1.0.0'
  spec.platform     = :ios
  spec.ios.deployment_target = '11.0'
  spec.license      = { :type => 'MIT', :file => 'LICENSE' }
  spec.homepage     = 'https://cloud.tencent.com/document/product/269/3794'
  spec.documentation_url = 'https://cloud.tencent.com/document/product/269/9147'
  spec.authors      = 'tencent video cloud'
  spec.summary      = 'TUILiveRoom'
  spec.xcconfig     = { 'VALID_ARCHS' => 'armv7 arm64 x86_64' }
  spec.swift_version = '5.0'

  spec.dependency 'TXAppBasic'
  spec.dependency 'TUICore/ImSDK_Scenario'
  spec.dependency 'TUIBeauty'
  spec.dependency 'TUIBarrage'
  spec.dependency 'TUIGift'
#  Swift第三方库
  spec.dependency 'Alamofire'
  spec.dependency 'SnapKit'
  spec.dependency 'Toast-Swift'
  spec.dependency 'Kingfisher', '<= 6.3.1'
  spec.dependency 'SSZipArchive'
 
#  OC第三方库
  spec.dependency 'Masonry'
  spec.dependency 'CWStatusBarNotification', '~> 2.3.5'
  spec.dependency 'MBProgressHUD', '~> 1.2.0'
  spec.dependency 'MJExtension'
  spec.dependency 'MJRefresh'
  spec.dependency 'SDWebImage'
  
  spec.requires_arc = true
  spec.static_framework = true
  spec.source = { :path => './' }
  spec.source_files = 'Source/**/*.{h,m,mm,swift}', 'Source/*.{h,m,mm,swift}'
  spec.pod_target_xcconfig = {
    'EXCLUDED_ARCHS[sdk=iphonesimulator*]' => 'arm64'
  }
  spec.user_target_xcconfig = { 'EXCLUDED_ARCHS[sdk=iphonesimulator*]' => 'arm64' }
 
  spec.default_subspec = 'TRTC'
  spec.subspec 'TRTC' do |trtc|
    trtc.dependency 'TXLiteAVSDK_TRTC'
    trtc.dependency 'TUIAudioEffect/TRTC'
    trtc.source_files = 'Source/localized/**/*.{h,m,mm,swift}', 'Source/model/**/*.{h,m,mm,swift}', 'Source/Category/**/*.{h,m,mm,swift}', 'Source/ui/**/*.{h,m,mm,swift}', 'Source/TUILiveRoomKit_TRTC/*.{h,m,mm,swift}', 'Source/TUILiveRoom.{h,swift}'
    trtc.ios.framework = ['AVFoundation', 'Accelerate']
    trtc.library = 'c++', 'resolv'
    trtc.resource_bundles = {
     'TUILiveRoomKitBundle' => ['Resources/Localized/**/*.strings','Resources/*.xcassets']
    }
  end
 
  spec.subspec 'Enterprise' do |enterprise|
    enterprise.dependency 'TXLiteAVSDK_Enterprise'
    enterprise.dependency 'TUIAudioEffect/Enterprise'
    enterprise.source_files = 'Source/localized/**/*.{h,m,mm,swift}', 'Source/model/**/*.{h,m,mm,swift}', 'Source/Category/**/*.{h,m,mm,swift}', 'Source/ui/**/*.{h,m,mm,swift}', 'Source/TUILiveRoomKit_Enterprise/*.{h,m,mm,swift}', 'Source/TUILiveRoom.{h,swift}'
    enterprise.ios.framework = ['AVFoundation', 'Accelerate', 'AssetsLibrary']
    enterprise.library = 'c++', 'resolv', 'sqlite3'
    enterprise.resource_bundles = {
      'TUILiveRoomKitBundle' => ['Resources/Localized/**/*.strings','Resources/*.xcassets']
    }
  end
  
  spec.subspec 'Professional' do |professional|
    professional.dependency 'TXLiteAVSDK_Professional'
    professional.dependency 'TUIAudioEffect/Professional'
    professional.source_files = 'Source/localized/**/*.{h,m,mm,swift}', 'Source/model/**/*.{h,m,mm,swift}', 'Source/Category/**/*.{h,m,mm,swift}', 'Source/ui/**/*.{h,m,mm,swift}', 'Source/TUILiveRoomKit_Professional/*.{h,m,mm,swift}', 'Source/TUILiveRoom.{h,swift}'
    professional.ios.framework = ['AVFoundation', 'Accelerate', 'AssetsLibrary']
    professional.library = 'c++', 'resolv', 'sqlite3'
    professional.resource_bundles = {
      'TUILiveRoomKitBundle' => ['Resources/Localized/**/*.strings','Resources/*.xcassets']
    }
  end
end

