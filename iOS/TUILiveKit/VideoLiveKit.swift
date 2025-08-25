//
//  VideoLiveKit.swift
//  TUILiveKit-TUILiveKitBundle
//
//  Created by jack on 2024/9/29.
//

import Foundation
import TUICore
import RTCRoomEngine

@objcMembers
public class VideoLiveKit: NSObject {
    
    private static let sharedInstance = VideoLiveKit()
    
    private override init() {}
    
    private weak var viewController: UIViewController?
    
    public static func createInstance() -> VideoLiveKit {
        return sharedInstance
    }
    
    @MainActor
    public func enableFollowFeature(_ enable: Bool) {
        enableFollow = enable
    }
    
    @MainActor
    public func startLive(roomId: String) {
        
        if FloatWindow.shared.isShowingFloatWindow() {
            if let ownerId = FloatWindow.shared.getRoomOwnerId(), ownerId == TUILogin.getUserID() {
                getRootController()?.view.makeToast(.pushingToReturnText)
                return
            } else if FloatWindow.shared.getIsLinking() {
                getRootController()?.view.makeToast(.pushingToReturnText)
                return
            }
        }
        let listManager = TUIRoomEngine.sharedInstance().getExtension(extensionType: .liveListManager)as? TUILiveListManager
        guard let listManager = listManager else {return}
        listManager.getLiveInfo(roomId){[weak self] liveInfo in
            guard let self = self else { return }
            if liveInfo.keepOwnerOnSeat == true {
                self.showPrepareViewController(roomId: roomId)
            } else {
                self.showAnchorViewController(roomId: roomId)
            }
        } onError: { [weak self] error, message in
            guard let self = self else { return }
            self.showPrepareViewController(roomId: roomId)
        }
    }
    
    @MainActor
    public func stopLive(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        guard let vc = viewController as? TUILiveRoomAnchorViewController else {
            onSuccess?()
            return
        }
        vc.stopLive { [weak self] in
            guard let self = self else { return }
            onSuccess?()
            self.viewController = nil
        } onError: { code, message in
            onError?(code, message)
        }
    }
    
    @MainActor
    public func joinLive(roomId: String) {
        
        let viewController = TUILiveRoomAudienceViewController(roomId: roomId)
        viewController.modalPresentationStyle = .fullScreen
        
        getRootController()?.present(viewController, animated: true)
        self.viewController = viewController
    }
    
    @MainActor
    public func leaveLive(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        if FloatWindow.shared.isShowingFloatWindow() {
            FloatWindow.shared.releaseFloatWindow()
            onSuccess?()
        } else if let vc = viewController as? TUILiveRoomAudienceViewController {
            vc.leaveLive { [weak self] in
                guard let self = self else { return }
                self.viewController?.dismiss(animated: true)
                self.viewController = nil
                onSuccess?()
            } onError: { code, message in
                onError?(code, message)
            }
        } else if let vc = viewController as? TUILiveRoomAnchorViewController  {
            vc.getCoreView().leaveLiveStream { [weak self] in
                guard let self = self else { return }
                self.viewController?.dismiss(animated: true)
                self.viewController = nil
                onSuccess?()
            } onError: { error, message in
                onError?(error, message)
            }
        } else {
            onSuccess?()
        }
    }
    
    var enableFollow: Bool = true
}

// MARK: - Private
extension VideoLiveKit {
    
    private func getRootController() -> UIViewController? {
        return TUITool.applicationKeywindow().rootViewController
    }

    private func showPrepareViewController(roomId: String) {
        let vc = TUILiveRoomAnchorPrepareViewController(roomId: roomId)
        vc.modalPresentationStyle = .fullScreen
        vc.willStartLive = { [weak self] controller in
            guard let self = self else { return }
            self.viewController = controller
        }
        getRootController()?.present(vc, animated: true)
    }

    private func showAnchorViewController(roomId: String) {
        var liveInfo = LiveInfo()
        liveInfo.roomId = roomId
        let anchorVC = TUILiveRoomAnchorViewController(liveInfo: liveInfo, behavior: .enterRoom)
        anchorVC.modalPresentationStyle = .fullScreen
        getRootController()?.present(anchorVC, animated: true)
    }
}

extension String {
    fileprivate static let pushingToReturnText = internalLocalized("Live streaming in progress. Please try again later.")
}
