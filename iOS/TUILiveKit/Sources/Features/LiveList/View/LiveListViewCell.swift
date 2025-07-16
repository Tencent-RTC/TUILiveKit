//
//  LiveListVIewCell.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/4/15.
//

import LiveStreamCore
import TUICore

class LiveListViewCell: UICollectionViewCell {
    static let identifier = "LiveListViewCell"
    
    private var roomId: String?
    private lazy var coreView = LiveCoreView()
    
    private lazy var imageBgView: UIImageView = {
        let view = UIImageView(frame: .zero)
        return view
    }()
    
    private lazy var blurView: UIVisualEffectView = {
        let effect = UIBlurEffect(style: .light)
        let blurView = UIVisualEffectView(effect: effect)
        return blurView
    }()
    
    override init(frame: CGRect) {
        super.init(frame: .zero)
        self.layer.cornerRadius = 10
        self.layer.masksToBounds = true
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    override func prepareForReuse() {
        super.prepareForReuse()
        stopPreload()
        self.roomId = nil
    }
    
    private func constructViewHierarchy() {
        insertSubview(imageBgView, at: 0)
        insertSubview(coreView, at: 1)
    }
    
    private func activateConstraints() {
        imageBgView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        coreView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func addBlurEffect() {
        insertSubview(blurView, aboveSubview: imageBgView)
        blurView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func removeBlurEffect() {
        blurView.removeFromSuperview()
    }
    
    func updateView(liveInfo: LiveInfo) {
        imageBgView.sd_setImage(with: URL(string: liveInfo.coverUrl),
                                placeholderImage: internalImage("live_edit_info_default_cover_image"))
    }
    
    func startPreload(roomId: String, isMuteAudio: Bool = true) {
        self.roomId = roomId
        if FloatWindow.shared.isShowingFloatWindow() && FloatWindow.shared.getCurrentRoomId() == roomId {
            coreView.isHidden = true
            LiveKitLog.info("\(#file)","\(#line)", "float window view is showing, startPreload ignore, roomId: \(roomId)")
            return
        }
        coreView.isHidden = false
        coreView.startPreviewLiveStream(roomId: roomId, isMuteAudio: isMuteAudio) { _ in
        } onLoading: { _ in
        } onError: { [weak self] _, _, _ in
            guard let self = self, self.roomId == roomId else { return }
            coreView.isHidden = true
        }
    }
    
    func stopPreload() {
        coreView.isHidden = true
        guard let roomId = roomId else { return }
        if FloatWindow.shared.isShowingFloatWindow() && FloatWindow.shared.getCurrentRoomId() == roomId {
            LiveKitLog.info("\(#file)","\(#line)", "float window view is showing, stopPreload ignore, roomId: \(String(describing: roomId))")
            return
        }
        coreView.stopPreviewLiveStream(roomId: roomId)
    }
    
    func unmutePreviewVideoStream() {
        guard let roomId = roomId else { return }
        if FloatWindow.shared.isShowingFloatWindow(), let ownerId = FloatWindow.shared.getRoomOwnerId(), ownerId == TUILogin.getUserID() {
            LiveKitLog.info("\(#file)","\(#line)", "Anchor FloatWindow is showing, unmutePreviewVideoStream ignore, roomId:\(roomId)")
            return
        }
        LiveKitLog.info("\(#file)","\(#line)", "unmutePreviewVideoStream roomId:\(roomId)")
        coreView.startPreviewLiveStream(roomId: roomId, isMuteAudio: false) { [weak self] _ in
            guard let self = self, self.roomId == roomId else { return }
            coreView.isHidden = false
        } onLoading: { _ in
        } onError: { [weak self] _, _, _ in
            guard let self = self, self.roomId == roomId else { return }
            coreView.isHidden = true
        }
    }
}
