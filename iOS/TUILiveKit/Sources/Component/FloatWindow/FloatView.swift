//
//  FloatView.swift
//  TUILiveKit
//
//  Created by gg on 2024/11/28.
//

import Foundation
import SnapKit

protocol FloatViewDelegate: AnyObject {
    func onClose()
    func onResume()
}

class FloatView: UIView {
    
    weak var delegate: FloatViewDelegate?
    
    private let closeButton: UIButton = {
        var button = UIButton(type: .custom)
        button.setImage(.liveBundleImage("live_music_delete"), for: .normal)
        button.isUserInteractionEnabled = true
        return button
    }()
    
    private let gestureOverlayView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        view.translatesAutoresizingMaskIntoConstraints = false
        return view
    }()
    
    
    private let contentView: UIView
    
    private var margin : CGFloat = 10
    
    init(contentView: UIView) {
        let screenWidth = UIScreen.main.bounds.width
        let screenHeight = UIScreen.main.bounds.height
        let floatViewWidth: CGFloat = screenWidth * 0.293333
        let floatViewHeight: CGFloat = screenHeight * 0.23152
        let initialX = screenWidth - floatViewWidth - margin
        let initialY = screenHeight * 0.1
        self.contentView = contentView
        super.init(frame: CGRect(x: initialX, y: initialY, width: floatViewWidth, height: floatViewHeight))
        setViewStyle()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func setViewStyle(){
        backgroundColor = .black
        layer.borderColor = UIColor(0xD1D9EC).cgColor
        layer.borderWidth = 1.5
        
        layer.shadowColor = UIColor(0x152960, alpha: 0.1).cgColor
        layer.shadowOpacity = 1
        layer.shadowOffset = CGSize(width: CGFloat.zero, height: CGFloat.zero)
    
        layer.cornerRadius = 9
        layer.shouldRasterize = true
        layer.rasterizationScale = UIScreen.main.scale
        
        clipsToBounds = true
    }

    private func constructViewHierarchy() {
        addSubview(contentView)
        addSubview(gestureOverlayView)
        addSubview(closeButton)
    }
    
    private func activateConstraints() {
        contentView.snp.makeConstraints { make in
            make.size.equalToSuperview()
        }
        
        gestureOverlayView.snp.makeConstraints{ make in
            make.size.equalToSuperview()
        }
        
        closeButton.snp.makeConstraints { make in
            make.top.trailing.equalToSuperview().inset(5)
            make.width.height.equalTo(20)
        }
    }
    
    private func bindInteraction() {
        let tapGesture = UITapGestureRecognizer(target: self, action: #selector(handleTap))
        gestureOverlayView.addGestureRecognizer(tapGesture)
        
        let panGesture = UIPanGestureRecognizer(target: self, action: #selector(handlePan))
        gestureOverlayView.addGestureRecognizer(panGesture)
        
        tapGesture.require(toFail: panGesture)
        
        closeButton.addTarget(self, action: #selector(closeButtonClick), for: .touchUpInside)
    }
    
    override func layoutSubviews() {
        contentView.snp.remakeConstraints{ make in
            make.size.equalToSuperview()
        }
        gestureOverlayView.snp.remakeConstraints{ make in
            make.size.equalToSuperview()
        }
    }
    
    @objc private func handleTap() {
        delegate?.onResume()
    }
    
    @objc private func closeButtonClick() {
        delegate?.onClose()
    }
    
    @objc private func handlePan(_ gestureRecognizer: UIPanGestureRecognizer) {
        let translation = gestureRecognizer.translation(in: UIApplication.shared.windows.last!)
        
        switch gestureRecognizer.state {
            case .began, .changed:
                self.center = CGPoint(x: self.center.x + translation.x, y: self.center.y + translation.y)
                gestureRecognizer.setTranslation(CGPoint.zero, in: UIApplication.shared.windows.last!)
                
                adjustPosition()
                
            case .ended, .cancelled:
                
                snapToEdge()
                
            default:
                break
        }
    }
    
    private func adjustPosition() {
        contentView.snp.remakeConstraints { make in
            make.size.equalToSuperview()
            make.center.equalTo(self.center)
        }
        
        gestureOverlayView.snp.remakeConstraints{ make in
            make.size.equalToSuperview()
            make.center.equalTo(self.center)
        }
    }
    
    func snapToEdge() {
        let screenWidth = UIScreen.main.bounds.width
        let screenHeight = UIScreen.main.bounds.height
        let floatWindowWidth = frame.width
        let floatWindowHeight = frame.height
        
        let safeAreaInsets = UIApplication.shared.windows.first?.safeAreaInsets ?? .zero
        
        let currentCenter = self.center
        
        let distanceToLeftEdge = currentCenter.x - floatWindowWidth / 2 - safeAreaInsets.left
        let distanceToRightEdge = screenWidth - (currentCenter.x + floatWindowWidth / 2 + safeAreaInsets.right)
        let distanceToTopEdge = currentCenter.y - floatWindowHeight / 2 - safeAreaInsets.top
        let distanceToBottomEdge = screenHeight - (currentCenter.y + floatWindowHeight / 2 + safeAreaInsets.bottom)
        
        let minDistance = min(distanceToLeftEdge, distanceToRightEdge, distanceToTopEdge, distanceToBottomEdge)
        
        var newCenter = currentCenter
        
        if minDistance == distanceToLeftEdge {
            newCenter.x = floatWindowWidth / 2 + safeAreaInsets.left + margin
        } else if minDistance == distanceToRightEdge {
            newCenter.x = screenWidth - floatWindowWidth / 2 - safeAreaInsets.right - margin
        } else if minDistance == distanceToTopEdge {
            newCenter.y = floatWindowHeight / 2 + safeAreaInsets.top + margin
        } else if minDistance == distanceToBottomEdge {
            newCenter.y = screenHeight - floatWindowHeight / 2 - safeAreaInsets.bottom - margin
        }
        
        UIView.animate(withDuration: 0.3) {
            self.center = newCenter
        }
    }
    
    func setMargin(_ margin : CGFloat) {
        self.margin = margin
    }
    
    deinit {
        gestureOverlayView.gestureRecognizers?.forEach { gesture in
            gestureOverlayView.removeGestureRecognizer(gesture)
        }
    }
    
}
