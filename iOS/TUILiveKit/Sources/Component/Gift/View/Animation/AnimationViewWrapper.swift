//
//  AnimationViewWrapper.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/7/22.
//

import UIKit

class AnimationViewWrapper: UIView {
    
    private enum AnimationSourceType {
        case other
        case mp4
        case svg
    }
    
    private var animationView: AnimationView?
    
    init() {
        super.init(frame: .zero)
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        animationView = createAnimationView()
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        if let animationView = animationView {
            addSubview(animationView)
        }
    }
    
    private func activateConstraints() {
        if let animationView = animationView {
            animationView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func createAnimationView() -> AnimationView {
        let animationView = TCEffectAnimationView()
        if animationView.usable {
            return animationView
        } else {
            return SVGAAnimationView()
        }
    }
    
    private lazy var svgAnimationView: SVGAAnimationView = {
        SVGAAnimationView()
    }()
    private lazy var effectAnimationView: TCEffectAnimationView = {
        TCEffectAnimationView()
    }()
}

extension AnimationViewWrapper: AnimationView {
    func playAnimation(playUrl: String, onFinished: @escaping ((Int)->Void)) {
        LiveKitLog.info("\(#file)","\(#line)","playAnimation:[playUrl:\(playUrl)]")
        let type = getSourceType(playUrl: playUrl)
        if type == .other {
            onFinished(-1)
            return
        }
        switchPlaySourceIfNeeded(type: type)
        animationView?.playAnimation(playUrl: playUrl, onFinished: onFinished)
    }
}

extension AnimationViewWrapper {
    private func switchPlaySourceIfNeeded(type: AnimationSourceType) {
        guard let shouldUseView = getPlayAnimationView(type: type) else {
            animationView?.removeFromSuperview()
            animationView = nil
            return
        }
        if let animationView = animationView {
            if animationView == shouldUseView {
                return
            }
            animationView.removeFromSuperview()
            self.animationView = nil
        }
        self.animationView = shouldUseView
        addSubview(shouldUseView)
        shouldUseView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        return
    }
    
    private func getPlayAnimationView(type: AnimationSourceType) -> AnimationView? {
        if effectAnimationView.usable && type == .mp4 {
            return effectAnimationView
        } else if type == .svg {
            return svgAnimationView
        } else {
            return nil
        }
    }
    
    private func getSourceType(playUrl: String) -> AnimationSourceType {
        if playUrl.isEmpty {
            return .other
        }
        if playUrl.lowercased().hasSuffix(".mp4") {
            return .mp4
        }
        if playUrl.lowercased().hasSuffix(".svga") {
            return .svg
        }
        return .other
    }
}
