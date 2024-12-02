//
//  AnimationViewWrapper.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/7/22.
//

import UIKit

class AnimationViewWrapper: UIView {
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
}

extension AnimationViewWrapper: AnimationView {
    func playAnimation(playUrl: String) {
        LiveKitLog.info("\(#file)","\(#line)","playAnimation:[playUrl:\(playUrl)]")
        animationView?.playAnimation(playUrl: playUrl)
    }
    
    func setFinishClosure(onFinished: @escaping ((Int) -> Void)) {
        animationView?.setFinishClosure(onFinished: onFinished)
    }
}
