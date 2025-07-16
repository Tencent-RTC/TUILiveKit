//
//  AudienceBattleCountDownBackgroundView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/9/2.
//

import UIKit

class AudienceBattleCountDownBackgroundView: UIView {
  
    var countDownTimeInSeconds: Int = 10
    private lazy var animationDuration: Double = 1.5
    private let initRadius: CGFloat = 80.scale375()
    private let finalRadius: CGFloat = 121.scale375()
    
    private lazy var circleView: UIView = {
        let view = UIView()
        view.layer.cornerRadius = layer.cornerRadius
        view.backgroundColor = backgroundColor
        view.alpha = alpha
        return view
    }()
    
    private lazy var shortArcLayer: CAShapeLayer = createArcLayer(startAngle: .pi / 3.0 * 2, endAngle: .pi)
    private lazy var longArcLayer: CAShapeLayer = createArcLayer(startAngle: -.pi / 3.0 * 2, endAngle: .pi / 3.0)
    
    private lazy var innerWaveLayer: CAShapeLayer = createWaveLayer()
    private lazy var outerWaveLayer: CAShapeLayer = createWaveLayer()
    
    private var isViewReady = false
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(circleView)
        circleView.layer.addSublayer(shortArcLayer)
        circleView.layer.addSublayer(longArcLayer)
        circleView.layer.addSublayer(innerWaveLayer)
        circleView.layer.addSublayer(outerWaveLayer)
    }
    
    private func activateConstraints() {
        circleView.snp.makeConstraints { make in
            make.center.equalToSuperview()
        }
    }
    
    private func createArcLayer(startAngle: CGFloat, endAngle: CGFloat) -> CAShapeLayer {
        let shapeLayer = CAShapeLayer()
        let arcPath = UIBezierPath(arcCenter: CGPoint.zero, radius: layer.cornerRadius, startAngle: startAngle, endAngle: endAngle, clockwise: true)
        shapeLayer.path = arcPath.cgPath
        shapeLayer.strokeColor = UIColor.white.cgColor
        shapeLayer.fillColor = UIColor.clear.cgColor
        shapeLayer.lineWidth = 3
        shapeLayer.position = CGPoint(x: circleView.bounds.midX, y: circleView.bounds.midY)
        return shapeLayer
    }
    
    private func createWaveLayer() -> CAShapeLayer {
        let shapeLayer = CAShapeLayer()
        shapeLayer.strokeColor = UIColor.white.cgColor
        shapeLayer.fillColor = UIColor.clear.cgColor
        shapeLayer.lineWidth = 1
        shapeLayer.backgroundColor = UIColor.purple.cgColor
        shapeLayer.frame = bounds
        shapeLayer.cornerRadius = layer.cornerRadius
        return shapeLayer
    }
    
    private func startArcAnimations() {
        let rotationAnimation = CABasicAnimation(keyPath: "transform.rotation")
        rotationAnimation.fromValue = 0
        rotationAnimation.toValue = 2 * Double.pi
        rotationAnimation.duration = animationDuration
        rotationAnimation.setValue("arcRotationAnimation", forKey: "animationKey")
        rotationAnimation.delegate = self
        
        shortArcLayer.add(rotationAnimation, forKey: "rotationAnimation")
        longArcLayer.add(rotationAnimation, forKey: "rotationAnimation")
    }
    
    private func stopArcAnimations() {
        shortArcLayer.removeAllAnimations()
        longArcLayer.removeAllAnimations()
    }
    
    private func startWaveAnimations() {
        startWaveAnimation(for: innerWaveLayer, initialRadius: initRadius, finalRadius: (initRadius + finalRadius) / 2)
        startWaveAnimation(for: outerWaveLayer, initialRadius: (initRadius + finalRadius) / 2, finalRadius: finalRadius)
    }
    
    private func startWaveAnimation(for layer: CAShapeLayer, initialRadius: CGFloat, finalRadius: CGFloat) {
        let scaleAnimation = CABasicAnimation(keyPath: "path")
        scaleAnimation.fromValue = UIBezierPath(arcCenter: CGPoint(x: circleView.bounds.midX, 
                                                                   y: circleView.bounds.midY),
                                                radius: initialRadius,
                                                startAngle: 0,
                                                endAngle: CGFloat.pi * 2,
                                                clockwise: true).cgPath
        scaleAnimation.toValue = UIBezierPath(arcCenter: CGPoint(x: circleView.bounds.midX,
                                                                 y: circleView.bounds.midY),
                                              radius: finalRadius,
                                              startAngle: 0,
                                              endAngle: CGFloat.pi * 2,
                                              clockwise: true).cgPath
        scaleAnimation.duration = animationDuration
        
        let opacityAnimation = CABasicAnimation(keyPath: "opacity")
        opacityAnimation.fromValue = 1
        opacityAnimation.toValue = 0
        opacityAnimation.duration = CFTimeInterval(countDownTimeInSeconds) / 2.0
        
        let groupAnimation = CAAnimationGroup()
        groupAnimation.animations = [scaleAnimation, opacityAnimation]
        groupAnimation.duration = animationDuration
        groupAnimation.delegate = self
        groupAnimation.setValue(layer == innerWaveLayer ? "innerWaveGroupAnimation" : "outerWaveGroupAnimation", forKey: "animationKey")
        
        layer.add(groupAnimation, forKey: "waveGroupAnimation")
    }
    
    private func stopWaveAnimations() {
        innerWaveLayer.removeAllAnimations()
        outerWaveLayer.removeAllAnimations()
    }
    
    func startAnimations() {
        startArcAnimations()
        startWaveAnimations()
    }
    
    func stopAnimations() {
        stopArcAnimations()
        stopWaveAnimations()
    }
}

extension AudienceBattleCountDownBackgroundView: CAAnimationDelegate {
    func animationDidStop(_ anim: CAAnimation, finished flag: Bool) {
        if flag, let animationKey = anim.value(forKey: "animationKey") as? String {
            switch animationKey {
                case "arcRotationAnimation":
                    startArcAnimations()
                case "innerWaveGroupAnimation":
                    startWaveAnimation(for: innerWaveLayer, initialRadius: initRadius, finalRadius: (initRadius + finalRadius) / 2)
                case "outerWaveGroupAnimation":
                    startWaveAnimation(for: outerWaveLayer, initialRadius: (initRadius + finalRadius) / 2, finalRadius: finalRadius)
                default:
                    break
            }
        }
    }
}
