//
//  SGSeatSoundWaveView.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/28.
//

import UIKit

class SGSeatSoundWaveView: UIView {
    
    private var isPlaying: Bool = false
    private var needStopPlay: Bool = false
    
    private lazy var replicatorLayer: CAReplicatorLayer = {
        let replicatorLayer = CAReplicatorLayer()
        replicatorLayer.instanceCount = 3
        replicatorLayer.instanceDelay = 0.5
        return replicatorLayer
    }()
    
    private lazy var waveLayer: CAShapeLayer = {
        let shapeLayer = CAShapeLayer()
        shapeLayer.backgroundColor = backgroundColor?.cgColor
        shapeLayer.frame = bounds
        shapeLayer.cornerRadius = layer.cornerRadius
        return shapeLayer
    }()
    
    func startWave() {
        if isPlaying { return }
        isPlaying = true
        needStopPlay = false
        let animation1 = CABasicAnimation(keyPath: "transform")
        animation1.toValue = NSValue(caTransform3D: CATransform3DMakeScale(1.4, 1.4, 1))
        animation1.duration = 2
        animation1.timingFunction = CAMediaTimingFunction(name: CAMediaTimingFunctionName.easeInEaseOut)
        
        let animation2 = CABasicAnimation(keyPath: "opacity")
        animation2.fromValue = 1
        animation2.toValue = 0
        animation2.duration = 2
        
        let group = CAAnimationGroup()
        group.animations = [animation1, animation2]
        group.duration = 3
        group.repeatCount = 1
        group.delegate = self
        group.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
        waveLayer.add(group, forKey: "replicator.wave.animation")
        
        replicatorLayer.addSublayer(waveLayer)
        layer.addSublayer(replicatorLayer)
    }
    
    func stopWave() {
        needStopPlay = true
    }
}

extension SGSeatSoundWaveView: CAAnimationDelegate {
    func animationDidStop(_ anim: CAAnimation, finished flag: Bool) {
        if flag {
            isPlaying = false
            if needStopPlay {
                waveLayer.removeAllAnimations()
                replicatorLayer.removeFromSuperlayer()
                waveLayer.removeFromSuperlayer()
            } else {
                startWave()
            }
        }
    }
}
