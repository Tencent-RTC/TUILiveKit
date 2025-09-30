//
//  KaraokePitchCanvas.swift
//  TUIKaraoke
//
//  Created by adams on 2023/6/5.
//

import UIKit
import Foundation
import SnapKit

protocol KaraokePitchCanvasDelegate: NSObject {
    func updatePitchIndicatorPosition(endPoint: CGPoint)
}

class KaraokePitchCanvas: UIView {
    weak var delegate: KaraokePitchCanvasDelegate?
    private var config: KaraokePitchViewConfig?
    private var backgroundImageView: UIImageView?
    private var stdPitchModelsToDraw: [KaraokePitchModel] = []
    private var hitPitchModelsToDraw: [KaraokePitchModel] = []
    private var pitch: Int = 10
    private var progress: Int = 0
    
    private var msWidth: CGFloat = 0
    private var pitchHeight: CGFloat = 0
    private var vlineOffsetX: CGFloat = 0
    
    override func draw(_ rect: CGRect) {
        drawStandardPitchModels()
        drawHitPitchModels()
        drawVerticalLine()
        drawPitchIndicator()
    }
    
    override func layoutSubviews() {
        super.layoutSubviews()
        setupUIConfig()
    }
    
    // MARK: - Public
    func setConfig(config: KaraokePitchViewConfig) {
        self.config = config
        setupUIConfig()
    }
    
    func drawWithProgress(progress: Int, stdPitchModels: [KaraokePitchModel], hitPitchModels: [KaraokePitchModel], pitch: Int) {
        self.progress = progress
        self.stdPitchModelsToDraw = stdPitchModels
        self.hitPitchModelsToDraw = hitPitchModels
        self.pitch = pitch
        setNeedsDisplay()
    }
    
    func clear() {
        self.stdPitchModelsToDraw = []
        self.hitPitchModelsToDraw = []
        self.progress = 0
        self.pitch = 0
        setNeedsDisplay()
    }
    
    func setBackgroundImage(_ image: UIImage?) {
        if let image = image {
            if backgroundImageView == nil {
                let imageView = UIImageView()
                imageView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
                addSubview(imageView)
                backgroundImageView?.snp.makeConstraints { make in
                    make.edges.equalToSuperview()
                }
                backgroundImageView = imageView
            }
            backgroundImageView?.image = image
        } else {
            backgroundImageView?.removeFromSuperview()
            backgroundImageView = nil
        }
        setNeedsDisplay()
    }
    
    func clearBackgroundImage() {
        backgroundImageView?.removeFromSuperview()
        backgroundImageView = nil
        setNeedsDisplay()
    }
    
    // MARK: - Private
    private func setupUIConfig() {
        guard let config = self.config else { return }
        let height = bounds.height
        let width = bounds.width
        backgroundColor = .clear
        msWidth = width / CGFloat(config.timeElapsedOnScreen + config.timeToPlayOnScreen)
        pitchHeight = height / CGFloat(config.pitchNum)
    }
    
    private func getPitchRectCenterYWithPitch(pitch: Int) -> CGFloat {
        guard let config = self.config else { return 0.0 }
        let height = bounds.height
        var pitchTemp = pitch
        if pitchTemp < config.minPitch {
            return height
        }
        if pitchTemp > config.maxPitch {
            return 0
        }
        if pitchTemp == config.minPitch {
            pitchTemp += 1
        }
        return CGFloat((config.maxPitch - pitchTemp) * config.pitchNum) /
                CGFloat((config.maxPitch - config.minPitch)) * pitchHeight + pitchHeight * 0.5
    }
    
    // MARK: - Draw
    private func drawVerticalLine() {
        guard let config = self.config else { return }
        let height = bounds.height
        let width = bounds.width
        let lineWidth = 0.5
        
        let vlineOffsetX = width * CGFloat(config.timeElapsedOnScreen) / CGFloat(config.timeElapsedOnScreen + config.timeToPlayOnScreen)
        self.vlineOffsetX = vlineOffsetX
        let vlineRect = CGRectMake(vlineOffsetX, 0, lineWidth, height)
        let vlinePath = UIBezierPath(rect: vlineRect)
        let vlineColor = config.verticalLineColor
        vlineColor.setFill()
        vlinePath.fill()
    }
    
    private func drawStandardPitchModels() {
        guard let config = self.config else { return }
        drawPitchModels(pitchModels: stdPitchModelsToDraw, fillColor: config.standardRectColor)
    }
    
    private func drawHitPitchModels() {
        guard let config = self.config else { return }
        drawPitchModels(pitchModels: hitPitchModelsToDraw, fillColor: config.hitRectColor)
    }
    
    private func drawPitchModels(pitchModels: [KaraokePitchModel], fillColor: UIColor) {
        guard let config = config, pitchModels.count > 0 else { return }
        let msWidth = self.msWidth
        let pitchHeight = self.pitchHeight
        
        let context = UIGraphicsGetCurrentContext()
        context?.setShouldAntialias(true)
        context?.setAllowsAntialiasing(true)
        
        let sortedModels = pitchModels.sorted { $0.startTime < $1.startTime }
        
        var mergedModels: [KaraokePitchModel] = []
        var currentModel: KaraokePitchModel? = nil
        
        for model in sortedModels {
            if let last = currentModel {
                if last.pitch == model.pitch && 
                   (model.startTime - (last.startTime + last.duration)) <= 20 { 
                    last.duration = (model.startTime + model.duration) - last.startTime
                    continue
                }
            }
            let newModel = KaraokePitchModel(startTime: model.startTime,
                                           duration: model.duration,
                                           pitch: model.pitch)
            mergedModels.append(newModel)
            currentModel = newModel
        }
        
        for model in mergedModels {
            let x = msWidth * CGFloat(model.startTime - (progress - config.timeElapsedOnScreen)) - 2
            let y = getPitchRectCenterYWithPitch(pitch: model.pitch) - pitchHeight * 0.5
            let w = msWidth * CGFloat(model.duration) + 2
            let h = pitchHeight
            
            let pitchRect = CGRectMake(x, y, w, h)
            let linePath = UIBezierPath(roundedRect: pitchRect, cornerRadius: pitchHeight * 0.5)
            let lineColor = fillColor
            lineColor.setFill()
            linePath.fill()
            
            context?.saveGState()
            let gradient = CGGradient(colorsSpace: nil,
                                     colors: [fillColor.withAlphaComponent(0.8).cgColor,
                                              fillColor.cgColor,
                                              fillColor.withAlphaComponent(0.8).cgColor] as CFArray,
                                     locations: [0.0, 0.5, 1.0])
            context?.addPath(linePath.cgPath)
            context?.clip()
            context?.drawLinearGradient(gradient!,
                                      start: CGPoint(x: x, y: y),
                                      end: CGPoint(x: x + w, y: y),
                                      options: [])
            context?.restoreGState()
        }
    }
    
    private func drawPitchIndicator() {
        let y = getPitchRectCenterYWithPitch(pitch: pitch)
        let endPoint = CGPoint(x: vlineOffsetX, y: y)
        if let delegate = delegate {
            delegate.updatePitchIndicatorPosition(endPoint: endPoint)
        }
    }
}
