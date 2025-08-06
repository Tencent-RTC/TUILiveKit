//
//  MusicPitchCanvas.swift
//  TUIKaraoke
//
//  Created by adams on 2023/6/5.
//

import UIKit

protocol MusicPitchCanvasDelegate: NSObject {
    func updatePitchIndicatorPosition(endPoint: CGPoint)
}

class MusicPitchCanvas: UIView {
    weak var delegate: MusicPitchCanvasDelegate?
    
    private var config: MusicPitchViewConfig?
    private var stdPitchModelsToDraw: [MusicPitchModel] = []
    private var hitPitchModelsToDraw: [MusicPitchModel] = []
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
    func setConfig(config: MusicPitchViewConfig) {
        self.config = config
        setupUIConfig()
    }
    
    func drawWithProgress(progress: Int, stdPitchModels: [MusicPitchModel], hitPitchModels: [MusicPitchModel], pitch: Int) {
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
    
    private func drawPitchModels(pitchModels: [MusicPitchModel], fillColor: UIColor) {
        guard let config = config, pitchModels.count > 0 else { return }
        let msWidth = self.msWidth
        let pitchHeight = self.pitchHeight
        
        for pitchModel in pitchModels {
            let startTime = pitchModel.startTime
            let duration = pitchModel.duration
            let pitch = pitchModel.pitch
            
            let x = msWidth * CGFloat(startTime - (progress - config.timeElapsedOnScreen))
            let y = getPitchRectCenterYWithPitch(pitch: pitch) - self.pitchHeight * 0.5
            let w = msWidth * CGFloat(duration)
            let h = pitchHeight
            
            let pitchRect = CGRectMake(x, y, w, h)
            let linePath = UIBezierPath(roundedRect: pitchRect, cornerRadius: pitchHeight * 0.5)
            let lineColor = fillColor
            lineColor.setFill()
            linePath.fill()
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
