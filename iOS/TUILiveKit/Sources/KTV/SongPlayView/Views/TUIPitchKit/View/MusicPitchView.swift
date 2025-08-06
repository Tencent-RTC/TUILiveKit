//
//  MusicPitchView.swift
//  TUIKaraoke
//
//  Created by adams on 2023/6/5.
//

import UIKit

class MusicPitchView: UIView {
    private lazy var canvas: MusicPitchCanvas = {
        let canvas = MusicPitchCanvas()
        canvas.delegate = self
        return canvas
    }()
    
    private lazy var bgImageView: UIImageView = {
        let imageView = UIImageView(image: internalImage("score_bg"))
        return imageView
    }()
    
    private lazy var scoreLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.backgroundColor = UIColor.white.withAlphaComponent(0.2)
        label.textColor = .white
        label.font = UIFont(name: "PingFangSC-Regular", size: 12)
        label.alpha = 0
        return label
    }()
    
    private var pitchIndicatorLayer: CAShapeLayer?
    
    private var config: MusicPitchViewConfig?
    
    private var stdPitchModels: [MusicPitchModel] = []
    private var hitPitchModels: [MusicPitchModel] = []
    private var currentProgress: Int = 0
    private var currentPitch: Int = 0
    private var currentSingPitch: Int = 0
    private var hitInPeriod: Bool = false
    private var musicName: String = ""
    private var isCreatingScore: Bool = false
    
    private var lastMusicProgress: Int = 0
    private var timingTrigger: DispatchSourceTimer?
    override init(frame: CGRect) {
        super.init(frame: frame)
        setup()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func layoutSubviews() {
        let canvasMarginY: CGFloat = 0
        canvas.frame = CGRectMake(0, canvasMarginY, bounds.width, bounds.height - 2 * canvasMarginY)
        bgImageView.frame = CGRectMake(0, canvasMarginY, bounds.width, bounds.height - 2 * canvasMarginY)
        scoreLabel.frame = CGRectMake(0, canvasMarginY + 5, 68, 20)
       // scoreLabel.roundedRect([.topRight,.bottomRight], withCornerRatio: 8)
    }
    
    func setConfig(config: MusicPitchViewConfig) {
        self.config = config
        canvas.setConfig(config: config)
    }
    
    func setStandardPitchModels(standardPitchModels: [MusicPitchModel]) {
        stdPitchModels = standardPitchModels
    }
    
    func getPitchStartTime() -> Int {
        if let firstPitchModel = stdPitchModels.first {
            return Int(firstPitchModel.startTime)
        }
        return 0
    }
    
    func setCurrentSongProgress(progress: Int) {
        let musicProgressInterval = progress - lastMusicProgress
        lastMusicProgress = progress
        addTimingTrigger(timeInterval: musicProgressInterval)
    }
    
    private func addTimingTrigger(timeInterval: Int) {
        let repeatMax = timeInterval / 20
        var repeatCurrent = 0
        if let gcdTimingTrigger = timingTrigger {
            gcdTimingTrigger.cancel()
        }
        let gcdTimingTrigger = DispatchSource.makeTimerSource(queue: .main)
        gcdTimingTrigger.schedule(deadline: .now(), repeating: .milliseconds(20))
        gcdTimingTrigger.setEventHandler { [weak self] in
            guard let self = self else { return }
            if repeatCurrent < repeatMax {
                let progress = self.lastMusicProgress + repeatCurrent * 20
                self.setAccompanimentClipCurrentSongProgress(progress: progress)
            } else {
                gcdTimingTrigger.cancel()
            }
            repeatCurrent += 1
        }
        gcdTimingTrigger.setCancelHandler { [weak self] in
            guard let self = self else { return }
            self.timingTrigger = nil
        }
        gcdTimingTrigger.resume()
        timingTrigger = gcdTimingTrigger
    }
    
    func setCurrentPitch(pitch: Int) {
        currentPitch = pitch
    }
    
    private func setAccompanimentClipCurrentSongProgress(progress: Int) {
        currentProgress = progress
        
        let startTime = beginTimeOnViewWithProgress(progress: progress)
        let endTime = endTimeOnViewWithProgress(progress: progress)
        
        let stdPitchModelsToDraw = findPitchModel(pitchModels: stdPitchModels, startTime: startTime, endTime: endTime)
        
        updateCurrentSingPitchAndHitPitchModelsIfNeededWithProgress(progress: progress,
                                                                    pitch: currentPitch,
                                                                    stdPitchModelsOnView: stdPitchModelsToDraw)
        let hitPitchModelsToDraw = findPitchModel(pitchModels: hitPitchModels, startTime: startTime, endTime: progress)
        canvas.drawWithProgress(progress: progress,
                                stdPitchModels: stdPitchModelsToDraw,
                                hitPitchModels: hitPitchModelsToDraw,
                                pitch: currentSingPitch)
    }
    
    func setScore(score: Int) {
        if isProgressBeforeFirstPitchModel() { return }
        scoreLabel.alpha = 1
        scoreLabel.text = " 单句 xxx分".replacingOccurrences(of: "xxx", with: "\(score)")
    }
    
    func show(songName: String, score: Int32) {
        let scoreAlert = MusicScoreDialog(frame: .zero)
        scoreAlert.show(songName: songName, score: score)
    }
    
    func clear() {
        stdPitchModels = []
        hitPitchModels = []
        currentProgress = 0
        currentPitch = 0
        currentSingPitch = 0
        hitInPeriod = false
        scoreLabel.alpha = 0
        canvas.clear()
        musicName = ""
        lastMusicProgress = 0
        timingTrigger?.cancel()
    }
    
}

extension MusicPitchView {
    private func setup() {
        addSubview(bgImageView)
        addSubview(canvas)
        addSubview(scoreLabel)
    }
    
    private func findPitchModel(pitchModels: [MusicPitchModel], startTime: Int, endTime: Int) -> [MusicPitchModel] {
        if pitchModels.count == 0 { return [] }
        if startTime >= endTime { return [] }
        var ret: [MusicPitchModel] = []
        for model in pitchModels {
            
            if model.startTime >= endTime {
                continue
            }
            if model.startTime + model.duration <= startTime {
                continue
            }
            ret.append(model)
        }
        return ret
    }
    
    private func updateCurrentSingPitchAndHitPitchModelsIfNeededWithProgress(progress:Int, pitch: Int, stdPitchModelsOnView: [MusicPitchModel]) {
        guard let config = config else { return }
        var hit = false
        for stdPitch in stdPitchModelsOnView {
            let contain = !((progress > (stdPitch.startTime + stdPitch.duration + Int(config.estimatedCallInterval)))
                            || progress < stdPitch.startTime)
            if contain {
                if pitch == -1 {
                    currentSingPitch = 0
                } else if abs(pitch - stdPitch.pitch) <= 3 {
                    hit = true
                    currentSingPitch = stdPitch.pitch
                    updateHitPitchModelsAtProgress(progress: progress, pitch: currentSingPitch, matchedStdPitchModel: stdPitch)
                } else {
                    currentSingPitch = pitch
                }
                continue
            }
        }
        hitInPeriod = hit
    }
    
    private func updateHitPitchModelsAtProgress(progress: Int, pitch: Int, matchedStdPitchModel:MusicPitchModel) {
        guard let config = config else { return }
        let stdStart = matchedStdPitchModel.startTime
        let stdDuration = matchedStdPitchModel.duration
        let stdEnd = stdStart + stdDuration
        
        let postTolerance = 33
        if let prePitchModel = hitPitchModels.last {
            if prePitchModel.pitch != pitch || prePitchModel.startTime + prePitchModel.duration + Int(config.estimatedCallInterval) < progress {
                if stdEnd - progress < postTolerance { return }
                let startTime = max(progress - Int(config.estimatedCallInterval), stdStart)
                let endTime = min(stdEnd, progress)
                let model = MusicPitchModel(startTime: startTime,
                                            duration: endTime - startTime,
                                            pitch: pitch)
                hitPitchModels.append(model)
            } else {
                let endTime = min(stdEnd, progress)
                prePitchModel.duration = endTime - prePitchModel.startTime
            }
        } else {
            if stdEnd - progress < postTolerance { return }
            let startTime = max(progress - Int(config.estimatedCallInterval), stdStart)
            let endTime = min(stdEnd, progress)
            let model = MusicPitchModel(startTime: startTime,
                                        duration: endTime - startTime,
                                        pitch: pitch)
            hitPitchModels.append(model)
        }
    }
    
    private func beginTimeOnViewWithProgress(progress: Int) -> Int {
        guard let config = config else { return 0 }
        return progress - config.timeElapsedOnScreen
    }
    
    private func endTimeOnViewWithProgress(progress: Int) -> Int {
        guard let config = config else { return 0 }
        return progress + config.timeToPlayOnScreen
    }
    
    private func isProgressBeforeFirstPitchModel() -> Bool {
        let startTime = getPitchStartTime()
        return currentProgress < startTime
    }
}

extension MusicPitchView: MusicPitchCanvasDelegate {
    func updatePitchIndicatorPosition(endPoint: CGPoint) {
        let originY = canvas.frame.maxY
        let translatedY = endPoint.y + canvas.frame.minY
        let translatedEndPoint = CGPoint(x: endPoint.x, y: translatedY)
        
        if pitchIndicatorLayer == nil {
            let shapeLayer = CAShapeLayer()
            let arcPath = UIBezierPath()
            arcPath.addArc(withCenter: translatedEndPoint, radius: 4, startAngle: 0.0, endAngle: Double.pi * 2.0, clockwise: true)
            arcPath.lineWidth = 3
            shapeLayer.fillColor = config?.pitchIndicatorColor.cgColor
            shapeLayer.path = arcPath.cgPath
            layer.addSublayer(shapeLayer)
            
            pitchIndicatorLayer = shapeLayer
        } else {
            CATransaction.begin()
            CATransaction.setAnimationDuration(0.35)
            let ty = translatedY - originY
            let translation = CGAffineTransformMakeTranslation(0, ty)
            pitchIndicatorLayer?.setAffineTransform(translation)
            CATransaction.commit()
        }
    }
}
