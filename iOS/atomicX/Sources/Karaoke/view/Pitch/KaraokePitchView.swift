//
//  KaraokePitchView.swift
//  TUIKaraoke
//
//  Created by ssc on 2023/6/5.
//
import UIKit
import SnapKit
import Combine
import RTCCommon

class KaraokePitchView: UIView {

    private lazy var canvas: KaraokePitchCanvas = {
        let canvas = KaraokePitchCanvas()
        canvas.delegate = self
        return canvas
    }()

    private let isKTV: Bool
    private var scoreButtonLayer: CATextLayer?
    private var butterflyEmitterLayer: CAEmitterLayer?
    private var pitchIndicatorLayer: CAShapeLayer?
    private weak var manager: KaraokeManager?
    private var config: KaraokePitchViewConfig?
    private var cancellables = Set<AnyCancellable>()
    private var stdPitchModels: [KaraokePitchModel] = []
    private var hitPitchModels: [KaraokePitchModel] = []

    private var currentProgress: Int = 0
    private var currentPitch: Int = -1
    private var currentSingPitch: Int = 0
    private var hitInPeriod: Bool = false

    private var musicName: String = ""
    private var isCreatingScore: Bool = false
    private var lastMusicProgress: Int = 0

    private var timingTrigger: DispatchSourceTimer?
    private var currentScore: String = "10.0"
    private var scoreUpdateCounter: Int = 0

    init(manager: KaraokeManager,isKTV: Bool) {
        self.isKTV = isKTV
        self.manager = manager
        scoreButtonLayer?.isHidden = true
        super.init(frame: .zero)
        setup()
    }

    init() {
        self.isKTV = false
        super.init(frame: .zero)
    }

    deinit {
        cancellables.forEach { $0.cancel() }
        cancellables.removeAll()
        stopButterflyEffect()
        timingTrigger?.cancel()
        timingTrigger = nil
        canvas.delegate = nil
        clear()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setup() {
        addSubview(canvas)

        canvas.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }

        if self.isKTV {
            canvas.setBackgroundImage(UIImage.atomicXBundleImage(named: "ktv_canvas"))
        }

        manager?.subscribe(StateSelector(keyPath: \.enableScore))
            .receive(on: DispatchQueue.main)
            .removeDuplicates()
            .sink { [weak self] enableScore in
                guard let self = self ,let manager = manager else {return}
                if manager.karaokeState.selectedSongs.count > 0{
                    self.setScoreComponentVisible(enableScore)
                } else {
                    self.setScoreComponentVisible(false)
                }
            }
            .store(in: &cancellables)
    }

    func setScore(_ score: String) {
        if scoreButtonLayer?.isHidden == true {
            return
        }
        currentScore = score
        scoreUpdateCounter += 1

        scoreButtonLayer?.string = score
    }

    func setScoreComponentVisible(_ visible: Bool) {
        scoreButtonLayer?.isHidden = !visible
    }
    
    func hidden() {
        pitchIndicatorLayer?.isHidden = true
        scoreButtonLayer?.isHidden = true
        canvas.isHidden = true
    }

    func show() {
        pitchIndicatorLayer?.isHidden = false
        canvas.isHidden = false
        guard let manager = manager else { return }
        scoreButtonLayer?.isHidden = !manager.karaokeState.enableScore
    }

    func setConfig(config: KaraokePitchViewConfig) {
        self.config = config
        canvas.setConfig(config: config)
    }

    func setStandardPitchModels(standardPitchModels: [KaraokePitchModel]) {
        stdPitchModels = standardPitchModels
        hitPitchModels = []
    }

    func setCurrentSongProgress(progress: Int) {
        let musicProgressInterval = progress - lastMusicProgress
        lastMusicProgress = progress
        addTimingTrigger(timeInterval: musicProgressInterval)
    }


    func setCurrentPitch(pitch: Int) {
        currentPitch = pitch
    }

    func clear() {
        hitPitchModels = []
        currentProgress = 0
        currentPitch = -1
        currentSingPitch = 0
        hitInPeriod = false

        canvas.clear()
        lastMusicProgress = 0
        timingTrigger?.cancel()
        timingTrigger = nil
    }

    private func getPitchStartTime() -> Int {
        return stdPitchModels.first?.startTime ?? 0
    }

    private func updateScoreButtonPosition(ballCenter: CGPoint) {
        let scoreWidth: CGFloat = 28
        let scoreHeight: CGFloat = 18
        let gap: CGFloat = 6
        let scoreX = ballCenter.x - scoreWidth / 2.0 - 4
        let scoreY = ballCenter.y - 4 - gap - scoreHeight

        ensureScoreButton()

        CATransaction.begin()
        CATransaction.setAnimationDuration(0.35)
        scoreButtonLayer?.frame = CGRect(x: scoreX, y: scoreY, width: scoreWidth, height: scoreHeight)
        CATransaction.commit()
    }

    private func ensureScoreButton() {
        if scoreButtonLayer != nil { return }

        let textLayer = CATextLayer()
        textLayer.string = currentScore
        textLayer.fontSize = 12
        textLayer.alignmentMode = .center
        textLayer.backgroundColor = UIColor.flowKitWhite.cgColor
        textLayer.cornerRadius = 5
        textLayer.frame = CGRect(x: 0, y: 0, width: 28, height: 18)
        textLayer.contentsScale = UIScreen.main.scale
        textLayer.foregroundColor = config?.scoreTextColor.cgColor
        layer.addSublayer(textLayer)
        scoreButtonLayer = textLayer
    }

    private func addTimingTrigger(timeInterval: Int) {
        let repeatMax = max(0, timeInterval / 20)
        var repeatCurrent = 0

        timingTrigger?.cancel()

        if repeatMax == 0 { return }

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


    private func beginTimeOnViewWithProgress(progress: Int) -> Int {
        guard let config = config else { return progress }
        return progress - config.timeElapsedOnScreen
    }

    private func endTimeOnViewWithProgress(progress: Int) -> Int {
        guard let config = config else { return progress }
        return progress + config.timeToPlayOnScreen
    }

    private func isProgressBeforeFirstPitchModel() -> Bool {
        let startTime = getPitchStartTime()
        return currentProgress < startTime
    }

    private func findPitchModel(pitchModels: [KaraokePitchModel], startTime: Int, endTime: Int) -> [KaraokePitchModel] {
        if pitchModels.isEmpty { return [] }
        if startTime >= endTime { return [] }

        var ret: [KaraokePitchModel] = []
        for model in pitchModels {
            if model.startTime >= endTime { break }
            if model.startTime + model.duration > startTime {
                ret.append(model)
            }
        }
        return ret
    }

    private func updateCurrentSingPitchAndHitPitchModelsIfNeededWithProgress(progress: Int,
                                                                             pitch: Int,
                                                                             stdPitchModelsOnView: [KaraokePitchModel]) {
        guard let config = config else { return }
        var hit = false
        var firstHitInPeriod = false

        for stdPitch in stdPitchModelsOnView {
            let contain = !((progress > (stdPitch.startTime + stdPitch.duration + Int(config.estimatedCallInterval)))
                            || progress < stdPitch.startTime)
            if contain {
                if pitch == -1 {
                    hit = true
                    currentSingPitch = stdPitch.pitch
                    updateHitPitchModelsAtProgress(progress: progress, pitch: currentSingPitch, matchedStdPitchModel: stdPitch)
                    if !hitInPeriod {
                        firstHitInPeriod = true
                        DispatchQueue.main.async {
                            self.setScore("10.0")
                        }
                    }
                } else if abs(pitch - stdPitch.pitch) <= 3 {
                    if !hitInPeriod {
                        firstHitInPeriod = true
                        DispatchQueue.main.async {
                            self.setScore("10.0")
                        }
                    }
                    hit = true
                    currentSingPitch = stdPitch.pitch
                    updateHitPitchModelsAtProgress(progress: progress, pitch: currentSingPitch, matchedStdPitchModel: stdPitch)
                } else {
                    currentSingPitch = pitch
                }
                continue
            }
        }

        if hit {
            guard let manager = manager else { return }
            if butterflyEmitterLayer == nil && manager.karaokeState.playbackState != .pause{
                startButterflyEffect()
            }
        } else {
            stopButterflyEffect()
        }

        hitInPeriod = hit
    }

    private func updateHitPitchModelsAtProgress(progress: Int, pitch: Int, matchedStdPitchModel: KaraokePitchModel) {
        guard let config = config else { return }

        let stdStart = matchedStdPitchModel.startTime
        let stdDuration = matchedStdPitchModel.duration
        let stdEnd = stdStart + stdDuration

        let postTolerance = 0
        if let prePitchModel = hitPitchModels.last {
            if stdEnd - progress < postTolerance { return }

            let lastEnd = prePitchModel.startTime + prePitchModel.duration
            let startTime = max(lastEnd, stdStart)
            let endTime = min(stdEnd, progress)

            let model = KaraokePitchModel(startTime: startTime,
                                          duration: endTime - startTime + 15,
                                          pitch: pitch)
            hitPitchModels.append(model)
        } else {
            if stdEnd - progress < postTolerance { return }

            let startTime = max(progress - Int(config.estimatedCallInterval), stdStart)
            let endTime = min(stdEnd, progress)
            let model = KaraokePitchModel(startTime: startTime,
                                        duration: endTime - startTime,
                                        pitch: pitch)
            hitPitchModels.append(model)
        }
    }

    func startButterflyEffect() {
        if let existing = butterflyEmitterLayer {
            existing.removeFromSuperlayer()
            butterflyEmitterLayer = nil
        }
        let emitter = CAEmitterLayer()
        emitter.name = "butterflyEmitter"
        let leftTopMargin: CGFloat = 20.0
        emitter.emitterPosition = CGPoint(x: leftTopMargin, y: leftTopMargin)
        emitter.emitterSize = CGSize(width: 10, height: 10)
        emitter.emitterMode = .points
        emitter.renderMode = .additive
        emitter.zPosition = 10

        let cell = CAEmitterCell()
        cell.contents = UIImage.atomicXBundleImage(named: "ktv_fly_note")?.cgImage
        cell.birthRate = 2.0
        cell.lifetime = 1.0
        cell.lifetimeRange = 0.4
        cell.velocity = 50.0
        cell.velocityRange = 15.0
        cell.emissionLongitude = 245.0 * CGFloat.pi / 180.0
        cell.emissionRange = 20.0 * CGFloat.pi / 180.0
        cell.xAcceleration = -20
        cell.yAcceleration = -10
        cell.scale = 0.7
        cell.scaleRange = 0.1
        cell.spin = 0.8
        cell.spinRange = 0.5
        cell.alphaSpeed = -1.0
        cell.alphaRange = 0.2

        emitter.emitterCells = [cell]
        self.layer.addSublayer(emitter)

        butterflyEmitterLayer = emitter
    }

    func stopButterflyEffect() {
        guard let emitter = butterflyEmitterLayer else { return }

        CATransaction.begin()
        CATransaction.setAnimationDuration(0.25)
        emitter.birthRate = 0
        CATransaction.commit()
        self.butterflyEmitterLayer = nil
    }
}

// MARK: - KaraokePitchCanvasDelegate
extension KaraokePitchView: KaraokePitchCanvasDelegate {
    func updatePitchIndicatorPosition (endPoint: CGPoint) {
        let translatedY = endPoint.y + canvas.frame.minY
        let ballCenter = CGPoint(x: endPoint.x + 4, y: translatedY + 2)

        if pitchIndicatorLayer == nil {
            let shapeLayer = CAShapeLayer()
            let circlePath = UIBezierPath(arcCenter: .zero,
                                          radius: 4,
                                          startAngle: 0.0,
                                          endAngle: CGFloat.pi * 2.0,
                                          clockwise: true)
            shapeLayer.path = circlePath.cgPath
            shapeLayer.bounds = CGRect(x: 0, y: 0, width: 8, height: 8)
            shapeLayer.fillColor = config?.pitchIndicatorColor.cgColor
            shapeLayer.position = ballCenter
            layer.addSublayer(shapeLayer)

            pitchIndicatorLayer = shapeLayer
            updateScoreButtonPosition(ballCenter: ballCenter)
        } else {
            CATransaction.begin()
            CATransaction.setAnimationDuration(0.35)
            pitchIndicatorLayer?.position = ballCenter
            CATransaction.commit()

            updateScoreButtonPosition(ballCenter: ballCenter)
        }

        if let emitter = butterflyEmitterLayer {
            let emitterPos = CGPoint(x: ballCenter.x - 2, y: ballCenter.y)
            CATransaction.begin()
            CATransaction.setAnimationDuration(0.25)
            emitter.emitterPosition = emitterPos
            CATransaction.commit()
        }
    }
}
