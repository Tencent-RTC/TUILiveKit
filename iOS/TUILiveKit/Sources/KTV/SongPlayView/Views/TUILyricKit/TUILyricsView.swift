//
//  TUILyricsView.swift
//
//  Created by adams on 2021/7/16.
//  Copyright © 2022 Tencent. All rights reserved.

import UIKit

// MARK: - debug 后续删除
protocol TUILyricsViewDelegate: AnyObject {
    func lyricsView(_ view: TUILyricsView, didUpdate info: TUILyricsInfo?)
}

let cellHeight: CGFloat = 30.0
class TUILyricsView: UIView {
    weak var delegate: TUILyricsViewDelegate?

    var lyricsInfo: TUILyricsInfo? {
        didSet {
            lyricsTableView.reloadData()
            DispatchQueue.main.async {
                self.delegate?.lyricsView(self, didUpdate: self.lyricsInfo)
            }
        }
    }
    
    var lyricsPathString: String? {
        didSet {
            if let lyricsPathString = lyricsPathString {
                if lyricsPathString == "" {
                    lyricsInfo = nil
                    headerView.text = "暂无歌词"
                } else {
                    lyricsInfo = TUILyricParser.parserLocalLyricFile(fileURL: URL(fileURLWithPath: lyricsPathString))
                    //headerView.text = "即将播放..."
                }
            } else {
                lyricsInfo = nil
            }
        }
    }
    
    var currentIndex: NSInteger {
        get {
            guard let lyricsInfo = lyricsInfo else { return 0 }
            for (i, model) in lyricsInfo.lyricLineInfos.enumerated() {
                if model.startTime > currentTime {
                    if i > 0 {
                        return i - 1
                    } else {
                        return i
                    }
                }
            }
            return lyricsInfo.lyricLineInfos.count - 1
        }
    }
    
    var currentTime: TimeInterval = 0 {
        didSet {
            guard lyricsInfo != nil, lyricsPathString != nil else { return }
            setTime(currentTime)
        }
    }
    
    var currentIndexPath: IndexPath = IndexPath(row: -1, section: 0)
    
    var lastCell: TUILyricsCell?
    
    private func setTime(_ time: TimeInterval) {
        guard let lyricsInfo = lyricsInfo else { return }
        if currentIndex != currentIndexPath.row {
            debugPrint("currentIndex = \(currentIndex), lyricLineInfoCount = \(lyricsInfo.lyricLineInfos.count)")
            lyricsTableView.scrollToRow(at: IndexPath(row: currentIndex, section: 0), at: .top, animated: true)
            if let finishCell = lastCell {
                finishCell.updateCurrentPlayingStatus(status: .prepare, animate: true)
            }
        }
        
        let indexPath = IndexPath(row: currentIndex, section: 0)
        
        if indexPath.row != currentIndexPath.row {
            currentIndexPath = indexPath
        }
        
        guard let lyricsCell = lyricsTableView.cellForRow(at:indexPath) as? TUILyricsCell else { return }
        lyricsCell.updateCurrentPlayingStatus(status: .playing, animate: true)
        let currentLineInfo = lyricsInfo.lyricLineInfos[indexPath.row]
        let progress = time - currentLineInfo.startTime
        lyricsCell.updateLyricsProgress(progress: progress)
        lastCell = lyricsCell
    }
    
    lazy var lyricsTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(TUILyricsCell.self, forCellReuseIdentifier: "TUILyricsCell")
        tableView.backgroundColor = .clear
        tableView.showsVerticalScrollIndicator = false
        tableView.isUserInteractionEnabled = false
        tableView.separatorStyle = .none
        return tableView
    }()
    
    lazy var headerView: UILabel = {
        let headerView = UILabel(frame: .zero)
       // headerView.text = "即将播放..."
        headerView.textAlignment = .center
        headerView.textColor = .white
        headerView.font = UIFont(name: "PingFangSC-Semibold", size: 18)
        headerView.transform = CGAffineTransform(scaleX: 1.1, y: 1.1)
        return headerView
    }()
    
    init() {
        super.init(frame: .zero)
        setupView()
        backgroundColor = .clear
    }
    
    deinit {
        debugPrint("\(self) deinit")
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func resetLyricsViewStatus() {
        lastCell = nil
        currentIndexPath = IndexPath(row: -1, section: 0)
        lyricsTableView.reloadData()
        if let lyricsInfo = lyricsInfo, lyricsInfo.lyricLineInfos.count > 0 {
            lyricsTableView.scrollToRow(at: IndexPath(row: 0, section: 0), at: .bottom, animated: false)
        }
    }
}

extension TUILyricsView {
    
    private func setupView() {
        addSubview(lyricsTableView)
        lyricsTableView.snp.makeConstraints { make in
            make.left.right.equalToSuperview()
            make.top.bottom.equalToSuperview()
        }
        
        lyricsTableView.tableHeaderView = headerView
        headerView.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview()
            make.height.equalTo(cellHeight)
        }
        
        let footerView = UILabel(frame: .zero)
        lyricsTableView.tableFooterView = footerView
        
        footerView.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview()
            make.height.equalTo(cellHeight)
        }
    }
}

extension TUILyricsView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return lyricsInfo?.lyricLineInfos.count ?? 0
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "TUILyricsCell", for: indexPath)
        if let lyricsCell = cell as? TUILyricsCell {
            if let info = lyricsInfo {
                let lineInfo = info.lyricLineInfos[indexPath.row]
                lyricsCell.setCellCurrentLyricsLineInfo(lyricsLineInfo: lineInfo)
                lyricsCell.updateCurrentPlayingStatus(status:.prepare, animate: false)
            }
        }
        return cell
    }
}

extension TUILyricsView: UITableViewDelegate {
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return cellHeight
    }
}

class TUILyricsLineView: UIView {
    
    let normalTextColor: UIColor
    let selectedTextColor: UIColor
    
    var lineInfo: TUILyricsLineInfo? {
        didSet {
            updateView()
        }
    }
    
    init(frame: CGRect, normalTextColor: UIColor = .white, selectedTextColor: UIColor = .orange) {
        self.normalTextColor = normalTextColor
        self.selectedTextColor = selectedTextColor
        super.init(frame: frame)
        setupView()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func setupView() {
        guard let lineInfo = lineInfo else { return }
        var lastLabel: TUILyricsLabel?
        for (index,model) in lineInfo.charStrArray.enumerated() {
            let characterLabel = TUILyricsLabel(frame: .zero)
            characterLabel.characterInfo = model
            characterLabel.textAlignment = .left
            characterLabel.normalTextColor = normalTextColor
            characterLabel.selectedTextColor = selectedTextColor
            characterLabel.font = UIFont(name: "PingFangSC-Semibold", size: 18)
            addSubview(characterLabel)
            if index == 0 {
                characterLabel.snp.makeConstraints { make in
                    make.left.equalToSuperview()
                    make.top.bottom.equalToSuperview()
                    if lineInfo.charStrArray.count == 1 {
                        make.right.equalToSuperview()
                    }
                }
            } else {
                characterLabel.snp.makeConstraints { make in
                    if let lastLabel = lastLabel {
                        make.left.equalTo(lastLabel.snp.right)
                        make.top.bottom.equalTo(lastLabel)
                    }
                    if index == lineInfo.charStrArray.count - 1 {
                        make.right.equalToSuperview()
                    }
                }
            }
            lastLabel = characterLabel
        }
    }
    
    func updateView() {
        for subView in subviews {
            guard let lyricLabel = subView as? TUILyricsLabel else { continue }
            lyricLabel.removeFromSuperview()
        }
        setupView()
    }
    
    func updateProgress(time: Double) {
        guard let lastLabel = subviews.last as? TUILyricsLabel else { return }
        if Int(time) > lastLabel.characterInfo.endTime && time < 0 {
            return
        }
        for subView in subviews  {
            guard let lyricLabel = subView as? TUILyricsLabel else { continue }
            let mill = time * 1000
            if (mill <= Double(lyricLabel.characterInfo.endTime)) {
                let current = mill - Double(lyricLabel.characterInfo.startTime)
                if current >= 0 {
                    let progress = current / Double(lyricLabel.characterInfo.duration)
                    lyricLabel.progress = progress
                    return
                }
            } else {
                lyricLabel.progress = 1
            }
        }
    }
}

public class TUILyricsLabel: UIView {
    
    public var font: UIFont? = UIFont(name: "PingFangSC-Semibold", size: 18) {
        didSet {
            textLabel.font = font
            maskLabel.font = font
        }
    }
    
    public var textAlignment: NSTextAlignment = .left {
        didSet {
            textLabel.textAlignment = textAlignment
            maskLabel.textAlignment = textAlignment
        }
    }
    
    public var textColor: UIColor? = .white {
        didSet {
            textLabel.textColor = textColor
        }
    }
    
    public var characterInfo: TUILyricsCharacterInfo = TUILyricsCharacterInfo(startTime: 0,
                                                                              duration: 0,
                                                                              characterStr: "") {
        didSet {
            if oldValue.startTime != characterInfo.startTime {
                CATransaction.begin()
                CATransaction.setDisableActions(true)
                maskLayer.bounds = CGRect(x: 0, y: 0, width: 0, height: bounds.height)
                CATransaction.commit()
            }
            textLabel.text = characterInfo.characterStr
            maskLabel.text = characterInfo.characterStr
            textLabel.sizeToFit()
            maskLabel.sizeToFit()
        }
    }
    
    public var progress: Double = 0 {
        didSet {
            if progress > 0 && progress <= 1 {
                setNeedsDisplay()
            }
        }
    }
    
    public func reset() {
        maskLayer.bounds = CGRect(x: 0, y: 0, width: 0, height: bounds.height)
        progress = 0
    }
    
    var normalTextColor: UIColor? {
        didSet {
            textLabel.textColor = normalTextColor
        }
    }
    
    var selectedTextColor: UIColor = .cyan {
        didSet {
            maskLabel.textColor = selectedTextColor
        }
    }
    
    lazy var textLabel: UILabel = {
        let label = UILabel(frame: bounds)
        label.font = font
        label.text = characterInfo.characterStr
        label.textAlignment = textAlignment
        label.textColor = textColor
        return label
    }()
    
    lazy var maskLabel: UILabel = {
        let label = UILabel(frame: bounds)
        label.font = font
        label.text = characterInfo.characterStr
        label.textAlignment = textAlignment
        label.textColor = textColor
        label.layer.mask = maskLayer
        backgroundColor = .clear
        return label
    }()
    
    lazy var maskLayer: CALayer = {
        let maskLayer = CALayer()
        maskLayer.anchorPoint = CGPoint(x: 0, y: 0.5)
        maskLayer.backgroundColor = UIColor.white.cgColor
        return maskLayer
    }()
    
    public override func draw(_ rect: CGRect) {
        super.draw(rect)
        maskLayer.position = CGPoint(x: 0, y: bounds.height * 0.5)
        
        if progress == 0 {
            maskLayer.bounds = CGRect(x: 0, y: 0, width: 0, height: bounds.height)
        }
        else {
            maskLayer.bounds = CGRect(x: 0, y: 0, width: maskLabel.bounds.width * CGFloat(progress), height: bounds.height)
        }
    }
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        if isViewReady {
            return
        }
        isViewReady = true
        
        addSubview(textLabel)
        textLabel.snp.makeConstraints { (make) in
            make.top.bottom.leading.trailing.equalToSuperview()
        }
        
        addSubview(maskLabel)
        maskLabel.snp.makeConstraints { (make) in
            make.top.bottom.leading.trailing.equalToSuperview()
        }
    }
}
