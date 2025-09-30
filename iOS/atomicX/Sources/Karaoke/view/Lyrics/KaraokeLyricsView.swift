//
//  KaraokeLyricsView.swift
//  Pods
//
//  Created by ssc on 2025/8/8.
//

import UIKit
import SnapKit

struct KaraokeLyricLine {
    let startTime: Int
    let endTime: Int
    let text: String
    let words: [KaraokeLyricWord]
    var totalDuration: Int = 0
}

struct KaraokeLyricWord {
    let text: String
    let startTime: Int
    let duration: Int
}

struct LyricRowStyle {
    var labelHeight: CGFloat
    var top: CGFloat
    var bottom: CGFloat
}

class KaraokeLyricsView: UIView {

    private var currentPlaybackProgress: Int = 0
    private var currentmusicId: String?
    private var lyricsData: [KaraokeLyricLine] = []
    private var lyricsCache: [String: [KaraokeLyricLine]] = [:]
    private var currentLineIndex: Int = -1
    private var lastDisplayedLine: KaraokeLyricLine?
    private var isKTV: Bool

    private lazy var tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.delegate = self
        tableView.dataSource = self
        tableView.separatorStyle = .none
        tableView.backgroundColor = .clear
        tableView.showsVerticalScrollIndicator = false
        tableView.isScrollEnabled = false
        tableView.register(LyricCell.self, forCellReuseIdentifier: "LyricCell")
        tableView.rowHeight = 22
        tableView.estimatedRowHeight = 22
        tableView.sectionHeaderHeight = 0
        tableView.sectionFooterHeight = 0
        return tableView
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setupViewStyle()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        addSubview(tableView)
    }

    private func activateConstraints() {
        tableView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
            make.height.equalTo(isKTV ? 55 : 40)
        }
    }

    init(isKTV: Bool){
        self.isKTV = isKTV
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setupViewStyle() {
        backgroundColor = .clear 
    }

    func updateLyrics(progress: Int) {
        currentPlaybackProgress = progress
        guard let currentIndex = findCurrentLineIndex(progress: progress) else { return }

        if currentIndex == currentLineIndex {
            if let cell = tableView.cellForRow(at: IndexPath(row: 0, section: 0)) as? LyricCell {
                cell.updateProgress(progress)
            }
        } else {
            updateDisplay(with: progress)
        }
    }

    func loadLyrics(fileURL: URL) {
        lyricsData.removeAll()
        currentLineIndex = -1
        lyricsData = parseLyricsFile(fileURL: fileURL)
        updateLyrics(progress: 0)
    }

    private func parseLyricsFile(fileURL: URL) -> [KaraokeLyricLine] {
        guard let lyricsInfo = KaraokeLyricParser.parserLocalLyricFile(fileURL: fileURL) else {
            return []
        }

        var result: [KaraokeLyricLine] = []

        for lineInfo in lyricsInfo.KaraokeLyricLineInfos {
            let words = lineInfo.charStrArray.map { charInfo in
                KaraokeLyricWord(text: charInfo.characterStr,
                                 startTime: charInfo.startTime,
                                 duration: charInfo.duration)
            }

            let fullText = words.map { $0.text }.joined()
            let totalDuration = words.reduce(0) { $0 + $1.duration }

            if fullText.characterCount > 30 {
                var splitIndex = words.count / 2
                var bestSplitIndex = splitIndex
                var minDistance = Int.max
                for i in max(0, splitIndex-5)...min(words.count-1, splitIndex+5) {
                    let char = words[i].text
                    if char.isEmpty || char == " " || char == "," || char == "，" || char == "." || char == "。" {
                        let distance = abs(i - splitIndex)
                        if distance < minDistance {
                            minDistance = distance
                            bestSplitIndex = i
                        }
                    }
                }

                splitIndex = bestSplitIndex

                let firstHalf = Array(words[0..<splitIndex])
                let secondHalf = Array(words[splitIndex..<words.count])

                let firstHalfDuration = firstHalf.reduce(0) { $0 + $1.duration }
                let secondHalfDuration = secondHalf.reduce(0) { $0 + $1.duration }

                result.append(KaraokeLyricLine(
                    startTime: Int(lineInfo.startTime * 1000),
                    endTime: Int(lineInfo.startTime * 1000) + firstHalfDuration,
                    text: firstHalf.map { $0.text }.joined(),
                    words: firstHalf,
                    totalDuration: firstHalfDuration
                ))

                result.append(KaraokeLyricLine(
                    startTime: Int(lineInfo.startTime * 1000) + firstHalfDuration,
                    endTime: Int(lineInfo.endTime * 1000),
                    text: secondHalf.map { $0.text }.joined(),
                    words: secondHalf,
                    totalDuration: secondHalfDuration
                ))
            } else {
                result.append(KaraokeLyricLine(
                    startTime: Int(lineInfo.startTime * 1000),
                    endTime: Int(lineInfo.endTime * 1000),
                    text: fullText,
                    words: words,
                    totalDuration: totalDuration
                ))
            }
        }

        return result
    }

    private func updateDisplay(with progress: Int) {
        let oldRowCount = tableView.numberOfRows(inSection: 0)

        guard let currentIndex = findCurrentLineIndex(progress: progress) else {
            return
        }

        let needsScroll = currentIndex != currentLineIndex
        currentLineIndex = currentIndex

        let remaining = lyricsData.count - currentLineIndex
        if remaining <= 0 {
            return
        }

        let newRowCount = min(2, max(1, remaining))

        if oldRowCount != newRowCount {
            tableView.reloadData()
        } else {
            var indexPaths: [IndexPath] = []
            if newRowCount > 0 {
                indexPaths.append(IndexPath(row: 0, section: 0))
            }
            if newRowCount > 1 {
                indexPaths.append(IndexPath(row: 1, section: 0))
            }
            if !indexPaths.isEmpty {
                tableView.reloadRows(at: indexPaths, with: .none)
            }
        }

        if needsScroll {
            if tableView.numberOfRows(inSection: 0) > 0 {
                tableView.scrollToRow(at: IndexPath(row: 0, section: 0), at: .middle, animated: true)
            }
        }
    }

    private func findCurrentLineIndex(progress: Int) -> Int? {
        guard !lyricsData.isEmpty else { return nil }

        if let index = lyricsData.lastIndex(where: { progress >= $0.startTime && progress <= $0.endTime }) {
            return index
        }

        for i in 0..<lyricsData.count - 1 {
            if progress > lyricsData[i].endTime && progress < lyricsData[i+1].startTime {
                return i
            }
        }

        if progress < lyricsData.first?.startTime ?? 0 {
            return 0
        }

        return currentLineIndex
    }

    private func findNextLine(after line: KaraokeLyricLine) -> KaraokeLyricLine? {
        guard let index = lyricsData.firstIndex(where: { $0.startTime == line.startTime }) else {
            return nil
        }
        let nextIndex = index + 1
        return nextIndex < lyricsData.count ? lyricsData[nextIndex] : nil
    }

    private func lyricRowStyle(for isKTV: Bool, row: Int) -> LyricRowStyle {
        if isKTV {
            if row == 0 {
                return LyricRowStyle(labelHeight: 21, top: 3, bottom: 3)
            } else {
                return LyricRowStyle(labelHeight: 17, top: 5, bottom: 5)
            }
        } else {
            if row == 0 {
                return LyricRowStyle(labelHeight: 18, top: 4, bottom: 0)
            } else {
                return LyricRowStyle(labelHeight: 14, top: 4, bottom: 4)
            }
        }
    }

    deinit {
        lyricsCache.removeAll()
        tableView.delegate = nil
        tableView.dataSource = nil
    }

}

// MARK: - UITableViewDelegate & DataSource
extension KaraokeLyricsView: UITableViewDelegate, UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        guard !lyricsData.isEmpty else { return 0 }
        return min(2, max(1, lyricsData.count - currentLineIndex))
    }

    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        let style = lyricRowStyle(for: isKTV, row: indexPath.row)
        return style.labelHeight + style.top + style.bottom
    }


    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "LyricCell", for: indexPath) as! LyricCell
        let style = lyricRowStyle(for: isKTV, row: indexPath.row)
        cell.setRowStyle(style)
        let lineIndex = currentLineIndex + indexPath.row
        guard lineIndex < lyricsData.count else { return cell }
        let line = lyricsData[lineIndex == -1 ? 0 : lineIndex]
        let isCurrentLine = indexPath.row == 0
        cell.configure(with: line, isCurrentLine: isCurrentLine, progress: currentPlaybackProgress, alignment: isKTV ? .center : .right)
        return cell
    }
}

private class LyricCell: UITableViewCell {
    private var topConstraint: Constraint?
    private var bottomConstraint: Constraint?
    private var heightConstraint: Constraint?
    private let karaokeLyricLabel: KaraokeLyricLabel = {
        let label = KaraokeLyricLabel()
        return label
    }()
    private var line: KaraokeLyricLine?

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        setupViews()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }



    private func setupViews() {
        backgroundColor = .clear
        selectionStyle = .none
        karaokeLyricLabel.textAlignment = .right
        karaokeLyricLabel.numberOfLines = 1
        contentView.addSubview(karaokeLyricLabel)
        karaokeLyricLabel.snp.makeConstraints { make in
            make.left.right.equalToSuperview()
            topConstraint = make.top.equalToSuperview().constraint
            bottomConstraint = make.bottom.equalToSuperview().constraint
            heightConstraint = make.height.equalTo(22).constraint
        }
    }

    func setRowStyle(_ style: LyricRowStyle) {
        heightConstraint?.update(offset: style.labelHeight)
        topConstraint?.update(offset: style.top)
        bottomConstraint?.update(offset: -style.bottom)
    }

    func configure(with line: KaraokeLyricLine, isCurrentLine: Bool, progress: Int, alignment: NSTextAlignment) {
        self.line = line
        karaokeLyricLabel.textAlignment = alignment
        if alignment == .center {
            karaokeLyricLabel.font = isCurrentLine ?
            UIFont(name: "PingFangSC-Semibold", size: 18) ?? .systemFont(ofSize: 18, weight: .semibold) :
            UIFont(name: "PingFangSC-Regular", size: 12) ?? .systemFont(ofSize: 10, weight: .regular)
        } else {
            karaokeLyricLabel.font = isCurrentLine ?
            UIFont(name: "PingFangSC-Medium", size: 14) ?? .systemFont(ofSize: 14, weight: .medium) :
            UIFont(name: "PingFangSC-Regular", size: 10) ?? .systemFont(ofSize: 10, weight: .regular)
        }
        karaokeLyricLabel.text = line.text

        if isCurrentLine {
            updateProgress(progress)
        } else {
            karaokeLyricLabel.textColor = .lightGray
            karaokeLyricLabel.progress = 0
            karaokeLyricLabel.setNeedsDisplay()
        }
    }

    func updateProgress(_ progress: Int) {
        guard let line = self.line else { return }
        var elapsed: CGFloat = 0
        var totalDuration: CGFloat = 0

        for word in line.words {
            let wordStart = word.startTime + line.startTime
            if progress >= wordStart {
                elapsed += min(CGFloat(progress - wordStart), CGFloat(word.duration))
            }
            totalDuration += CGFloat(word.duration)
        }
        let progressRatio = totalDuration > 0 ? min(elapsed / totalDuration, 1.0) : 0
        karaokeLyricLabel.progress = progressRatio
        karaokeLyricLabel.textColor = .white
        karaokeLyricLabel.setNeedsDisplay()
    }
}

fileprivate extension String {
    var characterCount: Int {
        var count = 0
        for char in self {
            count += char.unicodeScalars.first?.value ?? 0 > 255 ? 2 : 1
        }
        return count
    }
}

