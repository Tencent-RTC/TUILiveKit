//
//  TUILyricsCell.swift
//  TUIKaraoke
//
//  Created by adams on 2023/4/21.
//

import UIKit

enum LyricsCellPlayingStatus {
    case prepare
    case playing
}

class TUILyricsCell: UITableViewCell {

    lazy var lyricsLabel: TUILyricsLineView = {
        let label = TUILyricsLineView(frame: .zero)
        label.transform = CGAffineTransform(scaleX: 0.8, y: 0.8)
        return label
    }()
    
    private var currentPlayingStatus: LyricsCellPlayingStatus = .prepare
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        backgroundColor = .clear
        contentView.backgroundColor = .clear
        setupView()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setupView() {
        contentView.addSubview(lyricsLabel)
        lyricsLabel.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.centerX.equalToSuperview()
            make.bottom.equalToSuperview()
        }
    }
    
}

extension TUILyricsCell {
    
    func setCellCurrentLyricsLineInfo(lyricsLineInfo: TUILyricsLineInfo) {
        lyricsLabel.lineInfo = lyricsLineInfo
    }
    
    func updateCurrentPlayingStatus(status: LyricsCellPlayingStatus, animate:Bool) {
        switch status {
        case .prepare:
            UIView.animate(withDuration: animate ? 0.35 : 0.0) {
                self.lyricsLabel.transform = CGAffineTransform(scaleX: 0.8, y: 0.8)
            }
            lyricsLabel.alpha = 0.7
        case .playing:
            UIView.animate(withDuration: animate ? 0.15 : 0.0) {
                self.lyricsLabel.transform = CGAffineTransform(scaleX: 1.1, y: 1.1)
            }
            lyricsLabel.alpha = 1.0
        }
    }
    
    func updateLyricsProgress(progress: Double) {
        lyricsLabel.updateProgress(time: progress)
    }
}
