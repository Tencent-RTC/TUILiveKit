//
//  MusicInfoPlayCell.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/20.
//

import Foundation
import TUICore

class MusicInfoCell: UITableViewCell {
    let action: Observable<MusicPlayAction> = Observable(.default)
    var musicInfo: MusicInfo? {
        didSet {
            guard let musicInfo = musicInfo else {
                return
            }
            nameLabel.text = musicInfo.name
            updateView()
        }
    }

    let nameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 16)
        if TUIGlobalization.getRTLOption() {
            label.textAlignment = .right
        } else {
            label.textAlignment = .left
        }
        label.textColor = .g7
        return label
    }()

    let lineView: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        return view
    }()

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        selectionStyle = .none
        backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func updateView() {
        musicInfo?.isPlaying.removeObserver(self)
        musicInfo?.isPlaying.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }
    }
}
