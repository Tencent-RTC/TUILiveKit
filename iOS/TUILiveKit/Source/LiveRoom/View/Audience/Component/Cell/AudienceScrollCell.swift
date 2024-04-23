//
//  AudienceScrollCell.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/12.
//

import Foundation

class AudienceScrollCell: UITableViewCell {
    var audienceView: AudienceView? {
        willSet {
            audienceView?.removeFromSuperview()
        }
        didSet {
            guard let audienceView = audienceView else { return }
            contentView.addSubview(audienceView)
            audienceView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        }
    }

    var state: AudienceViewState = .default {
        didSet {
            if audienceView?.state.value != state {
                audienceView?.state.value = state
            }
        }
    }

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        selectionStyle = .none
        backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}
