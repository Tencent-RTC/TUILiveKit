//
//  ActionCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/7.
//

import UIKit

class ActionItemDesignConfig {
    var titleColor: UIColor
    var lineWidth: CGFloat
    var backgroundColor: UIColor? = .clear
    var titleFont: UIFont = .customFont(ofSize: 16)
    var lineColor: UIColor? = .g3.withAlphaComponent(0.3)
    init(lineWidth: CGFloat = 1.0, titleColor: UIColor = .flowKitWhite) {
        self.lineWidth = lineWidth
        self.titleColor = titleColor
    }
}

class ActionItem {
    init(text: String = "", designConfig: ActionItemDesignConfig = ActionItemDesignConfig(),
         action: Any) {
        self.text = text
        self.designConfig = designConfig
        actionType = action
    }

    var text: String?
    var designConfig: ActionItemDesignConfig
    var actionType: Any = LiveKitClickEvent.default
}

class ActionCell: UITableViewCell {
    var item: ActionItem? {
        didSet {
            self.updateView()
        }
    }

    override var safeAreaInsets: UIEdgeInsets {
        return UIEdgeInsets(top: 0, left: 0, bottom: 0, right: 0)
    }

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        selectionStyle = .none
        backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private lazy var actionLabel: UILabel = {
        let view = UILabel()
        view.textAlignment = .center
        contentView.addSubview(view)
        return view
    }()

    private lazy var lineView: UIView = {
        let view = UIView()
        contentView.addSubview(view)
        return view
    }()

    var actionType: LiveKitClickEvent = .default
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        setView()
        isViewReady = true
    }

    private func setView() {
        actionLabel.snp.makeConstraints { make in
            make.centerX.centerY.width.equalToSuperview()
            make.height.equalTo(48.scale375Width())
        }
    }

    private func updateView() {
        guard let item = item else { return }
        actionLabel.text = item.text
        actionLabel.textColor = item.designConfig.titleColor
        actionLabel.font = item.designConfig.titleFont
        lineView.backgroundColor = item.designConfig.lineColor
        backgroundColor = item.designConfig.backgroundColor
        lineView.snp.remakeConstraints { make in
            make.centerX.bottom.width.equalToSuperview()
            make.height.equalTo(item.designConfig.lineWidth.scale375Height())
        }
    }
}
