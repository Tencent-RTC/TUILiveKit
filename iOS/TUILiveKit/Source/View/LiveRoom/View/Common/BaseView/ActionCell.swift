//
//  ActionCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/7.
//

import UIKit
import Combine

typealias ActionTapClosure = (Int) -> Void

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
    let id: UUID = UUID()
    var icon: String = ""
    var title: String?
    
    var designConfig: ActionItemDesignConfig
    var actionClosure: ActionTapClosure?
    
    init(title: String = "", designConfig: ActionItemDesignConfig = ActionItemDesignConfig(),
         actionClosure: ActionTapClosure? = nil) {
        self.title = title
        self.designConfig = designConfig
        self.actionClosure = actionClosure
    }
    
    init(title: String, icon: String) {
        self.title = title
        self.icon = icon
        self.designConfig = ActionItemDesignConfig()
    }
}

extension ActionItem: Equatable {
    static func == (lhs: ActionItem, rhs: ActionItem) -> Bool {
        return lhs.id == rhs.id
    }
}

class ActionCell: UITableViewCell {
    
    static let identifier = "ActionCell"
    @Published var item: ActionItem?
    private var cancellableSet = Set<AnyCancellable>()

    override var safeAreaInsets: UIEdgeInsets {
        return UIEdgeInsets(top: 0, left: 0, bottom: 0, right: 0)
    }
    
    private let actionLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        return label
    }()
    
    private let iconImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        return imageView
    }()

    private lazy var lineView: UIView = {
        let view = UIView()
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

    override func prepareForReuse() {
        super.prepareForReuse()
        item = nil
    }

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activeViewConstraint()
        bindInteraction()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        addSubview(iconImageView)
        addSubview(actionLabel)
        addSubview(lineView)
    }
    
    private func activeViewConstraint() {
        
        iconImageView.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.left.equalToSuperview().offset(16)
            make.height.width.equalTo(20)
        }
        
        actionLabel.snp.makeConstraints { make in
            let padding = ActionCell.iconWidth + ActionCell.paddingHorizontal + 10
            make.centerY.equalToSuperview()
            make.left.equalToSuperview().offset(padding)
            make.right.equalToSuperview().offset(-padding)
        }
    }
    
    private func bindInteraction() {
        $item.receive(on: RunLoop.main)
            .sink { [weak self] value in
                guard let self = self else { return }
                if let item = value {
                    actionLabel.text = item.title
                    actionLabel.textColor = item.designConfig.titleColor
                    actionLabel.font = item.designConfig.titleFont
                    
                    lineView.backgroundColor = item.designConfig.lineColor
                    backgroundColor = item.designConfig.backgroundColor
                    self.lineView.snp.remakeConstraints { make in
                        make.centerX.bottom.width.equalToSuperview()
                        make.height.equalTo(item.designConfig.lineWidth.scale375Height())
                    }
                    
                    let iconIsEmpty = item.icon.isEmpty
                    self.iconImageView.isHidden = iconIsEmpty
                    self.actionLabel.textAlignment = iconIsEmpty ? .center : .left
                    guard !iconIsEmpty else { return }
                    self.iconImageView.image = .liveBundleImage(item.icon)
                }
            }
            .store(in: &cancellableSet)
    }
}

extension ActionCell {
    static let iconWidth = 20
    static let paddingHorizontal = 16
}
