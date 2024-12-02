//
//  ActionPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/2.
//

import UIKit
import RTCCommon

struct ActionPanelData: Equatable {
    
    var title: String = ""
    
    var items: [ActionItem]  = []
    
    var containCancel: Bool = true
    
    init(title: String = "", items: [ActionItem], containCancel: Bool = true) {
        self.title = title
        self.items = items
        self.containCancel = containCancel
    }
    
    static func ==(lhs: ActionPanelData, rhs: ActionPanelData) -> Bool {
        return lhs.items == rhs.items
    }
}

class ActionPanel: UIView {
    var cancelActionClosure: (() -> Void)?
    private let panelData: ActionPanelData
    private var items: [ActionItem] {
        return panelData.items
    }
    private var isPortrait:Bool = {
        WindowUtils.isPortrait
    }()
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    private var actionTitleViewHeight: CGFloat {
        return panelData.title != "" ? 50.scale375Width() : 0
    }
    
    private var cancelButtonViewHeight: CGFloat {
        return panelData.containCancel ? 48.scale375() : 0
    }
        
    
    lazy var viewHeight : CGFloat = {
        var viewHeight = cancelButtonViewHeight
        for row in 0 ... items.count {
            viewHeight = viewHeight + getCellHeight(row: row)
        }
        if panelData.title != "" {
            viewHeight += actionTitleViewHeight
        }
        return viewHeight
    }()
    
    init(panelData: ActionPanelData) {
        self.panelData = panelData
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var actionTableView: UITableView = {
        let tableView = UITableView()
        tableView.backgroundColor = .g2
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(ActionCell.self, forCellReuseIdentifier: ActionCell.cellReuseIdentifier)
        tableView.isScrollEnabled = false
        tableView.contentInsetAdjustmentBehavior = .never
        tableView.separatorStyle = .none
        return tableView
    }()
    
    private lazy var actionHeaderView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .white
        return view
    }()
    
    private lazy var actionHeaderLineView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .g8
        return view
    }()

    private lazy var titleLabel: UILabel = {
        let titleLabel = UILabel(frame: .zero)
        titleLabel.textColor = .g4
        titleLabel.font = UIFont.customFont(ofSize: 12)
        titleLabel.numberOfLines = 0
        titleLabel.textAlignment = .center
        titleLabel.text = panelData.title
        return titleLabel
    }()
    
    private lazy var cancelButton: UIButton = {
        let button = UIButton()
        button.setTitle(.cancelText, for: .normal)
        button.setTitleColor(.g2, for: .normal)
        button.titleLabel?.textAlignment = .center
        button.titleLabel?.font = .customFont(ofSize: 16)
        button.backgroundColor = .white
        button.addTarget(self, action: #selector(cancelButtonClick), for: .touchUpInside)
        return button
    }()
    
    func constructViewHierarchy() {
        backgroundColor = .g8
        layer.cornerRadius = 20
        layer.masksToBounds = true
        addSubview(actionHeaderView)
        actionHeaderView.addSubview(titleLabel)
        actionHeaderView.addSubview(actionHeaderLineView)
        addSubview(actionTableView)
        addSubview(cancelButton)
        cancelButton.isHidden = !panelData.containCancel
        actionHeaderView.isHidden = panelData.title == ""
    }
    
    func activateConstraints() {
        let bottomSafeHeight = WindowUtils.bottomSafeHeight
        snp.remakeConstraints { [weak self] make in
            guard let self = self else { return }
            if self.isPortrait {
                make.height.equalTo(min(718.scale375Height(), self.viewHeight+bottomSafeHeight))
            } else {
                make.width.equalTo(375.scale375())
            }
            make.edges.equalToSuperview()
        }
        
        actionHeaderView.snp.remakeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
            make.height.equalTo(actionTitleViewHeight)
        }
        
        titleLabel.snp.remakeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(12)
            make.centerY.equalToSuperview()
        }
        
        actionHeaderLineView.snp.remakeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.bottom.equalToSuperview()
            make.height.equalTo(1)
        }
        
        actionTableView.snp.remakeConstraints { [weak self] make in
            guard let self = self else { return }
            make.top.equalTo(actionHeaderView.snp.bottom)
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(self.viewHeight - cancelButtonViewHeight - actionTitleViewHeight)
        }
        
        cancelButton.snp.remakeConstraints { make in
            make.top.equalTo(actionTableView.snp.bottom)
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(cancelButtonViewHeight)
        }
    }
    
    private func adjustTableViewBounces() {
        if viewHeight <= screenWidth - (48.scale375() + WindowUtils.bottomSafeHeight) {
            actionTableView.bounces = false
        } else {
            actionTableView.bounces = true
        }
    }
    
    private func getCellHeight(row:Int) -> CGFloat {
        if row >= items.count {
            return 0
        }
        return 62.scale375Height() + items[row].designConfig.lineWidth
    }
}

// MARK: Action

extension ActionPanel {
    @objc func cancelButtonClick() {
        cancelActionClosure?()
    }
}

extension ActionPanel: UITableViewDelegate{
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return getCellHeight(row: indexPath.row)
    }
}

extension ActionPanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return items.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        
        let cell = tableView.dequeueReusableCell(withIdentifier: ActionCell.cellReuseIdentifier, for: indexPath)
        if let cell = cell as? ActionCell, indexPath.row < items.count {
            cell.item = items[indexPath.row]
        }
        return cell
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let item = items[indexPath.row]
        item.actionClosure?(indexPath.row)
    }
}

private extension String {
    static var cancelText = {
        localized("live.audience.link.confirm.cancel")
    }()
}
