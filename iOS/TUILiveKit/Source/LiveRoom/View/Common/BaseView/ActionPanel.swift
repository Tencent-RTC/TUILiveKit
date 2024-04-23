//
//  ActionPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/2.
//

import UIKit


class ActionModel {
    var items: [ActionItem] = []
}

class ActionPanel: UIView {
    let clickEventCallBack: Observable<Any> = Observable(LiveKitClickEvent.default)
    private var model: ActionModel
    private var popupAction: Observable<PopupPanelAction>?
    private var isPortrait:Bool = {
        WindowUtils.isPortrait
    }()
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    lazy var viewHeight : CGFloat = {
        var viewHeight = 48.scale375Width()
        for row in 0 ... model.items.count {
            viewHeight = viewHeight + getCellHeight(row: row)
        }
        return viewHeight
    }()
    
    init(model: ActionModel) {
        self.model = model
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
        return tableView
    }()
    
    private lazy var cancelButton: UIButton = {
        let button = UIButton()
        button.setTitle(.cancelText, for: .normal)
        button.setTitleColor(.g7, for: .normal)
        button.titleLabel?.textAlignment = .center
        button.titleLabel?.font = .customFont(ofSize: 16)
        button.backgroundColor = .g2
        button.addTarget(self, action: #selector(cancelButtonClick), for: .touchUpInside)
        return button
    }()
    
    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 20
        layer.masksToBounds = true
        addSubview(actionTableView)
        addSubview(cancelButton)
    }

    func activateConstraints() {
        let bottomSafeHeight = WindowUtils.bottomSafeHeight
        snp.remakeConstraints { [weak self] make in
            guard let self = self else { return }
            if self.isPortrait {
                make.width.equalToSuperview()
                make.height.equalTo(min(718.scale375Height(), self.viewHeight+bottomSafeHeight))
                make.centerX.equalToSuperview()
            } else {
                make.width.equalTo(375)
                make.height.equalToSuperview()
                make.trailing.equalToSuperview()
            }
            make.bottom.equalToSuperview()
        }
        
        actionTableView.snp.remakeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
            make.bottom.equalTo(cancelButton.snp.top)
        }
        
        cancelButton.snp.remakeConstraints { make in
            make.bottom.equalToSuperview().offset(-bottomSafeHeight)
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(48.scale375Width())
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
        if row >= model.items.count {
            return 0
        }
        return 62.scale375Height() + model.items[row].designConfig.lineWidth
    }
}

// MARK: Action

extension ActionPanel {
    
    @objc func cancelButtonClick() {
        popupAction?.value = .close
    }
}

extension ActionPanel: PopupPanelSubViewProtocol {
    func setAction(_ popupAction: Observable<PopupPanelAction>) {
        self.popupAction = popupAction
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
        adjustTableViewBounces()
    }
}

extension ActionPanel: UITableViewDelegate{
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return getCellHeight(row: indexPath.row)
    }
}

extension ActionPanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return model.items.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        
        let cell = tableView.dequeueReusableCell(withIdentifier: ActionCell.cellReuseIdentifier, for: indexPath)
        if let cell = cell as? ActionCell, indexPath.row < model.items.count {
            cell.item = model.items[indexPath.row]
        }
        return cell
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let item = model.items[indexPath.row]
        self.popupAction?.addObserver(self, closure: { [weak self] action, _ in
            if action == .dismiss {
                self?.clickEventCallBack.value = item.actionType
            }
        })
        self.popupAction?.value = .close
    }
    
}

private extension String {
    static var cancelText = {
        localized("live.audience.link.confirm.cancel")
    }()
}
