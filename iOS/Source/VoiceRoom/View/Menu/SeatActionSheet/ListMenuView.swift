//
//  SeatOpreateMenu.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/19.
//

import UIKit
import SnapKit
import Combine

class ListMenuView: UIView {
    private var isViewReady: Bool = false
    private var cancellables = Set<AnyCancellable>()
    
    private var height: CGFloat {
        return CGFloat(menus.count * 55)
    }
    
    let menus: [ListMenuInfo]
    
    init(menus: [ListMenuInfo]) {
        self.menus = menus
        super.init(frame: .zero)
        backgroundColor = .clear
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private let tableView: UITableView = {
        let view = UITableView(frame: .zero)
        view.register(ListMenuCell.self, forCellReuseIdentifier: ListMenuCell.identifier)
        view.separatorStyle = .none
        view.backgroundColor = .clear
        return view
    }()
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activeViewConstraint()
        bindInteraction()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(tableView)
    }
    
    private func activeViewConstraint() {
        tableView.snp.remakeConstraints { make in
            make.top.bottom.left.right.equalToSuperview()
            make.height.equalTo(height > 0 ? height : 55)
        }
    }
    private func bindInteraction() {
        tableView.delegate = self
        tableView.dataSource = self
    }
}

extension ListMenuView: UITableViewDelegate {
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let menu = menus[indexPath.row]
        menu.tapAction?(indexPath.row)
    }
}

extension ListMenuView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: ListMenuCell.identifier, for: indexPath)
        if let menuCell = cell as? ListMenuCell {
            let menu = menus[indexPath.row]
            menuCell.title = menu.title
            menuCell.icon = menu.icon
        }
        return cell
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return menus.count
    }
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 55
    }
}


