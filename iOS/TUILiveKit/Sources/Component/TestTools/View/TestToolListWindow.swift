//
//  TestToolListWindow.swift
//  TUILiveKit
//
//  Created by gg on 2025/6/19.
//

import UIKit
import SnapKit
import RTCCommon

#if DEV_MODE
class TestToolListWindow: UIWindow {
    private lazy var tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.dataSource = self
        tableView.delegate = self
        tableView.register(TestToolListCell.self, forCellReuseIdentifier: TestToolListCell.cellId)
        tableView.separatorStyle = .none
        return tableView
    }()
    
    private var dataSource: [TestCaseModel] = []
    
    func updateShowCases(_ cases: [TestCaseModel]) {
        dataSource = cases
        tableView.reloadData()
    }
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        initUI()
    }
    
    override init(windowScene: UIWindowScene) {
        super.init(windowScene: windowScene)
        initUI()
    }
    
    private func initUI() {
        let w: CGFloat = 200
        let h: CGFloat = 300
        frame = CGRect(x: screenWidth - 50 - w, y: ScreenHeight * 0.5 - h * 0.5, width: w, height: h)
        
        windowLevel = .statusBar - 1
        
        t_makeKeyAndVisible()
        isHidden = true
        
        addSubview(tableView)
        tableView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

extension TestToolListWindow: UITableViewDataSource, UITableViewDelegate {
    func numberOfSections(in tableView: UITableView) -> Int {
        dataSource.count
    }
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return dataSource[section].list.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: TestToolListCell.cellId, for: indexPath)
        if let cell = cell as? TestToolListCell {
            let model = dataSource[indexPath.section].list[indexPath.row]
            cell.setModel(model)
        }
        return cell
    }
}
#endif
