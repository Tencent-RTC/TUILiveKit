//
//  TestToolListCell.swift
//  TUILiveKit
//
//  Created by gg on 2025/6/19.
//

import UIKit
import SnapKit

class TestToolListCell: UITableViewCell {
    static let cellId = "TestToolListCellID"
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        selectionStyle = .none
        
        contentView.addSubview(switcher)
        switcher.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-20).priority(.high)
            make.centerY.equalToSuperview()
        }
        
        contentView.addSubview(titleLabel)
        titleLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(20)
            make.trailing.equalTo(switcher.snp.leading).offset(-20)
        }
    }
    
    var model: TestCaseItemModel? = nil {
        didSet {
            if let model = model {
                titleLabel.text = model.title
            } else {
                titleLabel.text = "Error"
            }
        }
    }
    
    func setModel(_ model: TestCaseItemModel) {
        self.model = model
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .black
        label.font = .systemFont(ofSize: 11)
        label.adjustsFontSizeToFitWidth = true
        label.minimumScaleFactor = 0.5
        return label
    }()
    
    private lazy var switcher: UISwitch = {
        let switcher = UISwitch(frame: .zero)
        switcher.addTarget(self, action: #selector(onSwitchValueChanged), for: .valueChanged)
        return switcher
    }()
    
    @objc private func onSwitchValueChanged(_ switcher: UISwitch) {
        guard let model = model else { return }
        if model.view?.responds(to: model.sel) == true {
            model.view?.perform(model.sel, with: switcher.isOn)
        }
    }
}
