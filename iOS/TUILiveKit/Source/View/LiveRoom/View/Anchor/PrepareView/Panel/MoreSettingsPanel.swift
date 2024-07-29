//
//  MoreSettingsPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/10.
//

import Foundation
import RTCCommon

class MoreSettingsModel {
    var title: String
    var enable: Bool
    init(title: String, enable: Bool) {
        self.title = title
        self.enable = enable
    }
}

class MoreSettingsCell: UITableViewCell {
    var item: MoreSettingsModel? {
        didSet {
            guard let item = item else { return }
            switchButton.isOn = item.enable
            titleLabel.text = item.title
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

    private lazy var switchButton: UISwitch = {
        let view = UISwitch()
        view.onTintColor = .b1
        view.addTarget(self, action: #selector(switchButtonClick), for: .touchUpInside)
        return view
    }()

    lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 16)
        label.textColor = .g5
        return label
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        setView()
        isViewReady = true
    }

    func setView() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(switchButton)
        switchButton.snp.remakeConstraints { make in
            make.trailing.equalToSuperview().inset(24)
            make.centerY.equalToSuperview()
            make.height.equalTo(20.scale375())
            make.width.equalTo(32.scale375())
        }

        titleLabel.snp.makeConstraints { make in
            make.top.bottom.width.equalToSuperview()
            make.leading.equalToSuperview().inset(24)
        }
    }

    @objc func switchButtonClick() {
        item?.enable = switchButton.isOn
    }
}

class MoreSettingsPanel: UIView {
    private var isPortrait: Bool = {
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

    private var viewHeight: CGFloat = 718.scale375Height()
    private var list: [MoreSettingsModel]
    init(list: [MoreSettingsModel], _ viewHeight: CGFloat = 718.scale375Height()) {
        self.list = list
        self.viewHeight = viewHeight
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .setTitleText
        label.sizeToFit()
        return label
    }()

    private lazy var actionTableView: UITableView = {
        let tableView = UITableView()
        tableView.backgroundColor = .g2
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(MoreSettingsCell.self, forCellReuseIdentifier: MoreSettingsCell.cellReuseIdentifier)
        return tableView
    }()

    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 20
        layer.masksToBounds = true
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(actionTableView)
    }

    func activateConstraints() {
        snp.remakeConstraints { [weak self] make in
            guard let self = self else { return }
            if isPortrait {
                make.width.equalToSuperview()
                make.height.equalTo(viewHeight)
                make.centerX.equalToSuperview()
            } else {
                make.width.equalTo(375)
                make.height.equalToSuperview()
                make.trailing.equalToSuperview()
            }
            make.bottom.equalToSuperview()
        }

        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }

        titleLabel.snp.remakeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(titleLabel.mm_w)
        }

        actionTableView.snp.remakeConstraints { make in
            make.top.equalTo(backButton.snp.bottom).offset(18)
            make.leading.trailing.equalToSuperview()
            make.bottom.equalToSuperview().offset(-WindowUtils.bottomSafeHeight)
        }
    }

    private func getCellHeight(row: Int) -> CGFloat {
        if row >= list.count {
            return 0
        }
        return 48.scale375Height()
    }
}

// MARK: Action

extension MoreSettingsPanel {
    @objc func backButtonClick() {
        // TODO: Abyyxwang Router
    }
}

extension MoreSettingsPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return getCellHeight(row: indexPath.row)
    }
}

extension MoreSettingsPanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return list.count
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: MoreSettingsCell.cellReuseIdentifier, for: indexPath)
        if let cell = cell as? MoreSettingsCell, indexPath.row < list.count {
            cell.item = list[indexPath.row]
            cell.isUserInteractionEnabled = true
        }
        return cell
    }
}

private extension String {
    static var setTitleText = {
        localized("live.anchor.more.set.title")
    }()
}
