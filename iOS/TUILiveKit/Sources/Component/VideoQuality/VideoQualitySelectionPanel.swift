//
//  VideoQualitySelectionView.swift
//  AFNetworking
//
//  Created by jack on 2025/8/28.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine

class VideoQualitySelectionPanel: UIView {
    
    private var resolutions: [TUIVideoQuality] = []
    
    public var cancelClosure: (()->Void)?
    public var selectedClosure: ((TUIVideoQuality)->Void)?
    
    public init(resolutions: [TUIVideoQuality],
                selectedClosure: ((TUIVideoQuality) -> Void)? = nil) {
        self.resolutions = resolutions
        super.init(frame: .zero)
        self.selectedClosure = selectedClosure
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private let kCellRowHeight: CGFloat = 56

    private lazy var tableView: UITableView = {
        let view = UITableView(frame: .zero, style: .plain)
        view.dataSource = self
        view.delegate = self
        view.register(VideoQualityCell.self, forCellReuseIdentifier: VideoQualityCell.identifier)
        view.backgroundColor = .clear
        view.sectionFooterHeight = 0
        view.sectionHeaderHeight = 0
        view.showsVerticalScrollIndicator = false
        view.isScrollEnabled = false
        return view
    }()
    
    private lazy var lineView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .bgEntrycardColor
        return view
    }()
    
    private lazy var cancelButton: UIButton = {
        let button = UIButton(type: .custom)
        button.titleLabel?.font = .customFont(ofSize: 16)
        button.setTitle(.cancelText, for: .normal)
        return button
    }()
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupView()
        isViewReady = true
    }
    
    private func setupView() {
        backgroundColor = .bgOperateColor
        layer.cornerRadius = 16
    }
}

// MARK: - Layout
private extension VideoQualitySelectionPanel {
    
    func constructViewHierarchy() {
        addSubview(tableView)
        addSubview(lineView)
        addSubview(cancelButton)
    }
    
    func activateConstraints() {
        let tableViewHeight = CGFloat(resolutions.count) * kCellRowHeight
        tableView.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.height.equalTo(tableViewHeight)
            make.leading.trailing.equalToSuperview()
        }
        lineView.snp.makeConstraints { make in
            make.height.equalTo(7)
            make.top.equalTo(tableView.snp.bottom)
            make.leading.trailing.equalToSuperview()
        }
        cancelButton.snp.makeConstraints { make in
            make.top.equalTo(lineView.snp.bottom).offset(20)
            make.bottom.equalToSuperview().offset(-20)
            make.leading.trailing.equalToSuperview()
        }
    }
    
    func bindInteraction() {
        cancelButton.addTarget(self, action: #selector(cancelAction), for: .touchUpInside)
    }
}

// MARK: - Action
extension VideoQualitySelectionPanel {
    
    @objc private func cancelAction() {
        cancelClosure?()
    }
}

// MARK: - UITableViewDataSource
extension VideoQualitySelectionPanel: UITableViewDataSource {

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return resolutions.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: VideoQualityCell.identifier, for: indexPath) as! VideoQualityCell
        cell.contentLabel.text = .videoQualityToString(quality: resolutions[indexPath.row])
        return cell
    }
}

// MARK: - UITableViewDelegate
extension VideoQualitySelectionPanel: UITableViewDelegate {

    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return kCellRowHeight
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        selectedClosure?(resolutions[indexPath.row])
    }
}


class VideoQualityCell: UITableViewCell {
    public static let identifier = "VideoResolutionCell"
    
    let contentLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 16)
        label.textColor = .g7
        return label
    }()
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setupStyle()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(contentLabel)
    }
    
    private func activateConstraints() {
        contentLabel.snp.makeConstraints { make in
            make.center.equalToSuperview()
        }
    }
    
    private func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
}


fileprivate extension String {
    static let cancelText: String = internalLocalized("Cancel")
    
    static func videoQualityToString(quality: TUIVideoQuality) -> String {
        switch quality {
        case .quality1080P:
            return "1080P"
        case .quality720P:
            return "720P"
        case .quality540P:
            return "540P"
        case .quality360P:
            return "360P"
        default:
            return "unknown"
        }
    }
}
