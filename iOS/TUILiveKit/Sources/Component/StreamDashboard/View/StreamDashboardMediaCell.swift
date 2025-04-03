//
//  LiveDashboardCell.swift
//
//
//  Created by jack on 2024/11/21.
//

import Foundation

class StreamDashboardMediaCell: UICollectionViewCell {
    static let CellID: String = "StreamDashboardMediaCell"
    
    enum VideoDataType {
        case resolution
        case bitrate
        case fps
    }
    
    enum AudioDataType {
        case bitrate
        case sampleRate
    }
    
    private lazy var containerView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .black.withAlphaComponent(0.2)
        view.layer.cornerRadius = 12
        view.layer.masksToBounds = true
        return view
    }()
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 14, weight: .semibold)
        label.textColor = .g7
        label.textAlignment = .center
        return label
    }()
    
    private lazy var videoTitleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.text = .videoText
        label.font = .customFont(ofSize: 14, weight: .semibold)
        label.textColor = .g7
        return label
    }()
    
    private lazy var videoTableView: UITableView = {
        let view = UITableView(frame: .zero, style: .plain)
        view.isScrollEnabled = false
        view.isUserInteractionEnabled = false
        view.delegate = self
        view.dataSource = self
        view.register(StreamDashboardMediaItemCell.self, forCellReuseIdentifier: StreamDashboardMediaItemCell.CellID)
        view.backgroundColor = .black.withAlphaComponent(0.3)
        view.layer.cornerRadius = 12
        view.layer.masksToBounds = true
        view.separatorInset = .init(top: 0, left: 20, bottom: 0, right: 20)
        view.separatorColor = .g3
        return view
    }()
    
    private lazy var audioTitleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.text = .audioText
        label.font = .customFont(ofSize: 14, weight: .semibold)
        label.textColor = .g7
        return label
    }()
    
    private lazy var audioTableView: UITableView = {
        let view = UITableView(frame: .zero, style: .plain)
        view.isScrollEnabled = false
        view.isUserInteractionEnabled = false
        view.delegate = self
        view.dataSource = self
        view.register(StreamDashboardMediaItemCell.self, forCellReuseIdentifier: StreamDashboardMediaItemCell.CellID)
        view.backgroundColor = .black.withAlphaComponent(0.3)
        view.layer.cornerRadius = 12
        view.layer.masksToBounds = true
        view.separatorInset = .init(top: 0, left: 20, bottom: 0, right: 20)
        view.separatorColor = .g3
        return view
    }()
    
    
    private var videoDataSource: [VideoDataType] = [.resolution, .bitrate, .fps]
    private var audioDataSource: [AudioDataType] = [.sampleRate, .bitrate]
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        contentView.backgroundColor = .clear
    }
    
    override func preferredLayoutAttributesFitting(_ layoutAttributes: UICollectionViewLayoutAttributes) -> UICollectionViewLayoutAttributes {
        self.setNeedsLayout()
        self.layoutIfNeeded()
        
        let size = self.contentView.systemLayoutSizeFitting(layoutAttributes.size)
        var cellFrame = layoutAttributes.frame
        cellFrame.size.height = size.height
        layoutAttributes.frame = cellFrame
        return layoutAttributes
    }
    
    private var data: StreamDashboardUser = StreamDashboardUser()
    func updateData(_ data: StreamDashboardUser) {
        self.data = data
        if data.isLocal {
            titleLabel.text = .localText
        } else {
            titleLabel.text = .remoteText + ": \(data.userId)"
        }
        self.videoTableView.reloadData()
        self.audioTableView.reloadData()
    }
}

extension StreamDashboardMediaCell {
    
    private func constructViewHierarchy() {
        contentView.addSubview(containerView)
        containerView.addSubview(titleLabel)
        containerView.addSubview(videoTitleLabel)
        containerView.addSubview(videoTableView)
        containerView.addSubview(audioTitleLabel)
        containerView.addSubview(audioTableView)
    }
    
    private func activateConstraints() {
        containerView.snp.makeConstraints { make in
            make.top.bottom.equalToSuperview()
            make.leading.trailing.equalToSuperview().inset(20)
        }
        titleLabel.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(30)
            make.top.equalTo(10)
        }
        videoTitleLabel.snp.makeConstraints { make in
            make.leading.equalTo(20)
            make.top.equalTo(titleLabel.snp.bottom)
        }
        videoTableView.snp.makeConstraints { make in
            make.top.equalTo(videoTitleLabel.snp.bottom).offset(10)
            make.leading.trailing.equalToSuperview().inset(20)
            make.height.equalTo(StreamDashboardMediaItemCell.CellHeight * CGFloat(videoDataSource.count))
        }
        audioTitleLabel.snp.makeConstraints { make in
            make.leading.equalTo(20)
            make.top.equalTo(videoTableView.snp.bottom).offset(10)
        }
        audioTableView.snp.makeConstraints { make in
            make.top.equalTo(audioTitleLabel.snp.bottom).offset(10)
            make.leading.trailing.equalToSuperview().inset(20)
            make.height.equalTo(StreamDashboardMediaItemCell.CellHeight * CGFloat(audioDataSource.count))
            make.bottom.equalToSuperview().offset(-20)
        }
    }
    
    private func updateVideoCellData(cell: StreamDashboardMediaItemCell, dataType: VideoDataType) {
        switch dataType {
        case .bitrate:
            cell.titleLabel.text = .videoBitrateText
            cell.valueLabel.text = "\(data.videoBitrate) kbps"
        case .fps:
            cell.titleLabel.text = .videoFrameRateText
            cell.valueLabel.text = "\(data.videoFrameRate) FPS"
        case .resolution:
            cell.titleLabel.text = .videoResolutionText
            cell.valueLabel.text = "\(data.videoResolution)"
        }
    }
    
    private func updateAudioCellData(cell: StreamDashboardMediaItemCell, dataType: AudioDataType) {
        switch dataType {
        case .bitrate:
            cell.titleLabel.text = .audioBitrateText
            cell.valueLabel.text = "\(data.audioBitrate) kbps"
        case .sampleRate:
            cell.titleLabel.text = .audioSampleRateText
            cell.valueLabel.text = "\(data.audioSampleRate) Hz"
        }
    }
}

extension StreamDashboardMediaCell: UITableViewDataSource {
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if tableView == videoTableView {
            return videoDataSource.count
        }
        if tableView == audioTableView {
            return audioDataSource.count
        }
        return 0
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: StreamDashboardMediaItemCell.CellID, for: indexPath) as! StreamDashboardMediaItemCell
        if tableView == videoTableView {
            updateVideoCellData(cell: cell, dataType: videoDataSource[indexPath.row])
        }
        if tableView == audioTableView {
            updateAudioCellData(cell: cell, dataType: audioDataSource[indexPath.row])
        }
        return cell
    }
}

extension StreamDashboardMediaCell: UITableViewDelegate {
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return StreamDashboardMediaItemCell.CellHeight
    }
}

class StreamDashboardMediaItemCell: UITableViewCell {
    static let CellID: String = "StreamDashboardMediaItemCell"
    static let CellHeight: CGFloat = 38
    
    lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 12, weight: .semibold)
        label.textColor = .white
        return label
    }()
    
    lazy var valueLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 12)
        label.textColor = .white
        return label
    }()
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        constructViewHierarchy()
        activateConstraints()
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(valueLabel)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalTo(20)
            make.centerY.equalToSuperview()
        }
        valueLabel.snp.makeConstraints { make in
            make.trailing.equalTo(-20)
            make.centerY.equalToSuperview()
        }
    }
}

fileprivate extension String {
    
    static let localText = localized("Local User")
    static let remoteText = localized("Remote User")
    
    static let videoText = localized("Video Information")
    static let videoResolutionText = localized("Resolution")
    static let videoBitrateText = localized("Video Bitrate")
    static let videoFrameRateText = localized("Video FPS")
    
    static let audioText = localized("Audio Information")
    static let audioSampleRateText = localized("Audio Sample Rate")
    static let audioBitrateText = localized("Audio Bitrate")
}
