//
//  LinkMicTypeCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/27.
//

import UIKit

class LinkMicTypeCellData {
    let image: UIImage?
    let text: String
    var action: (() -> Void)?

    init(image: UIImage?, text: String, action: (() -> Void)? = nil) {
        self.image = image
        self.text = text
        self.action = action
    }
}

class LinkMicTypeCell: UITableViewCell {
    var data: LinkMicTypeCellData = LinkMicTypeCellData(image: UIColor.clear.trans2Image(), text: "") {
        didSet {
            leftImageView.image = data.image
            label.text = data.text
        }
    }
    
    private let lineView: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        return view
    }()

    private let leftImageView: UIImageView = {
        let view = UIImageView()
        return view
    }()

    private let label: UILabel = {
        let view = UILabel()
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .left
        return view
    }()

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }

    func constructViewHierarchy() {
        contentView.addSubview(lineView)
        contentView.addSubview(leftImageView)
        contentView.addSubview(label)
    }

    func activateConstraints() {
        lineView.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(leftImageView)
            make.trailing.equalToSuperview().inset(24)
            make.height.equalTo(1)
        }

        leftImageView.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(16.scale375Width())
            make.width.equalTo(20.scale375Width())
            make.height.equalTo(20.scale375Width())
        }

        label.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalTo(leftImageView.snp.trailing).offset(10.scale375Width())
            make.width.equalTo(150.scale375Width())
            make.height.equalTo(22.scale375Height())
        }
    }

    override func setSelected(_ selected: Bool, animated: Bool) {
        super.setSelected(selected, animated: animated)
    }
}
