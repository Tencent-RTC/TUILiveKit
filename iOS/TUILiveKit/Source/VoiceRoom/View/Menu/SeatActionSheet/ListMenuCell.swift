//
//  ListMenuCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/19.
//

import UIKit
import SnapKit
import Combine

class ListMenuCell: UITableViewCell {
    static let identifier = "ListMenuCell"
    
    private var isViewReady: Bool = false
    private var cancellableSet = Set<AnyCancellable>()

    @Published var title: String?
    @Published var icon: String = ""
    
    private let menuLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        return label
    }()
    
    private let iconImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        return imageView
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
        title = nil
        icon = ""
    }
    
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
        addSubview(menuLabel)
    }
    
    private func activeViewConstraint() { 
        iconImageView.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.left.equalToSuperview().offset(16)
            make.height.width.equalTo(20)
        }
        menuLabel.snp.makeConstraints { make in
            let padding = ListMenuCell.iconWidth + ListMenuCell.paddingHorizontal + 10
            make.centerY.equalToSuperview()
            make.left.equalToSuperview().offset(padding)
            make.right.equalToSuperview().offset(-padding)
        }
    }
    
    private func bindInteraction() {
        $title
            .receive(on: RunLoop.main)
            .assign(to: \UILabel.text, on: menuLabel)
            .store(in: &cancellableSet)
        $icon
            .receive(on: RunLoop.main)
            .sink { [weak self] value in
                guard let self = self else { return }
                self.iconImageView.isHidden = value.isEmpty
                self.menuLabel.textAlignment = value.isEmpty ? .center : .left
                guard !value.isEmpty else { return }
                self.iconImageView.image = .liveBundleImage(value)
            }
            .store(in: &cancellableSet)
    }
}

extension ListMenuCell {
    static let iconWidth = 20
    static let paddingHorizontal = 16
}
