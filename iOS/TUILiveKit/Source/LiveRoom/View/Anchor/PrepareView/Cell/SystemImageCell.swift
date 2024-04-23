//
//  SystemImageCell.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/9.
//

import Foundation

class SystemImageModel {
    private(set) var url: String
    let isSelected: Observable<Bool> = Observable(false)
    init(url: String, isSelected: Bool = false) {
        self.url = url
        self.isSelected.value = isSelected
    }

    static func configs() -> [SystemImageModel] {
        return [SystemImageModel(url: coverUrl(1), isSelected: true),
                SystemImageModel(url: coverUrl(2)),
                SystemImageModel(url: coverUrl(3)),
                SystemImageModel(url: coverUrl(4)),
                SystemImageModel(url: coverUrl(5)),
                SystemImageModel(url: coverUrl(6)),
                SystemImageModel(url: coverUrl(7)),
                SystemImageModel(url: coverUrl(8)),
                SystemImageModel(url: coverUrl(9)),
                SystemImageModel(url: coverUrl(10)),
                SystemImageModel(url: coverUrl(11)),
                SystemImageModel(url: coverUrl(12)),]
    }

    static func coverUrl(_ index: Int = Int(arc4random())) -> String {
        let random = index % 12 + 1
        return "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover\(random).png"
    }
}

class SystemImageCell: UICollectionViewCell {
    lazy var bgView: UIView = {
        let view = UIView()
        view.layer.masksToBounds = true
        view.layer.borderColor = UIColor.b1.cgColor
        view.layer.cornerRadius = 10
        contentView.addSubview(view)
        view.snp.remakeConstraints({ make in
            make.edges.equalToSuperview()
        })
        return view
    }()

    lazy var imageView: UIImageView = {
        let view = UIImageView()
        view.layer.masksToBounds = true
        view.layer.cornerRadius = 6
        bgView.addSubview(view)
        view.snp.remakeConstraints({ make in
            make.edges.equalToSuperview().inset(8)
        })
        return view
    }()

    var model: SystemImageModel? {
        willSet {
            model?.isSelected.removeObserver(self)
        }
        didSet {
            guard let model = model else { return }
            imageView.kf.setImage(with: URL(string: model.url), placeholder: UIImage.placeholderImage)
            model.isSelected.addObserver(self) { [weak self] _, _ in
                self?.updateView()
            }
            updateView()
        }
    }
    
    private func updateView() {
        guard let model = model else { return }
        if model.isSelected.value {
            bgView.backgroundColor = .b1.withAlphaComponent(0.4)
            bgView.layer.borderWidth = 2
        } else {
            bgView.layer.borderWidth = 0
            bgView.backgroundColor = .g3
        }
    }
}
