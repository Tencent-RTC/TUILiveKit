//
//  SystemImageCell.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/9.
//

import Foundation

private let imageDomain = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/"

enum ImageType: String {
    case cover = "voice_room_cover"
    case background = "voice_room_background"
}

struct SystemImageModel {
    var imageType: ImageType
    var imagePath: String
    var thumbnailImageUrl: URL? {
        let thumbnailImagePath = imagePath.replacingOccurrences(of: ".png", with: "_thumb.png")
        return URL(string: imageDomain + thumbnailImagePath)
    }
    var imageUrl: URL? {
        return URL(string: imageDomain + imagePath)
    }
}

class SystemImageFactory {
    static func getImageAssets(imageType: ImageType) -> [SystemImageModel] {
        let index = imageType == .cover ? 12 : 3
        var imageAssets: [SystemImageModel] = []
        for index in 1...index {
            let imagePath = imageType.rawValue + "\(index).png"
            imageAssets.append(SystemImageModel(imageType: imageType,
                                                imagePath: imagePath))
        }
        return imageAssets
    }
}

class SystemImageCell: UICollectionViewCell {
    let bgView: UIView = {
        let view = UIView()
        view.backgroundColor = .b1
        view.layer.cornerRadius = 10
        view.isHidden = true
        return view
    }()

    let imageView: UIImageView = {
        let view = UIImageView()
        view.layer.masksToBounds = true
        view.layer.cornerRadius = 6
        return view
    }()
    
    let selectedView: UIView = {
        let view = UIView()
        view.layer.cornerRadius = 25
        view.backgroundColor = .b1
        view.isHidden = true
        return view
    }()
    
    let selectedImageView: UIImageView = {
        let view = UIImageView()
        view.backgroundColor = .b1
        view.image = UIImage(named: "live_user_followed_icon", in: .liveBundle, compatibleWith: nil)
        view.isHidden = true
        return view
    }()

    var model: SystemImageModel? {
        didSet {
            guard let model = model else { return }
            if model.imageType == .cover {
                imageView.kf.setImage(with: model.imageUrl, placeholder: UIImage.placeholderImage)
            } else {
                imageView.kf.setImage(with: model.thumbnailImageUrl, placeholder: UIImage.placeholderImage)
            }
        }
    }
    
    override var isSelected: Bool {
        didSet {
            bgView.isHidden = !isSelected
            selectedView.isHidden = !isSelected
            selectedImageView.isHidden = !isSelected
        }
    }
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        constructViewHierarchy()
        activateConstraints()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

extension SystemImageCell {
    func constructViewHierarchy() {
        contentView.addSubview(bgView)
        contentView.addSubview(imageView)
        imageView.addSubview(selectedView)
        selectedView.addSubview(selectedImageView)
        
    }

    func activateConstraints() {
        bgView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        imageView.snp.makeConstraints({ make in
            make.edges.equalToSuperview().inset(4.scale375())
        })
        selectedView.snp.makeConstraints { make in
            make.centerX.equalTo(imageView.snp.right)
            make.centerY.equalTo(imageView.snp.top)
            make.size.equalTo(CGSize(width: 50.scale375(), height: 50.scale375()))
        }
        selectedImageView.snp.makeConstraints { make in
            make.top.equalTo(selectedView.snp.centerY)
            make.trailing.equalTo(selectedView.snp.centerX)
            make.size.equalTo(CGSize(width: 18.scale375(), height: 18.scale375()))
        }
    }
}
