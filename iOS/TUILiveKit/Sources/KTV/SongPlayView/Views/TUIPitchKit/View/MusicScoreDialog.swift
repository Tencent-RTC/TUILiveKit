//
//  MusicScoreDialog.swift
//  TUIKaraoke
//
//  Created by adams on 2023/6/14.
//

import UIKit

let textOffset: CGFloat = 15
class MusicScoreDialog: UIView {
    
    lazy var blurView: UIVisualEffectView = {
        let effect = UIBlurEffect(style: .dark)
        let view = UIVisualEffectView(effect: effect)
        return view
    }()
    
    lazy var bgImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.image = internalImage("final_score")
        return imageView
    }()
    
    lazy var closeBtn: UIButton = {
        let btn = UIButton(frame: .zero)
        btn.setTitle("关闭", for: .normal)
        btn.backgroundColor = .white.withAlphaComponent(0.2)
        btn.titleLabel?.font = UIFont(name: "PingFangSC-Regular", size: 12)
        btn.layer.cornerRadius = 11
        btn.addTarget(self, action: #selector(closeAction), for: .touchUpInside)
        return btn
    }()
    
    lazy var scoreLabel: MusicPointLabel = {
        let label = MusicPointLabel(frame: .zero)
        label.font = UIFont.boldSystemFont(ofSize: 66)
        return label
    }()
    
    lazy var pointsLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = UIFont.systemFont(ofSize: 16)
        label.textColor = UIColor(red: 0.879, green: 0.414, blue: 0.805, alpha: 1)
        return label
    }()
    
    lazy var songLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = UIColor.white
        label.font = UIFont(name: "PingFangSC-Regular", size: 12)
        return label
    }()
    
    lazy var scoreDescLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = UIColor.white
        label.font = UIFont(name: "PingFangSC-Medium", size: 20)
        label.text = "你的总评分"
        return label
    }()
    
    lazy var singImageView3: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.image = UIImage(named: "score3")
        return imageView
    }()
    
    lazy var scoreGradientLayer: CAGradientLayer = {
        let gradientLayer = CAGradientLayer()
        gradientLayer.startPoint = .zero
        gradientLayer.endPoint = CGPoint(x: 0, y: 1)
        gradientLayer.locations = [0, 1]
        gradientLayer.colors = [UIColor(red: 125/255.0, green: 0, blue: 189/255.0, alpha: 1).cgColor,
                                UIColor(red: 255/255.0, green: 136/255.0, blue: 221/255.0, alpha: 1).cgColor,]
        gradientLayer.frame = scoreLabel.frame
        return gradientLayer
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        setupUI()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        bgImageView.layer.addSublayer(scoreGradientLayer)
        scoreGradientLayer.mask = scoreLabel.layer
        scoreLabel.frame = scoreGradientLayer.bounds
    }
    
    private func setupUI() {
        addSubview(blurView)
        addSubview(bgImageView)
        addSubview(closeBtn)
        bgImageView.addSubview(scoreLabel)
        bgImageView.addSubview(pointsLabel)
        
        let containerView = UIView(frame: .zero)
        bgImageView.addSubview(containerView)
        containerView.addSubview(songLabel)
        containerView.addSubview(scoreDescLabel)
        containerView.addSubview(singImageView3)
     
        blurView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        bgImageView.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.centerY.equalToSuperview().offset(-30)
        }
        
        closeBtn.snp.makeConstraints { make in
            make.right.equalTo(bgImageView.snp.right)
            make.bottom.equalTo(bgImageView.snp.top).offset(32)
            make.size.equalTo(CGSize(width: 52, height: 22))
        }
        
        scoreLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview().offset(-textOffset + 18)
            make.right.equalTo(pointsLabel.snp.left).offset(-4)
        }
        
        pointsLabel.snp.makeConstraints { make in
            make.right.equalToSuperview().offset(-24)
            make.bottom.equalTo(scoreLabel.snp.bottom)
        }
        
        containerView.snp.makeConstraints { make in
            make.centerY.equalTo(scoreLabel.snp.centerY).offset(textOffset)
            make.left.equalToSuperview().offset(26)
        }
        
        songLabel.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.left.equalTo(singImageView3.snp.right).offset(4)
            make.right.equalToSuperview()
        }
        
        scoreDescLabel.snp.makeConstraints { make in
            make.top.equalTo(singImageView3.snp.bottom).offset(6)
            make.bottom.equalToSuperview()
            make.left.equalTo(singImageView3.snp.left)
            make.right.equalToSuperview()
        }
        
        singImageView3.snp.makeConstraints { make in
            make.left.equalToSuperview()
            make.centerY.equalTo(songLabel)
            make.size.equalTo(singImageView3.image?.size ?? .zero)
        }
            
    }
    
    func show(songName: String, score: Int32) {
        songLabel.text = songName
        scoreLabel.text = "\(score)"
        pointsLabel.text = "分"
        
        if #available(iOS 13.0, *) {
            if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene {
                if let keyWindow = windowScene.windows.first {
                    keyWindow.addSubview(self)
                }
            }
        } else {
            if let window = UIApplication.shared.windows.first {
                window.addSubview(self)
            }
        }
        
        snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

extension MusicScoreDialog {
    @objc func closeAction() {
        removeFromSuperview()
    }
}

class MusicPointLabel: UILabel {
    override init(frame: CGRect) {
        super.init(frame: frame)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func drawText(in rect: CGRect) {
        let textRect = textRect(forBounds: rect, limitedToNumberOfLines: numberOfLines)
        super.drawText(in: textRect)
    }
    
    override func textRect(forBounds bounds: CGRect, limitedToNumberOfLines numberOfLines: Int) -> CGRect {
        var textRect = super.textRect(forBounds: bounds, limitedToNumberOfLines: numberOfLines)
        textRect.origin.y = bounds.origin.y + bounds.size.height - textRect.size.height + textOffset
        return textRect
    }

}
