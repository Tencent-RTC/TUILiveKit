//
//  MainViewController.swift
//  TUILiveKitApp
//
//  Created by adams on 2021/6/4.
//

import UIKit
import TUICore
import LiveStreamCore

class AppNavigationController: UINavigationController {
    override init(rootViewController: UIViewController) {
        super.init(rootViewController: rootViewController)
        interactivePopGestureRecognizer?.isEnabled = false
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        guard let supportedInterfaceOrientations =
                topViewController?.supportedInterfaceOrientations as? UIInterfaceOrientationMask
        else { return .portrait }
        return supportedInterfaceOrientations
    }
    override var shouldAutorotate: Bool {
        guard let shouldAutorotate = topViewController?.shouldAutorotate else { return false }
        return shouldAutorotate
    }
}

class MainViewController: UIViewController {

    private var menuItems: [MainItemModel] = []
    
    private lazy var collectionView: UICollectionView = {
        let flowLayout = UICollectionViewFlowLayout()
        flowLayout.sectionInset = UIEdgeInsets(top: 0, left: 12, bottom: 0, right: 12)
        flowLayout.minimumLineSpacing = 0
        flowLayout.minimumInteritemSpacing = 0
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: flowLayout)
        collectionView.register(MainCollectionCell.self,
                                forCellWithReuseIdentifier: MainCollectionCell.CellID)
        collectionView.backgroundColor = UIColor.clear
        collectionView.delegate = self
        collectionView.dataSource = self
        collectionView.isScrollEnabled = true
        collectionView.isPagingEnabled = true
        return collectionView
    }()
    
    override func viewDidLoad() {
        super.viewDidLoad()
        initMenuData()
        setupNavigation()
        constructViewHierarchy()
        activateConstraints()
        view.backgroundColor = .white
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }
}

// MARK: - Private
extension MainViewController {
    
    private func initMenuData() {
        menuItems = [
            MainItemModel(imageName: "main_item_video_live", title: .videoLiveTitle, content: .videoLiveDesc),
            MainItemModel(imageName: "main_item_voice_room", title: .voiceRoomTitle, content: .voiceRoomDesc),
        ]
    }
    
    private func constructViewHierarchy() {
        view.addSubview(collectionView)
    }
    
    private func activateConstraints() {
        collectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func setupNavigation() {
        let logoImageView = UIImageView(frame: CGRect(x: 0, y: 0, width: 30, height: 30))
        logoImageView.contentMode = .scaleAspectFit
        logoImageView.image = UIImage(named: "main_nav_logo")
        let logoImageItem = UIBarButtonItem(customView: logoImageView)
        
        let logoTitleItem = UIBarButtonItem(title: "TUILiveKit", style: .plain, target: nil, action: nil)
        logoTitleItem.tintColor = .black
        navigationItem.leftBarButtonItems = [logoImageItem, logoTitleItem]
        
        let debugButton = UIButton(type: .custom)
        debugButton.setImage(UIImage(named: "debug"), for: .normal)
        debugButton.addTarget(self, action: #selector(debugClick), for: .touchUpInside)
        debugButton.sizeToFit()
        let debugButtonItem = UIBarButtonItem(customView: debugButton)
        debugButtonItem.tintColor = .black
        
        let mineBtn = UIButton(frame: .zero)
        mineBtn.layer.cornerRadius = 15
        mineBtn.layer.masksToBounds = true
        mineBtn.addTarget(self, action: #selector(mineAction), for: .touchUpInside)
        mineBtn.kf.setBackgroundImage(with: URL(string: SettingsConfig.share.avatar),
                                      for: .normal,
                                      placeholder: UIImage(named: "main_mine_nor"))
        mineBtn.widthAnchor.constraint(equalToConstant: 30).isActive = true
        mineBtn.heightAnchor.constraint(equalToConstant: 30).isActive = true
        let mineItem = UIBarButtonItem(customView: mineBtn)
        navigationItem.rightBarButtonItems = [mineItem, debugButtonItem, getSeatGridViewLogicTestButton()]
    }
    
    private func getSeatGridViewLogicTestButton() -> UIBarButtonItem {
            let button = UIButton()
            button.setImage(UIImage(systemName: "waveform.path"), for: .normal)
            button.addTarget(self, action: #selector(seatGridViewLayoutTestButtonClick), for: .touchUpInside)
            button.sizeToFit()
            let barButtonItem = UIBarButtonItem(customView: button)
            barButtonItem.tintColor = .black
            return barButtonItem
        }
}

// MARK: - Actions
extension MainViewController  {
    
    @objc private func mineAction() {
        let viewController = MeViewController()
        navigationController?.pushViewController(viewController, animated: true)
    }
    
    @objc private func debugClick() {
        let debugVC = SandBoxFileBrowserViewController(bathPath: NSHomeDirectory())
        navigationController?.pushViewController(debugVC, animated: true)
    }
       
    @objc private func seatGridViewLayoutTestButtonClick() {
        let seatGridViewLayoutTestVC = LayoutTestViewController()
        navigationController?.pushViewController(seatGridViewLayoutTestVC, animated: true)
    }
}

// MARK: - UICollectionViewDataSource
extension MainViewController: UICollectionViewDataSource {
    
    func collectionView(_ collectionView: UICollectionView,
                        numberOfItemsInSection section: Int) -> Int {
        return menuItems.count
    }
    
    func collectionView(_ collectionView: UICollectionView,
                        cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: MainCollectionCell.CellID,
                                                      for: indexPath) as! MainCollectionCell
        cell.config(menuItems[indexPath.row])
        return cell
    }
    
}

// MARK: - UICollectionViewDelegate
extension MainViewController: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        if indexPath.item == 0 {
            let controller = VideoLiveViewController()
            navigationController?.pushViewController(controller, animated: true)
        } else {
            let controller = VoiceRoomViewController()
            navigationController?.pushViewController(controller, animated: true)
        }
    }
    
    func collectionView(_ collectionView: UICollectionView,
                        layout collectionViewLayout: UICollectionViewLayout,
                        sizeForItemAt indexPath: IndexPath) -> CGSize {
        return CGSize(width: ScreenWidth / 2 - 12, height: 106)
    }
}

// MARK: - Localized String
private extension String {
    static let videoLiveTitle = TUILiveKitAppLocalize("Video Live")
    static let videoLiveDesc = TUILiveKitAppLocalize("Live preview/Beauty filters/Multi-host")
    static let voiceRoomTitle = TUILiveKitAppLocalize("Voice Room")
    static let voiceRoomDesc = TUILiveKitAppLocalize("High audio quality/Large room/Smooth mic on/off")
}