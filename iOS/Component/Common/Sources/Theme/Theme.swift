//
//  Theme.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/11.
//

import UIKit

public extension UIColor {
    /// colors in the interaction specification
    static let b1 = themColor("#1C66E5")
    static let b2 = themColor("#00E5E5")
    static let c1 = themColor("#00C2A8")
    static let c2 = themColor("#6C54E8")
    static let c3 = themColor("#FF643D")
    static let c4 = themColor("#F23C5B")
    static let b1d = themColor("#4791FF")
    static let b2d = themColor("#1AFFC9")
    static let g1 = themColor("#0F1014")
    static let g2 = themColor("#22262E")
    static let g3 = themColor("#4F586B")
    static let g3Divider = themColor("#804F586B")
    static let g4 = themColor("#6B758A")
    static let g5 = themColor("#8F9AB2")
    static let g6 = themColor("#B2BBD1")
    static let g7 = themColor("#D5E0F2")
    static let g8 = themColor("#F2F5FC")
    static let g9 = themColor("#E7ECF6")
    static let flowKitRed = themColor("#FC5555")
    static let flowKitGreen = themColor("#29CC6A")
    static let flowKitBlue = themColor("#0099FF")
    static let flowKitWhite = themColor("#FFFFFF")
    static let flowKitPurple = themColor("#7B61FF")
    static let flowKitCharcoal = themColor("#222222")
    static let transparent = themColor("#00000000")

    
    /// colors in non-interactive specifications
    static let whiteColor = themColor("#D1D9EC")
    static let greyColor = themColor("#7C85A6")
    static let redColor = themColor("#E5395C")
    static let blue40Transparency = themColor("#4D4F586B")
    static let brandBlueColor = themColor("#006CFF")
    static let blackColor = themColor("#181B21")
    static let redPinkColor = themColor("#ED414D")
    static let redDotColor = themColor("#E04343")
    static let pureBlackColor = themColor("#000000")
    static let blueColor = themColor("#5592EE")
    static let lightBlueColor = themColor("#80BEF6")
    static let lightGrayColor = themColor("#B2BBD1")
    static let darkGrayColor = themColor("#4F586B")
    static let lightCyanColor = themColor("#EBF4FF")
    static let darkBlueColor = themColor("#1C66E4")
    static let lightPurpleColor = themColor("#8F9AB2")
    static let yellowColor = themColor("#FFCC00")
    static let darkNavyColor = themColor("#13294b")
    static let lightGreenColor = themColor("#00d4a5")
    static let orangeColor = themColor("#0FA968")
    static let deepSeaBlueColor = themColor("#0157DF")
    static let cyanColor = themColor("#25D1D1")
    static let grayColor = themColor("#C5CCDB")
    static let tipsGrayColor = themColor("#8F9AB2")
    static let pinkColor = themColor("#F15065")
    
    /// message color
    static let barrageColorMsg1 = themColor("#3074FD")
    static let barrageColorMsg2 = themColor("#3CCFA5")
    static let barrageColorMsg3 = themColor("#FF8607")
    static let barrageColorMsg4 = themColor("#F7AF97")
    static let barrageColorMsg5 = themColor("#FF8BB7")
    static let barrageColorMsg6 = themColor("#FC6091")
    static let barrageColorMsg7 = themColor("#FCAF41")
    static let barrageItemBackColor = themColor("#6622262E")

    /// color of gift
    static let giftTwoFifthBlackColor = themColor("#66000000")
    static let giftContentColor = themColor("#EE3D544D")
    
    /// color of seat
    static let seatContentColor = themColor("#F2F5FC1A")
    static let seatContentBorderColor = themColor("#F2F5FC1A")
    static let seatWaveColor = themColor("#FFDADE")
    
    /// temporary semantic color
    static let textPrimaryColor = themColor("#FFFFFFE6")
    static let textSecondaryColor = themColor("FFFFFF8C")
    static let textDisabledColor = themColor("FFFFFF24")
    static let bgOperateColor = themColor("#1F2024")
    static let bgEntrycardColor = themColor("#2B2C30")
    static let strokeModuleColor = themColor("48494F")
    static let buttonPrimaryDefaultColor = themColor("4086FF")

    private static func themColor(_ hex: String) -> UIColor {
        return UIColor(hex: hex) ?? .black
    }
}
