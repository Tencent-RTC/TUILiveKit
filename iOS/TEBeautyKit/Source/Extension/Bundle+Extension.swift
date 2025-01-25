//
//  Bundle+Extension.swift
//  TEBeautyKit
//
//  Created by jack on 2024/12/31.
//  Copyright (c) 2024 Tencent.

import Foundation

extension Bundle {
    static var beautyKitBundle: Bundle {
        if let bundle = getBeautyKitBundle() {
            return bundle
        } else {
            return Bundle()
        }
    }
    
    private static func getBeautyKitBundle() -> Bundle? {
        guard let path = Bundle.main.path(forResource: "BeautyKitBundle", ofType: "bundle") else {
            return nil
        }
        return Bundle(path: path)

    }
}
