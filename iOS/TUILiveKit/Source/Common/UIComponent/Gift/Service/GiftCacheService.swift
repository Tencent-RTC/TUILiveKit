//
//  GiftCacheService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/18.
//

import Foundation
import CryptoKit

class GiftCacheService {
    private var cacheDirectory: URL = URL(fileURLWithPath: "")
    
    init() {
        guard let cacheDirectory = getCacheDirectory() else { return }
        let cacheGiftDirectory = cacheDirectory.appendingPathComponent("gift")
        setCacheDirectory(cacheGiftDirectory)
    }
    
    private func getCacheDirectory() -> URL? {
        let paths = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)
        return paths.first
    }
  
    func setCacheDirectory(_ cacheDirectoryUrl: URL) {
        cacheDirectory = cacheDirectoryUrl
    }
    
    func request(urlString: String, closure: ((Int, Data?) -> Void)?) {
        guard let key = keyForUrl(url: urlString) else { return }
        let result = readFileFromCacheDirectory(key: key)
        if result != nil {
            closure?(0, result)
            return
        }
        DispatchQueue.global().sync {
            guard let url = URL(string: urlString) else { return }
            let task = URLSession.shared.dataTask(with: url) {data, response, error in
                if error != nil {
                    closure?(-1, nil)
                    debugPrint("Error request url:\(urlString), error:\(String(describing: error))")
                    return
                }
                guard let data = data else { return }
                closure?(0, data)
                self.saveFileToCacheDirectory(key: key, data: data)
            }
            task.resume()
        }
    }
    
    private func keyForUrl(url: String) -> String? {
        guard let data = url.data(using: .utf8) else {
            return nil
        }

        let md5Hash = Insecure.MD5.hash(data: data)
        let hexString = Data(md5Hash).hexEncodedString()

        return hexString
    }
    
    private func readFileFromCacheDirectory(key: String) -> Data? {
        let fileURL = cacheDirectory.appendingPathComponent(key)
        do {
            let data = try Data(contentsOf: fileURL)
            return data
        } catch {
            debugPrint("Error readFile:\(fileURL.path), error:\(error)")
            return nil
        }
    }
    
    private func saveFileToCacheDirectory(key: String, data: Data) {
        let fileURL = cacheDirectory.appendingPathComponent(key)
        if !FileManager.default.fileExists(atPath: cacheDirectory.path) {
           try? FileManager.default.createDirectory(at: cacheDirectory, withIntermediateDirectories: true, attributes: nil)
        }
        if !FileManager.default.fileExists(atPath: fileURL.path) {
            FileManager.default.createFile(atPath: fileURL.path, contents: data)
        } else {
            do {
                try data.write(to: fileURL)
            } catch {
                debugPrint("Error saving file: \(error)")
            }
        }
    }
    
    func clearCacheDirectory() {
        do {
            let fileURLs = try FileManager.default.contentsOfDirectory(at: cacheDirectory,
                                                                       includingPropertiesForKeys: nil,
                                                                       options: [])
            for fileURL in fileURLs {
                try FileManager.default.removeItem(at: fileURL)
            }
        } catch {
            debugPrint("Error deleting files: \(error)")
        }
    }
}

private extension Data {
    func hexEncodedString() -> String {
        return map { String(format: "%02hhx", $0) }.joined()
    }
}
