import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_cache_manager/flutter_cache_manager.dart';

class GiftCacheManager {
  static const key = 'GiftCacheKey_';
  static CacheManager instance = CacheManager(
    Config(
      key,
      stalePeriod: const Duration(days: 365),
      maxNrOfCacheObjects: 100,
      repo: JsonCacheInfoRepository(databaseName: key),
      fileSystem: IOFileSystem(key),
      fileService: HttpFileService(),
    ),
  );

  static Future<String> getCachedJson(String url) async {
    final file = await instance.getSingleFile(url, key: key + url);
    return await file.readAsString();
  }

  static Future<Uint8List> getCachedBytes(String url) async {
    final file = await instance.getSingleFile(url, key: key + url);
    return await file.readAsBytes();
  }

  static Future<File> getCachedFile(String url) async {
    return await instance.getSingleFile(url, key: key + url);
  }
}
