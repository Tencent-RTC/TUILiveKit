import 'package:gift/common/constants/constants.dart';
import 'package:gift/state/index.dart';

class GiftJson {
  GiftMessage? data;
  String? platform;
  String? version;
  String? businessID;

  GiftJson({
    this.data,
    this.platform,
    this.version,
    this.businessID,
  });

  factory GiftJson.fromJson(Map<String, dynamic> json) {
    return GiftJson(
      data: GiftMessage.fromJson(json['data']),
      platform: json['platform'],
      version: json['version'],
      businessID: json['businessID'],
    );
  }

  factory GiftJson.fromMessage(GiftMessage message) {
    return GiftJson(
      data: message,
      platform: Constants.imCustomMessageValuePlatform,
      version: Constants.imCustomMessageValueVersion,
      businessID: Constants.imCustomMessageValueBusinessIdGift,
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'data': data?.toJson(),
      'platform': platform,
      'version': version,
      'businessID': businessID,
    };
  }

  @override
  String toString() {
    return 'GiftJson{data: $data, platform: $platform, version: $version, businessID: $businessID}';
  }
}