import 'package:gift/common/constants/index.dart';
import 'package:gift/state/index.dart';

class LikeJson {
  Data data;
  String platform;
  String version;
  String businessID;

  LikeJson({
    required this.data,
    required this.platform,
    required this.version,
    required this.businessID,
  });

  factory LikeJson.fromJson(Map<String, dynamic> json) {
    return LikeJson(
      data: Data.fromJson(json['data']),
      platform: json['platform'],
      version: json['version'],
      businessID: json['businessID'],
    );
  }

  factory LikeJson.fromData(GiftUser sender) {
    return LikeJson(
      data: Data(sender: sender),
      platform: Constants.imCustomMessageValuePlatform,
      version: Constants.imCustomMessageValueVersion,
      businessID: Constants.imCustomMessageValueBusinessIdLike,
    );
  }

  Map<String, dynamic> toJson() => {
    'data': data.toJson(),
    'platform': platform,
    'version': version,
    'businessID': businessID,
  };
}

class Data {
  GiftUser sender;

  Data({required this.sender});

  factory Data.fromJson(Map<String, dynamic> json) {
    return Data(
      sender: GiftUser.fromJson(json['sender']),
    );
  }

  Map<String, dynamic> toJson() => {
    'sender': sender.toJson(),
  };
}