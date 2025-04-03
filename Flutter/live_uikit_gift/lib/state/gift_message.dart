import 'gift_user.dart';

class GiftMessage {
  GiftUser? sender;
  GiftUser? receiver;
  GiftModel? gift;
  int? giftCount;

  GiftMessage({
    this.sender,
    this.receiver,
    this.gift,
    this.giftCount,
  });

  factory GiftMessage.fromJson(Map<String, dynamic> json) {
    return GiftMessage(
      sender: GiftUser.fromJson(json['sender']),
      receiver: GiftUser.fromJson(json['receiver']),
      gift: GiftModel.fromJson(json['gift']),
      giftCount: json['giftCount'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'sender': sender?.toJson(),
      'receiver': receiver?.toJson(),
      'gift': gift?.toJson(),
      'giftCount': giftCount,
    };
  }

  @override
  String toString() {
    return 'Data{sender: ${sender.toString()}, receiver: ${receiver.toString()}, gift: ${gift.toString()}'
        ', giftCount:$giftCount}';
  }
}

class GiftModel {
  String? giftId = "";
  String? imageUrl = "";
  String? animationUrl = "";
  String? giftName = "";
  int? price = 0;
  Map<String, String>? extInfo = {};

  GiftModel({
    this.giftId,
    this.giftName,
    this.imageUrl,
    this.animationUrl,
    this.price,
    this.extInfo,
  });

  factory GiftModel.fromJson(Map<String, dynamic> json) {
    return GiftModel(
      giftId: json['giftId'],
      giftName: json['giftName'],
      imageUrl: json['imageUrl'],
      animationUrl: json['animationUrl'],
      price: json['price'],
      extInfo: json['extInfo'] != null
          ? Map<String, String>.from(json['extInfo'])
          : Map<String, String>(),
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'giftId': giftId,
      'giftName': giftName,
      'imageUrl': imageUrl,
      'animationUrl': animationUrl,
      'price': price,
      'extInfo': extInfo != null ? Map<String, String>.from(extInfo!) : null,
    };
  }

  @override
  String toString() {
    return 'Gift{giftId: $giftId, giftName: $giftName, imageUrl: $imageUrl, '
        'animationUrl: $animationUrl, price: $price, extInfo: $extInfo}';
  }
}
