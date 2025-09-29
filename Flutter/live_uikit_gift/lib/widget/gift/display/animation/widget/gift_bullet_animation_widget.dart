import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:live_uikit_gift/common/screen/screen_adapter.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';

import '../../../../../common/index.dart';
import '../../../../../state/index.dart';

class GiftBulletGroupWidget extends StatelessWidget {
  final String roomId;

  GiftBulletGroupWidget({super.key, required this.roomId});

  final List<Widget> _giftBullets = [];

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: TUIGiftStore().giftDataMap,
      builder: (context, giftDataMap, child) {
        final giftData = giftDataMap[roomId];
        if (giftData == null && _giftBullets.isEmpty) {
          return Container();
        }
        if (_giftBullets.length >= 3) {
          _giftBullets.removeAt(0);
        }
        _giftBullets.add(
            GiftBulletAnimationWidget(key: UniqueKey(), giftData: giftData!));
        return _giftBullets.isNotEmpty
            ? Column(
          mainAxisAlignment: MainAxisAlignment.start,
          children: _giftBullets.reversed.toList(),
        )
            : Container();
      },
    );
  }
}

class GiftBulletAnimationWidget extends StatefulWidget {
  final TUIGiftData giftData;

  const GiftBulletAnimationWidget({super.key, required this.giftData});

  @override
  GiftBulletAnimationWidgetState createState() =>
      GiftBulletAnimationWidgetState();
}

class GiftBulletAnimationWidgetState extends State<GiftBulletAnimationWidget>
    with TickerProviderStateMixin {
  late AnimationController _controllerGift;
  late Animation<double> _animationGift;

  late AnimationController _controllerUser;
  late Animation<Offset> _animationUserElasticOut;
  late Animation<double> _animationUserFadeIn;

  late AnimationController _controllerEnd;
  late Animation<double> _animationEndOpacity;
  late Animation<Offset> _animationEndTranslate;

  @override
  void initState() {
    super.initState();
    _controllerUser = AnimationController(
      vsync: this,
      duration: const Duration(milliseconds: 800),
    );

    _controllerGift = AnimationController(
      vsync: this,
      duration: const Duration(milliseconds: 500),
    );

    _animationGift = Tween<double>(
      begin: -54,
      end: 150,
    ).animate(CurvedAnimation(
      parent: _controllerGift,
      curve: Curves.easeInOut,
    ))
      ..addListener(() {
        setState(() {});
      });

    _animationUserFadeIn = Tween<double>(begin: 0, end: 1).animate(
      CurvedAnimation(
        parent: _controllerUser,
        curve: const Interval(0, 0.5, curve: Curves.easeInOut),
      ),
    );

    _animationUserElasticOut = Tween<Offset>(
      begin: const Offset(-1, 0),
      end: Offset.zero,
    ).animate(
      CurvedAnimation(
        parent: _controllerUser,
        curve: const Interval(0.5, 1, curve: Curves.elasticOut),
      ),
    );

    _controllerEnd = AnimationController(
      vsync: this,
      duration: const Duration(seconds: 1),
    );

    _animationEndOpacity = Tween<double>(begin: 1, end: 0).animate(
      CurvedAnimation(
        parent: _controllerEnd,
        curve: Curves.easeInOut,
      ),
    )
      ..addListener(() {
        setState(() {});
      });

    _animationEndTranslate = Tween<Offset>(
      begin: const Offset(0, 0),
      end: const Offset(0, -40),
    ).animate(
      CurvedAnimation(
        parent: _controllerEnd,
        curve: Curves.easeInOut,
      ),
    )
      ..addListener(() {
        setState(() {});
      });

    _controllerUser.addStatusListener((status) {
      if (status == AnimationStatus.completed) {
        _controllerGift.forward();
      }
    });

    _controllerGift.addStatusListener((status) {
      if (status == AnimationStatus.completed) {
        Future.delayed(const Duration(seconds: 3), () {
          if(!mounted) return;
          _controllerEnd.forward();
        });
      }
    });

    _controllerUser.forward();
  }

  @override
  void dispose() {
    _controllerUser.dispose();
    _controllerGift.dispose();
    _controllerEnd.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      constraints: BoxConstraints(maxWidth: context.adapter.getWidth(220)),
      padding: EdgeInsets.only(top: context.adapter.getHeight(2), left: context.adapter.getWidth(20)),
      child: Opacity(
        opacity: _animationEndOpacity.value,
        child: Transform.translate(
          offset: _animationEndTranslate.value,
          child: Container(
              height: context.adapter.getHeight(40),
              decoration: BoxDecoration(
                color: GiftColors.designG2.withAlpha(0x22),
                  borderRadius: BorderRadius.all(Radius.circular(context.adapter.getHeight(20)))
              ),
              child: Stack(
                children: [
                  Positioned(
                    left: context.adapter.getWidth(4),
                    top: context.adapter.getHeight(4),
                    width: context.adapter.getWidth(130),
                    child: SlideTransition(
                      position: _animationUserElasticOut,
                      child: FadeTransition(
                        opacity: _animationUserFadeIn,
                        child: Center(
                            child: Row(
                              mainAxisAlignment: MainAxisAlignment.start,
                              crossAxisAlignment: CrossAxisAlignment.center,
                              children: [
                                ClipRRect(
                                  borderRadius: BorderRadius.circular(context.adapter.getWidth(16)),
                                  child: CachedNetworkImage(
                                    width: context.adapter.getWidth(32),
                                    height: context.adapter.getWidth(32),
                                    imageUrl: widget.giftData.sender.avatarUrl,
                                    fit: BoxFit.fitWidth,
                                    placeholder: (context, url) =>
                                        _buildAvatarWidget(),
                                    errorWidget: (context, url, error) =>
                                        _buildAvatarWidget(),
                                  ),
                                ),
                                SizedBox(width: context.adapter.getWidth(4)),
                                Container(
                                  constraints: BoxConstraints(maxWidth: context.adapter.getWidth(80)),
                                  child: Column(
                                    mainAxisAlignment: MainAxisAlignment.start,
                                    crossAxisAlignment: CrossAxisAlignment.start,
                                    children: [
                                      Text(
                                        _buildUserName(
                                            context, widget.giftData.sender),
                                        style: const TextStyle(
                                            fontSize: 14,
                                            overflow: TextOverflow.ellipsis,
                                            color: GiftColors.designG8),
                                      ),
                                      LayoutBuilder(
                                          builder: (context, constraints) {
                                            return Text(
                                              widget.giftData.giftInfo.name,
                                              overflow: TextOverflow.ellipsis,
                                              style: const TextStyle(
                                                  fontSize: 10,
                                                  color: GiftColors.grayColor),
                                              maxLines: 1,
                                              softWrap: false,
                                              strutStyle: const StrutStyle(
                                                forceStrutHeight: true,
                                                height: 1,
                                              ),
                                            );
                                          }),
                                    ],
                                  ),
                                ),
                              ],
                            )),
                      ),
                    ),
                  ),
                  Positioned(
                    left: _animationGift.value,
                    top: 0,
                    width: context.adapter.getWidth(40),
                    height: context.adapter.getWidth(40),
                    child: CachedNetworkImage(
                      imageUrl: widget.giftData.giftInfo.iconUrl,
                      fit: BoxFit.fitWidth,
                      placeholder: (context, url) => Container(),
                      errorWidget: (context, url, error) => Container(),
                    ),
                  ),
                ],
              )),
        ),
      ),
    );
  }

  Widget _buildAvatarWidget() {
    return Image.asset(
      GiftImages.gitDefaultAvatar,
      package: Constants.pluginName,
      fit: BoxFit.fill,
    );
  }

  String _buildUserName(BuildContext context, TUIUserInfo user) {
    final selfInfo = TUIRoomEngine.getSelfInfo();
    if (user.userId == selfInfo.userId) {
      return GiftLocalizations.of(context)!.live_gift_me;
    }
    if (user.userName.isNotEmpty) {
      return user.userName;
    }
    return user.userId;
  }
}
