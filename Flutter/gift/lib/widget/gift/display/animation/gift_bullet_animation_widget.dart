import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:gift/common/index.dart';
import 'package:gift/state/index.dart';

class GiftBulletGroupWidget extends StatelessWidget {
  GiftBulletGroupWidget({super.key});

  final List<Widget> _giftBullets = [];

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: GiftStore().state.giftMessage,
      builder: (BuildContext context, GiftMessage message, Widget? child) {
        if (message.gift == null && _giftBullets.isEmpty) {
          return Container();
        }
        if (_giftBullets.length >= 3) {
          _giftBullets.removeAt(0);
        }
        _giftBullets.add(GiftBulletAnimationWidget(key: UniqueKey(), message: message));
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
  const GiftBulletAnimationWidget({super.key, required this.message});

  final GiftMessage message;

  @override
  GiftBulletAnimationWidgetState createState() => GiftBulletAnimationWidgetState();
}

class GiftBulletAnimationWidgetState extends State<GiftBulletAnimationWidget> with TickerProviderStateMixin {
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
    )..addListener(() {
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
    )..addListener(() {
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
      width: 200,
      height: 60,
      padding: const EdgeInsets.only(top: 20, left: 12),
      child: Opacity(
        opacity: _animationEndOpacity.value,
        child: Transform.translate(
          offset: _animationEndTranslate.value,
          child: Stack(
            children: [
              Positioned(
                left: 0,
                top: 0,
                width: 130,
                height: 40,
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
                          borderRadius: BorderRadius.circular(20),
                          child: CachedNetworkImage(
                            width: 40,
                            height: 40,
                            imageUrl: widget.message.sender?.avatarUrl ?? "",
                            fit: BoxFit.fitWidth,
                            placeholder: (context, url) => _buildAvatarWidget(),
                            errorWidget: (context, url, error) => _buildAvatarWidget(),
                          ),
                        ),
                        const SizedBox(
                          width: 3,
                        ),
                        Container(
                          constraints: const BoxConstraints(maxWidth: 80),
                          child: Column(
                            mainAxisAlignment: MainAxisAlignment.start,
                            crossAxisAlignment: CrossAxisAlignment.start,
                            children: [
                              Text(
                                _buildUserName(context, widget.message.sender),
                                style: const TextStyle(fontSize: 12, color: Colors.white),
                              ),
                              LayoutBuilder(builder: (context, constraints) {
                                return Text(
                                  _buildGiftMessage(context, widget.message),
                                  overflow: TextOverflow.ellipsis,
                                  style: const TextStyle(fontSize: 11, color: Colors.white),
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
                width: 40,
                height: 40,
                child: CachedNetworkImage(
                  imageUrl: widget.message.gift?.imageUrl ?? "",
                  fit: BoxFit.fitWidth,
                  placeholder: (context, url) => Container(),
                  errorWidget: (context, url, error) => Container(),
                ),
              ),
            ],
          ),
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

  String _buildUserName(BuildContext context, GiftUser? user) {
    if (user == null || user.userId == null) {
      return "";
    }
    if (user.userId == GiftStore().selfInfo.userId) {
      return GiftLocalizations.of(context)!.livekit_gift_me;
    }
    if (user.userName != null && user.userName!.isNotEmpty) {
      return user.userName ?? "";
    }
    return user.userId ?? "";
  }

  String _buildGiftMessage(BuildContext context, GiftMessage message) {
    return "${GiftLocalizations.of(context)!.gift_send_give}${_buildUserName(context, message.receiver)}"
        "${message.gift?.giftName}x${message.giftCount}";
  }
}
