import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/live_navigator_observer.dart';

import 'component/room_list/widget/room_list_widget.dart';

class LiveListWidget extends StatefulWidget {
  const LiveListWidget({super.key});

  @override
  State<LiveListWidget> createState() => _LiveListWidgetState();
}

class _LiveListWidgetState extends State<LiveListWidget> with RouteAware {
  @override
  void didChangeDependencies() {
    super.didChangeDependencies();
    final route = ModalRoute.of(context);
    if (route == null) return;
    TUILiveKitNavigatorObserver.instance.subscribe(this, route);
  }

  @override
  void dispose() {
    TUILiveKitNavigatorObserver.instance.unsubscribe(this);
    super.dispose();
  }

  @override
  void didPopNext() {
    super.didPopNext();
    _refreshRoomList();
  }

  @override
  Widget build(BuildContext context) {
    return RoomListWidget(key: UniqueKey());
  }
}

extension on _LiveListWidgetState {
  void _refreshRoomList() {
    setState(() {});
  }
}
