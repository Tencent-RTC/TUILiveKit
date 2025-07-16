import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

class AudienceDashboardWidget extends StatefulWidget {
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const AudienceDashboardWidget({
    super.key,
    required this.liveCoreController,
    required this.liveStreamManager,
  });

  @override
  State<StatefulWidget> createState() => _AudienceDashboardWidgetState();
}

class _AudienceDashboardWidgetState extends State<AudienceDashboardWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      color: LiveColors.designStandardG2,
      child: Stack(
        alignment: Alignment.topCenter,
        children: [
          _buildCloseButton(),
          _buildTitle(),
          _buildAnchorAvatar(),
          _buildAnchorName(),
        ],
      ),
    );
  }

  Widget _buildCloseButton() {
    return Positioned(
      right: 16.width,
      top: 58.height,
      width: 24.width,
      height: 24.height,
      child: GestureDetector(
        onTap: _closeWidget,
        child: Image.asset(
          LiveImages.audienceClose,
          package: Constants.pluginName,
          semanticLabel: 'Close button', // Accessibility
        ),
      ),
    );
  }

  Widget _buildTitle() {
    return Positioned(
      top: 120.height,
      child: GestureDetector(
        onTap: _closeWidget,
        child: Text(
          LiveKitLocalizations.of(context)!.common_live_has_stop,
          style: const TextStyle(
            color: LiveColors.designStandardFlowkitWhite,
            fontSize: 20,
          ),
        ),
      ),
    );
  }

  Widget _buildAnchorAvatar() {
    return Positioned(
      top: 190.height,
      child: Container(
        margin: const EdgeInsets.symmetric(horizontal: 8),
        width: 80.width,
        height: 80.height,
        child: ClipOval(
          child: Image.network(
            widget.liveCoreController.roomState.ownerInfo.avatarUrl,
            fit: BoxFit.cover,
            loadingBuilder: (context, child, loadingProgress) {
              if (loadingProgress == null) return child;
              return const Center(child: CircularProgressIndicator());
            },
            errorBuilder: (context, error, stackTrace) {
              return Image.asset(
                LiveImages.defaultAvatar,
                package: Constants.pluginName,
              );
            },
          ),
        ),
      ),
    );
  }

  Widget _buildAnchorName() {
    return Positioned(
      top: 275.height,
      child: SizedBox(
        width: 200.width, // Constrain width for ellipsis
        child: Text(
          widget.liveCoreController.roomState.ownerInfo.userName,
          overflow: TextOverflow.ellipsis,
          textAlign: TextAlign.center,
          maxLines: 1,
          style: const TextStyle(
            fontSize: 14,
            color: LiveColors.designStandardFlowkitWhite,
          ),
        ),
      ),
    );
  }

  void _closeWidget() => Navigator.of(context).pop();
}
