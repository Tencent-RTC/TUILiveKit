import 'package:tencent_live_uikit/state/operation/operation_state.dart';
import 'package:tencent_live_uikit/state/view/view_state.dart';

class LiveState {
  final OperationState operationState = new OperationState();
  final ViewState      viewState      = new ViewState();

  void reset() {
    viewState.reset();
    operationState.reset();
  }
}