
/*** light-js-config
  //@requireAbility
***/
/** * light-js-config
 //@requireAbility
 ***/
// studio 探针函数
light.sendLightCommand = function (source, property, target, targetProperty, value) {
    var _a, _b, _c, _d;
    var assetE = (_c = (_b = (_a = target['entityManager']) === null || _a === void 0 ? void 0 : _a.entitiesWithComponents('ClientProxy')) === null || _b === void 0 ? void 0 : _b.get) === null || _c === void 0 ? void 0 : _c.call(_b, 0);
    var p = assetE === null || assetE === void 0 ? void 0 : assetE.getClientProxyComponent();
    if (light.LightCommand) {
        (_d = p === null || p === void 0 ? void 0 : p.commandQueue) === null || _d === void 0 ? void 0 : _d.sendClientCommand(new light.LightCommand("LiteGraphEvent\n" + JSON.stringify({
            type: 'LiteGraphEvent',
            targetNodeId: target['id'] || '-1',
            targetSlotName: targetProperty || '',
            value: value,
        })));
    }
};
