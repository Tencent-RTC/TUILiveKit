
/*** light-js-config
  //@requireAbility
***/

(function (light) {
  if(light.studioLibLoaded) {
    return;
  }
  light.studioLibLoaded = true;
var SDKRuntime = /** @class */ (function () {
    function SDKRuntime() {
        this.currentTime = 0;
        this.behaviors = [];
    }
    SDKRuntime.prototype.initialize = function (entityManager, eventManager, scriptSystem) {
        var _this = this;
        var entities = entityManager.entitiesWithComponents(light.ScriptBehaviors.componentType);
        entities.forEach(function (entity) {
            var idComponent = entity.getComponent(light.EntityIdentifier);
            var lightBehaviorComponent = entity.getComponent(light.ScriptBehaviors);
            console.log("JS::lightBehaviorComponent: " + JSON.stringify(lightBehaviorComponent));
            if (!lightBehaviorComponent) {
                return;
            }
            lightBehaviorComponent.behaviorProperties.forEach(function (behaviorPropertiesJson) {
                console.log("JS::behaviorPropertiesJson: " + behaviorPropertiesJson);
                var behaviorProperties = JSON.parse(behaviorPropertiesJson);
                var BehaviorClass = SDKRuntime.BehaviorClasses[behaviorProperties.type];
                if (!BehaviorClass) {
                    return;
                }
                var behavior = new BehaviorClass(idComponent.id, entityManager, eventManager, scriptSystem);
                Object.assign(behavior, behaviorProperties);
                _this.addBehavior(behavior);
            });
        });
        this.behaviors.forEach(function (b) {
            if (b.configure) {
                b.configure();
            }
        });
        var maps = light.NodeContext.getAllNodesMap();
        light.FlowNodeClasses.forEach(function (node) {
            if (!maps[node.definition.meta.nodeType]) {
                node.nodeType = node.definition.meta.nodeType;
                light.NodeContext.unregisterNode(node.nodeType);
                light.NodeContext.registerNode(node);
            }
        });
    };
    SDKRuntime.prototype.addBehavior = function (it) {
        this.behaviors.push(it);
    };
    SDKRuntime.prototype.removeBehavior = function (it) {
        var index = this.behaviors.indexOf(it);
        if (index >= 0) {
            this.behaviors.splice(index, 1);
        }
    };
    SDKRuntime.prototype.update = function (time, entityManager, eventManager) {
        this.currentTime = time;
        this.behaviors.forEach(function (b) {
            if (b.enabled === false || b.destroyed) {
                return;
            }
            b.entityManager = entityManager;
            b.eventManager = eventManager;
            if (b.update) {
                b.update(time);
            }
        });
    };
    SDKRuntime.prototype.destroy = function () {
        this.behaviors.forEach(function (b) { var _a; return (_a = b.destroy) === null || _a === void 0 ? void 0 : _a.call(b); });
    };
    SDKRuntime.BehaviorClasses = {};
    return SDKRuntime;
}());
light.SDKRuntime = SDKRuntime;
light.runtime = new SDKRuntime();
var BaseBehavior = /** @class */ (function () {
    function BaseBehavior(entityId, entityManager, eventManager, scriptSystem) {
        this.enabled = true;
        this.destroyed = false;
        this._entity = undefined;
        this.entityId = entityId;
        this.eventManager = eventManager;
        this.entityManager = entityManager;
        this.scriptSystem = scriptSystem;
        this._entity = this.entityManager.getEntityById(this.entityId);
        light.runtime.addBehavior(this);
    }
    Object.defineProperty(BaseBehavior.prototype, "entity", {
        get: function () {
            return this.entityManager.getEntityById(this.entityId);
        },
        enumerable: false,
        configurable: true
    });
    BaseBehavior.prototype.destroy = function () {
        this.destroyed = true;
        this.enabled = false;
        light.runtime.removeBehavior(this);
    };
    BaseBehavior.definition = null;
    return BaseBehavior;
}());
light.BaseBehavior = BaseBehavior;
light.Behavior = function (definition) {
    return function _Behavior(constructor) {
        var selfProps = (constructor.prototype._definition
            || {});
        var superProps = JSON.parse(JSON.stringify(constructor.definition || {}));
        if (!definition.properties) {
            definition.properties = [];
        }
        constructor.definition = definition;
        definition.properties = definition.properties
            .concat(superProps.properties || [])
            .concat(selfProps.properties || []);
        delete constructor.prototype._definition;
        Object.defineProperty(constructor.prototype, 'definition', {
            get: function () {
                return constructor.definition;
            },
        });
        SDKRuntime.BehaviorClasses[definition.type] = constructor;
        return constructor;
    };
};
light.Property = function Property(type) {
    return function (target, propertyKey) {
        var proto = target;
        if (!proto._definition) {
            proto._definition = {
                type: 'object',
                properties: [],
            };
        }
        type.name = propertyKey;
        if (type.editable !== false) {
            type.editable = true;
        }
        proto._definition.properties.push(type);
    };
};
var GestureAction;
(function (GestureAction) {
    GestureAction[GestureAction["HEART"] = 0] = "HEART";
    GestureAction[GestureAction["PAPER"] = 1] = "PAPER";
    GestureAction[GestureAction["SCISSOR"] = 2] = "SCISSOR";
    GestureAction[GestureAction["FIST"] = 3] = "FIST";
    GestureAction[GestureAction["ONE"] = 4] = "ONE";
    GestureAction[GestureAction["LOVE"] = 5] = "LOVE";
    GestureAction[GestureAction["LIKE"] = 6] = "LIKE";
    GestureAction[GestureAction["OK"] = 7] = "OK";
    GestureAction[GestureAction["ROCK"] = 8] = "ROCK";
    GestureAction[GestureAction["SIX"] = 9] = "SIX";
    GestureAction[GestureAction["EIGHT"] = 10] = "EIGHT";
    GestureAction[GestureAction["LIFT"] = 11] = "LIFT";
    GestureAction[GestureAction["CONGRATULATE"] = 12] = "CONGRATULATE";
})(GestureAction || (GestureAction = {}));
light.GestureAction = GestureAction;
var FaceAction;
(function (FaceAction) {
    FaceAction[FaceAction["FaceDetected"] = 0] = "FaceDetected";
    FaceAction[FaceAction["OpenMouth"] = 1] = "OpenMouth";
    FaceAction[FaceAction["BlinkEyebrow"] = 2] = "BlinkEyebrow";
    FaceAction[FaceAction["BlinkEye"] = 3] = "BlinkEye";
    FaceAction[FaceAction["ShakeHead"] = 4] = "ShakeHead";
    FaceAction[FaceAction["Kiss"] = 5] = "Kiss";
    FaceAction[FaceAction["BlinkLeftEye"] = 6] = "BlinkLeftEye";
    FaceAction[FaceAction["BlinkRightEye"] = 7] = "BlinkRightEye";
    FaceAction[FaceAction["Nod"] = 8] = "Nod";
    FaceAction[FaceAction["Smile"] = 9] = "Smile";
    FaceAction[FaceAction["MouthOccluded"] = 10] = "MouthOccluded";
    FaceAction[FaceAction["LeftEyeOccluded"] = 11] = "LeftEyeOccluded";
    FaceAction[FaceAction["RightEyeOccluded"] = 12] = "RightEyeOccluded";
    FaceAction[FaceAction["DoubleEyeOccluded"] = 13] = "DoubleEyeOccluded";
})(FaceAction || (FaceAction = {}));
light.FaceAction = FaceAction;
var NodeClasses = {};
light.NodeContext = /** @class */ (function () {
    function NodeContext(entityManager, eventManager, scriptSystem) {
        this.entityManager = entityManager;
        this.eventManager = eventManager;
        this.scriptSystem = scriptSystem;
    }
    NodeContext.registerNode = function (clazz) {
        NodeClasses[clazz.nodeType] = clazz;
    };
    NodeContext.unregisterNode = function (nodeType) {
        if (NodeClasses[nodeType]) {
            delete NodeClasses[nodeType];
        }
    };
    NodeContext.getAllNodesMap = function () {
        return NodeClasses;
    };
    NodeContext.prototype.create = function (type) {
        var Clazz = NodeClasses[type];
        if (Clazz) {
            var it_1 = new Clazz();
            it_1.entityManager = this.entityManager;
            it_1.eventManager = this.eventManager;
            it_1.scriptSystem = this.scriptSystem;
            return it_1;
        }
        console.log("Cannot find node: " + type);
    };
    NodeContext.prototype.connectData = function (source, property, target, targetProperty) {
        var descriptor = {
            configurable: true,
            enumerable: true,
            get: function () {
                return source["_studio_" + property];
            },
            set: function (val) {
                source["_studio_" + property] = val;
                // set值的时候 会触发函数
                var tasks = source['outputArray'];
                tasks.forEach(function (task) { return task(); });
            },
        };
        source["_studio_" + property] = source[property];
        Object.defineProperty(source, property, descriptor);
        if (!source['outputArray']) {
            source['outputArray'] = [];
        }
        var tasks = source['outputArray'];
        var task = function () {
            if (light.sendLightCommand) {
                light.sendLightCommand(source, property, target, targetProperty, JSON.stringify(source[property]));
            }
            target[targetProperty] = source[property];
            if (target['Process'] != undefined) {
                target['Process']();
            }
        };
        tasks.push(task);
    };
    NodeContext.prototype.connectEvent = function (source, property, target, targetProperty) {
        var propertyTasksName = property + "Tasks__";
        if (!source[propertyTasksName]) {
            // 重写应该基于 instance
            var descriptor = {
                configurable: true,
                enumerable: true,
                value: function () {
                    var tasks = this[propertyTasksName];
                    tasks.forEach(function (task) { return task(); });
                },
            };
            Object.defineProperty(source, property, descriptor);
            source[propertyTasksName] = [];
        }
        var tasks = source[propertyTasksName];
        var task = function () {
            if (light.sendLightCommand) {
                light.sendLightCommand(source, property, target, targetProperty);
            }
            target[targetProperty]();
        };
        tasks.push(task);
    };
    return NodeContext;
}());
light.FlowNodeClasses = light.FlowNodeClasses || [];
light['afterconfigure'] = function (entityManager, eventManager, scriptSystem) {
    var _a;
    (_a = light.BehaviorClasses) === null || _a === void 0 ? void 0 : _a.forEach(function (clazz) {
        light.SDKRuntime.BehaviorClasses[clazz.definition.type] = clazz;
    });
    light.runtime.initialize(entityManager, eventManager, scriptSystem);
    light.emit('start', entityManager, eventManager, scriptSystem);
};
function update(time, entityManager, eventManager) {
    light.runtime.update(time, entityManager, eventManager);
    light.emit('update', time, entityManager, eventManager);
}
light.getCurrentTime = function () {
    return light.runtime.currentTime;
};
light.update = update;
light.receive = function (event) {
    console.log(event.type());
    if (event.type() === 'CustomDataEvent') {
        var cEvent = event;
        light.emit(cEvent.event_type, JSON.parse(cEvent.json_data));
    }
    else {
        light.emit(event.type(), event);
    }
};

/******************************************************************************
Copyright (c) Microsoft Corporation.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
***************************************************************************** */
/* global Reflect, Promise */

var extendStatics = function(d, b) {
    extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
        function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
    return extendStatics(d, b);
};

function __extends(d, b) {
    if (typeof b !== "function" && b !== null)
        throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
    extendStatics(d, b);
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
}

var FaceAction$1;
(function (FaceAction) {
    FaceAction[FaceAction["FaceDetected"] = 0] = "FaceDetected";
    FaceAction[FaceAction["OpenMouth"] = 1] = "OpenMouth";
    FaceAction[FaceAction["BlinkEyebrow"] = 2] = "BlinkEyebrow";
    FaceAction[FaceAction["BlinkEye"] = 3] = "BlinkEye";
    FaceAction[FaceAction["ShakeHead"] = 4] = "ShakeHead";
    FaceAction[FaceAction["Kiss"] = 5] = "Kiss";
    FaceAction[FaceAction["BlinkLeftEye"] = 6] = "BlinkLeftEye";
    FaceAction[FaceAction["BlinkRightEye"] = 7] = "BlinkRightEye";
    FaceAction[FaceAction["Nod"] = 8] = "Nod";
    FaceAction[FaceAction["Smile"] = 9] = "Smile";
    FaceAction[FaceAction["MouthOccluded"] = 10] = "MouthOccluded";
    FaceAction[FaceAction["LeftEyeOccluded"] = 11] = "LeftEyeOccluded";
    FaceAction[FaceAction["RightEyeOccluded"] = 12] = "RightEyeOccluded";
    FaceAction[FaceAction["DoubleEyeOccluded"] = 13] = "DoubleEyeOccluded";
})(FaceAction$1 || (FaceAction$1 = {}));
var GestureAction$1;
(function (GestureAction) {
    GestureAction[GestureAction["HEART"] = 0] = "HEART";
    GestureAction[GestureAction["PAPER"] = 1] = "PAPER";
    GestureAction[GestureAction["SCISSOR"] = 2] = "SCISSOR";
    GestureAction[GestureAction["FIST"] = 3] = "FIST";
    GestureAction[GestureAction["ONE"] = 4] = "ONE";
    GestureAction[GestureAction["LOVE"] = 5] = "LOVE";
    GestureAction[GestureAction["LIKE"] = 6] = "LIKE";
    GestureAction[GestureAction["OK"] = 7] = "OK";
    GestureAction[GestureAction["ROCK"] = 8] = "ROCK";
    GestureAction[GestureAction["SIX"] = 9] = "SIX";
    GestureAction[GestureAction["EIGHT"] = 10] = "EIGHT";
    GestureAction[GestureAction["LIFT"] = 11] = "LIFT";
    GestureAction[GestureAction["CONGRATULATE"] = 12] = "CONGRATULATE";
})(GestureAction$1 || (GestureAction$1 = {}));
// 记录一次流程生命周期已开启的AI能力
var enabledAI = [];
function openAIFeature(features, entityManager, eventManager) {
    var aiRequire = new light.VectorString();
    features === null || features === void 0 ? void 0 : features.forEach(function (feature) {
        if (enabledAI.indexOf(feature) === -1) {
            aiRequire.add(feature);
            enabledAI.push(feature);
        }
    });
    if (aiRequire.size() > 0) {
        var event = new light.ScriptOpenAIEvent(entityManager, aiRequire);
        eventManager.emit(event);
    }
}
function getAIDataFromAIDataCenter(features, entityManager) {
    var results = [];
    features === null || features === void 0 ? void 0 : features.forEach(function (feature) {
        var featureAIResult = [];
        var AIData = light.AIDataUtils.GetAIDataFromAIDataCenter(entityManager, feature);
        for (var i = 0; i < AIData.size(); i++) {
            var aiDetectData = AIData.get(i);
            var detectParams = {};
            var AIDetectNames = aiDetectData.detect_params_.getKeys();
            for (var j = 0; j < AIDetectNames.size(); j++) {
                var AIDetectName = AIDetectNames.get(j);
                var AIDetectResultList = aiDetectData.detect_params_.get(AIDetectName);
                var AIDetectResults = [];
                for (var k = 0; k < AIDetectResultList.size(); k++) {
                    AIDetectResults.push(AIDetectResultList.get(k));
                }
                detectParams[AIDetectName] = AIDetectResults;
            }
            var jsAIDetectData = {
                aiType: aiDetectData.ai_type_,
                traceID: aiDetectData.trace_id_,
                detectParams: detectParams,
            };
            featureAIResult.push(jsAIDetectData);
        }
        results.push(featureAIResult);
    });
    return results;
}
function getAIClassData(features, entityManager) {
    var aiClassData = {};
    features === null || features === void 0 ? void 0 : features.forEach(function (feature) {
        var datas = light.AIDataUtils.GetJsEventListFromAIDataCenter(entityManager, feature);
        var keys = datas.getKeys();
        for (var i = 0; i < keys.size(); i++) {
            var key = keys.get(i);
            var value = JSON.parse(datas.get(key));
            aiClassData[key] = value;
        }
    });
    return aiClassData;
}
function getAIPointData(feature, entityManager) {
    var datas = light.AIDataUtils.GetAIPointDataFromAIDataCenter(entityManager, feature);
    var res = [];
    for (var i = 0; i < datas.size(); i++) {
        var data = JSON.parse(JSON.stringify(datas.get(i)));
        var points = [];
        var pointsData = datas.get(i)['point_array_'];
        if (pointsData === null || pointsData === void 0 ? void 0 : pointsData.size()) {
            for (var j = 0; j < pointsData.size(); j++) {
                points.push(pointsData.get(j));
            }
            data.point_array_ = points;
        }
        res.push(data);
    }
    return res;
}
function actionNameToEvent(name) {
    var str = name.toLowerCase();
    str = str.replace(/^\S/, function (s) { return s.toUpperCase(); });
    return "on" + str;
}

var Node = /** @class */ (function () {
    function Node() {
    }
    Node.prototype.addListener = function (name, func) {
        var bindFunc = func.bind(this);
        if (!this.bindFuncs) {
            this.bindFuncs = [];
        }
        this.bindFuncs.push(bindFunc);
        // @ts-ignore
        light.on(name, bindFunc);
    };
    Node.prototype.removeListener = function (name) {
        if (this.bindFuncs && this.bindFuncs.length > 0) {
            this.bindFuncs.forEach(function (bindFunc) {
                light.removeListener(name, bindFunc);
            });
            this.bindFuncs = [];
        }
    };
    Node.prototype.getEntityById = function (entityId) {
        if (typeof (entityId) !== 'number') {
            return undefined;
        }
        return this.entityManager.getEntityById(entityId);
    };
    return Node;
}());
var EventNode = /** @class */ (function (_super) {
    __extends(EventNode, _super);
    function EventNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return EventNode;
}(Node));
var DataProcessNode = /** @class */ (function (_super) {
    __extends(DataProcessNode, _super);
    function DataProcessNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return DataProcessNode;
}(Node));
var AITriggerNode = /** @class */ (function (_super) {
    __extends(AITriggerNode, _super);
    function AITriggerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        // 是否已触发
        _this.triggered = false;
        return _this;
    }
    AITriggerNode.prototype.Run = function () {
        openAIFeature(this.features, this.entityManager, this.eventManager);
        light.on('update', this.update.bind(this));
    };
    AITriggerNode.prototype.update = function () {
        if (this.shouldTrigger()) {
            this.on();
            if (!this.triggered) {
                this.detected();
            }
            this.triggered = true;
        }
        else {
            this.off();
            if (this.triggered) {
                this.lost();
            }
            this.triggered = false;
        }
    };
    // 有时触发
    AITriggerNode.prototype.on = function () { };
    // 无时触发
    AITriggerNode.prototype.off = function () { };
    // 从无到有时触发
    AITriggerNode.prototype.detected = function () { };
    // 从有到无时触发
    AITriggerNode.prototype.lost = function () { };
    return AITriggerNode;
}(EventNode));

/**
 * @author alteredq / http://alteredqualia.com/
 * @author mrdoob / http://mrdoob.com/
 * @author WestLangley / http://github.com/WestLangley
 * @author thezwap
 */
var _lut = [];

for (var i = 0; i < 256; i++) {
  _lut[i] = (i < 16 ? '0' : '') + i.toString(16);
}

var MathUtils = {
  DEG2RAD: Math.PI / 180,
  RAD2DEG: 180 / Math.PI,
  generateUUID: function generateUUID() {
    // http://stackoverflow.com/questions/105034/how-to-create-a-guid-uuid-in-javascript/21963136#21963136
    var d0 = Math.random() * 0xffffffff | 0;
    var d1 = Math.random() * 0xffffffff | 0;
    var d2 = Math.random() * 0xffffffff | 0;
    var d3 = Math.random() * 0xffffffff | 0;
    var uuid = "".concat(_lut[d0 & 0xff] + _lut[d0 >> 8 & 0xff] + _lut[d0 >> 16 & 0xff] + _lut[d0 >> 24 & 0xff], "-").concat(_lut[d1 & 0xff]).concat(_lut[d1 >> 8 & 0xff], "-").concat(_lut[d1 >> 16 & 0x0f | 0x40]).concat(_lut[d1 >> 24 & 0xff], "-").concat(_lut[d2 & 0x3f | 0x80]).concat(_lut[d2 >> 8 & 0xff], "-").concat(_lut[d2 >> 16 & 0xff]).concat(_lut[d2 >> 24 & 0xff]).concat(_lut[d3 & 0xff]).concat(_lut[d3 >> 8 & 0xff]).concat(_lut[d3 >> 16 & 0xff]).concat(_lut[d3 >> 24 & 0xff]); // .toUpperCase() here flattens concatenated strings to save heap memory space.

    return uuid.toUpperCase();
  },
  clamp: function clamp(value, min, max) {
    return Math.max(min, Math.min(max, value));
  },
  // compute euclidian modulo of m % n
  // https://en.wikipedia.org/wiki/Modulo_operation
  euclideanModulo: function euclideanModulo(n, m) {
    return (n % m + m) % m;
  },
  // Linear mapping from range <a1, a2> to range <b1, b2>
  mapLinear: function mapLinear(x, a1, a2, b1, b2) {
    return b1 + (x - a1) * (b2 - b1) / (a2 - a1);
  },
  // https://en.wikipedia.org/wiki/Linear_interpolation
  lerp: function lerp(x, y, t) {
    return (1 - t) * x + t * y;
  },
  // http://en.wikipedia.org/wiki/Smoothstep
  smoothstep: function smoothstep(x, min, max) {
    if (x <= min) return 0;
    if (x >= max) return 1;
    x = (x - min) / (max - min);
    return x * x * (3 - 2 * x);
  },
  smootherstep: function smootherstep(x, min, max) {
    if (x <= min) return 0;
    if (x >= max) return 1;
    x = (x - min) / (max - min);
    return x * x * x * (x * (x * 6 - 15) + 10);
  },
  // Random integer from <low, high> interval
  randInt: function randInt(low, high) {
    return low + Math.floor(Math.random() * (high - low + 1));
  },
  // Random float from <low, high> interval
  randFloat: function randFloat(low, high) {
    return low + Math.random() * (high - low);
  },
  // Random float from <-range/2, range/2> interval
  randFloatSpread: function randFloatSpread(range) {
    return range * (0.5 - Math.random());
  },
  degToRad: function degToRad(degrees) {
    return degrees * MathUtils.DEG2RAD;
  },
  radToDeg: function radToDeg(radians) {
    return radians * MathUtils.RAD2DEG;
  },
  isPowerOfTwo: function isPowerOfTwo(value) {
    return (value & value - 1) === 0 && value !== 0;
  },
  ceilPowerOfTwo: function ceilPowerOfTwo(value) {
    return Math.pow(2, Math.ceil(Math.log(value) / Math.LN2));
  },
  floorPowerOfTwo: function floorPowerOfTwo(value) {
    return Math.pow(2, Math.floor(Math.log(value) / Math.LN2));
  },
  setQuaternionFromProperEuler: function setQuaternionFromProperEuler(q, a, b, c, order) {
    // Intrinsic Proper Euler Angles - see https://en.wikipedia.org/wiki/Euler_angles
    // rotations are applied to the axes in the order specified by 'order'
    // rotation by angle 'a' is applied first, then by angle 'b', then by angle 'c'
    // angles are in radians
    var cos = Math.cos;
    var sin = Math.sin;
    var c2 = cos(b / 2);
    var s2 = sin(b / 2);
    var c13 = cos((a + c) / 2);
    var s13 = sin((a + c) / 2);
    var c1_3 = cos((a - c) / 2);
    var s1_3 = sin((a - c) / 2);
    var c3_1 = cos((c - a) / 2);
    var s3_1 = sin((c - a) / 2);

    switch (order) {
      case 'XYX':
        q.set(c2 * s13, s2 * c1_3, s2 * s1_3, c2 * c13);
        break;

      case 'YZY':
        q.set(s2 * s1_3, c2 * s13, s2 * c1_3, c2 * c13);
        break;

      case 'ZXZ':
        q.set(s2 * c1_3, s2 * s1_3, c2 * s13, c2 * c13);
        break;

      case 'XZX':
        q.set(c2 * s13, s2 * s3_1, s2 * c3_1, c2 * c13);
        break;

      case 'YXY':
        q.set(s2 * c3_1, c2 * s13, s2 * s3_1, c2 * c13);
        break;

      case 'ZYZ':
        q.set(s2 * s3_1, s2 * c3_1, c2 * s13, c2 * c13);
        break;

      default:
        console.warn("THREE.MathUtils: .setQuaternionFromProperEuler() encountered an unknown order: ".concat(order));
    }
  }
};

/**
 * @author mikael emtinger / http://gomo.se/
 * @author alteredq / http://alteredqualia.com/
 * @author WestLangley / http://github.com/WestLangley
 * @author bhouston / http://clara.io
 */

function Quaternion() {
  var x = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
  var y = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
  var z = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;
  var w = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 1;
  this._x = x;
  this._y = y;
  this._z = z;
  this._w = w;
}

Object.assign(Quaternion, {
  slerp: function slerp(qa, qb, qm, t) {
    return qm.copy(qa).slerp(qb, t);
  },
  slerpFlat: function slerpFlat(dst, dstOffset, src0, srcOffset0, src1, srcOffset1, t) {
    // fuzz-free, array-based Quaternion SLERP operation
    var x0 = src0[srcOffset0 + 0];
    var y0 = src0[srcOffset0 + 1];
    var z0 = src0[srcOffset0 + 2];
    var w0 = src0[srcOffset0 + 3];
    var x1 = src1[srcOffset1 + 0];
    var y1 = src1[srcOffset1 + 1];
    var z1 = src1[srcOffset1 + 2];
    var w1 = src1[srcOffset1 + 3];

    if (w0 !== w1 || x0 !== x1 || y0 !== y1 || z0 !== z1) {
      var s = 1 - t;
      var cos = x0 * x1 + y0 * y1 + z0 * z1 + w0 * w1;
      var dir = cos >= 0 ? 1 : -1;
      var sqrSin = 1 - cos * cos; // Skip the Slerp for tiny steps to avoid numeric problems:

      if (sqrSin > Number.EPSILON) {
        var sin = Math.sqrt(sqrSin);
        var len = Math.atan2(sin, cos * dir);
        s = Math.sin(s * len) / sin;
        t = Math.sin(t * len) / sin;
      }

      var tDir = t * dir;
      x0 = x0 * s + x1 * tDir;
      y0 = y0 * s + y1 * tDir;
      z0 = z0 * s + z1 * tDir;
      w0 = w0 * s + w1 * tDir; // Normalize in case we just did a lerp:

      if (s === 1 - t) {
        var f = 1 / Math.sqrt(x0 * x0 + y0 * y0 + z0 * z0 + w0 * w0);
        x0 *= f;
        y0 *= f;
        z0 *= f;
        w0 *= f;
      }
    }

    dst[dstOffset] = x0;
    dst[dstOffset + 1] = y0;
    dst[dstOffset + 2] = z0;
    dst[dstOffset + 3] = w0;
  },
  multiplyQuaternionsFlat: function multiplyQuaternionsFlat(dst, dstOffset, src0, srcOffset0, src1, srcOffset1) {
    var x0 = src0[srcOffset0];
    var y0 = src0[srcOffset0 + 1];
    var z0 = src0[srcOffset0 + 2];
    var w0 = src0[srcOffset0 + 3];
    var x1 = src1[srcOffset1];
    var y1 = src1[srcOffset1 + 1];
    var z1 = src1[srcOffset1 + 2];
    var w1 = src1[srcOffset1 + 3];
    dst[dstOffset] = x0 * w1 + w0 * x1 + y0 * z1 - z0 * y1;
    dst[dstOffset + 1] = y0 * w1 + w0 * y1 + z0 * x1 - x0 * z1;
    dst[dstOffset + 2] = z0 * w1 + w0 * z1 + x0 * y1 - y0 * x1;
    dst[dstOffset + 3] = w0 * w1 - x0 * x1 - y0 * y1 - z0 * z1;
    return dst;
  }
});
Object.defineProperties(Quaternion.prototype, {
  x: {
    get: function get() {
      return this._x;
    },
    set: function set(value) {
      this._x = value;

      this._onChangeCallback();
    }
  },
  y: {
    get: function get() {
      return this._y;
    },
    set: function set(value) {
      this._y = value;

      this._onChangeCallback();
    }
  },
  z: {
    get: function get() {
      return this._z;
    },
    set: function set(value) {
      this._z = value;

      this._onChangeCallback();
    }
  },
  w: {
    get: function get() {
      return this._w;
    },
    set: function set(value) {
      this._w = value;

      this._onChangeCallback();
    }
  }
});
Object.assign(Quaternion.prototype, {
  isQuaternion: true,
  set: function set(x, y, z, w) {
    this._x = x;
    this._y = y;
    this._z = z;
    this._w = w;

    this._onChangeCallback();

    return this;
  },
  clone: function clone() {
    return new this.constructor(this._x, this._y, this._z, this._w);
  },
  copy: function copy(quaternion) {
    this._x = quaternion.x;
    this._y = quaternion.y;
    this._z = quaternion.z;
    this._w = quaternion.w;

    this._onChangeCallback();

    return this;
  },
  setFromEuler: function setFromEuler(euler, update) {
    if (!(euler && euler.isEuler)) {
      throw new Error('THREE.Quaternion: .setFromEuler() now expects an Euler rotation rather than a Vector3 and order.');
    }

    var x = euler._x;
    var y = euler._y;
    var z = euler._z;
    var order = euler.order; // http://www.mathworks.com/matlabcentral/fileexchange/
    // 	20696-function-to-convert-between-dcm-euler-angles-quaternions-and-euler-vectors/
    //	content/SpinCalc.m

    var cos = Math.cos;
    var sin = Math.sin;
    var c1 = cos(x / 2);
    var c2 = cos(y / 2);
    var c3 = cos(z / 2);
    var s1 = sin(x / 2);
    var s2 = sin(y / 2);
    var s3 = sin(z / 2);

    switch (order) {
      case 'XYZ':
        this._x = s1 * c2 * c3 + c1 * s2 * s3;
        this._y = c1 * s2 * c3 - s1 * c2 * s3;
        this._z = c1 * c2 * s3 + s1 * s2 * c3;
        this._w = c1 * c2 * c3 - s1 * s2 * s3;
        break;

      case 'YXZ':
        this._x = s1 * c2 * c3 + c1 * s2 * s3;
        this._y = c1 * s2 * c3 - s1 * c2 * s3;
        this._z = c1 * c2 * s3 - s1 * s2 * c3;
        this._w = c1 * c2 * c3 + s1 * s2 * s3;
        break;

      case 'ZXY':
        this._x = s1 * c2 * c3 - c1 * s2 * s3;
        this._y = c1 * s2 * c3 + s1 * c2 * s3;
        this._z = c1 * c2 * s3 + s1 * s2 * c3;
        this._w = c1 * c2 * c3 - s1 * s2 * s3;
        break;

      case 'ZYX':
        this._x = s1 * c2 * c3 - c1 * s2 * s3;
        this._y = c1 * s2 * c3 + s1 * c2 * s3;
        this._z = c1 * c2 * s3 - s1 * s2 * c3;
        this._w = c1 * c2 * c3 + s1 * s2 * s3;
        break;

      case 'YZX':
        this._x = s1 * c2 * c3 + c1 * s2 * s3;
        this._y = c1 * s2 * c3 + s1 * c2 * s3;
        this._z = c1 * c2 * s3 - s1 * s2 * c3;
        this._w = c1 * c2 * c3 - s1 * s2 * s3;
        break;

      case 'XZY':
        this._x = s1 * c2 * c3 - c1 * s2 * s3;
        this._y = c1 * s2 * c3 - s1 * c2 * s3;
        this._z = c1 * c2 * s3 + s1 * s2 * c3;
        this._w = c1 * c2 * c3 + s1 * s2 * s3;
        break;

      default:
        console.warn("THREE.Quaternion: .setFromEuler() encountered an unknown order: ".concat(order));
    }

    if (update !== false) this._onChangeCallback();
    return this;
  },
  setFromAxisAngle: function setFromAxisAngle(axis, angle) {
    // http://www.euclideanspace.com/maths/geometry/rotations/conversions/angleToQuaternion/index.htm
    // assumes axis is normalized
    var halfAngle = angle / 2;
    var s = Math.sin(halfAngle);
    this._x = axis.x * s;
    this._y = axis.y * s;
    this._z = axis.z * s;
    this._w = Math.cos(halfAngle);

    this._onChangeCallback();

    return this;
  },
  setFromRotationMatrix: function setFromRotationMatrix(m) {
    // http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm
    // assumes the upper 3x3 of m is a pure rotation matrix (i.e, unscaled)
    var te = m.elements;
    var m11 = te[0];
    var m12 = te[4];
    var m13 = te[8];
    var m21 = te[1];
    var m22 = te[5];
    var m23 = te[9];
    var m31 = te[2];
    var m32 = te[6];
    var m33 = te[10];
    var trace = m11 + m22 + m33;

    if (trace > 0) {
      var s = 0.5 / Math.sqrt(trace + 1.0);
      this._w = 0.25 / s;
      this._x = (m32 - m23) * s;
      this._y = (m13 - m31) * s;
      this._z = (m21 - m12) * s;
    } else if (m11 > m22 && m11 > m33) {
      var _s = 2.0 * Math.sqrt(1.0 + m11 - m22 - m33);

      this._w = (m32 - m23) / _s;
      this._x = 0.25 * _s;
      this._y = (m12 + m21) / _s;
      this._z = (m13 + m31) / _s;
    } else if (m22 > m33) {
      var _s2 = 2.0 * Math.sqrt(1.0 + m22 - m11 - m33);

      this._w = (m13 - m31) / _s2;
      this._x = (m12 + m21) / _s2;
      this._y = 0.25 * _s2;
      this._z = (m23 + m32) / _s2;
    } else {
      var _s3 = 2.0 * Math.sqrt(1.0 + m33 - m11 - m22);

      this._w = (m21 - m12) / _s3;
      this._x = (m13 + m31) / _s3;
      this._y = (m23 + m32) / _s3;
      this._z = 0.25 * _s3;
    }

    this._onChangeCallback();

    return this;
  },
  setFromUnitVectors: function setFromUnitVectors(vFrom, vTo) {
    // assumes direction vectors vFrom and vTo are normalized
    var EPS = 0.000001;
    var r = vFrom.dot(vTo) + 1;

    if (r < EPS) {
      r = 0;

      if (Math.abs(vFrom.x) > Math.abs(vFrom.z)) {
        this._x = -vFrom.y;
        this._y = vFrom.x;
        this._z = 0;
        this._w = r;
      } else {
        this._x = 0;
        this._y = -vFrom.z;
        this._z = vFrom.y;
        this._w = r;
      }
    } else {
      // crossVectors( vFrom, vTo ); // inlined to avoid cyclic dependency on Vector3
      this._x = vFrom.y * vTo.z - vFrom.z * vTo.y;
      this._y = vFrom.z * vTo.x - vFrom.x * vTo.z;
      this._z = vFrom.x * vTo.y - vFrom.y * vTo.x;
      this._w = r;
    }

    return this.normalize();
  },
  angleTo: function angleTo(q) {
    return 2 * Math.acos(Math.abs(MathUtils.clamp(this.dot(q), -1, 1)));
  },
  rotateTowards: function rotateTowards(q, step) {
    var angle = this.angleTo(q);
    if (angle === 0) return this;
    var t = Math.min(1, step / angle);
    this.slerp(q, t);
    return this;
  },
  inverse: function inverse() {
    // quaternion is assumed to have unit length
    return this.conjugate();
  },
  conjugate: function conjugate() {
    this._x *= -1;
    this._y *= -1;
    this._z *= -1;

    this._onChangeCallback();

    return this;
  },
  dot: function dot(v) {
    return this._x * v._x + this._y * v._y + this._z * v._z + this._w * v._w;
  },
  lengthSq: function lengthSq() {
    return this._x * this._x + this._y * this._y + this._z * this._z + this._w * this._w;
  },
  length: function length() {
    return Math.sqrt(this._x * this._x + this._y * this._y + this._z * this._z + this._w * this._w);
  },
  normalize: function normalize() {
    var l = this.length();

    if (l === 0) {
      this._x = 0;
      this._y = 0;
      this._z = 0;
      this._w = 1;
    } else {
      l = 1 / l;
      this._x = this._x * l;
      this._y = this._y * l;
      this._z = this._z * l;
      this._w = this._w * l;
    }

    this._onChangeCallback();

    return this;
  },
  multiply: function multiply(q, p) {
    if (p !== undefined) {
      console.warn('THREE.Quaternion: .multiply() now only accepts one argument. Use .multiplyQuaternions( a, b ) instead.');
      return this.multiplyQuaternions(q, p);
    }

    return this.multiplyQuaternions(this, q);
  },
  premultiply: function premultiply(q) {
    return this.multiplyQuaternions(q, this);
  },
  multiplyQuaternions: function multiplyQuaternions(a, b) {
    // from http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/code/index.htm
    var qax = a._x;
    var qay = a._y;
    var qaz = a._z;
    var qaw = a._w;
    var qbx = b._x;
    var qby = b._y;
    var qbz = b._z;
    var qbw = b._w;
    this._x = qax * qbw + qaw * qbx + qay * qbz - qaz * qby;
    this._y = qay * qbw + qaw * qby + qaz * qbx - qax * qbz;
    this._z = qaz * qbw + qaw * qbz + qax * qby - qay * qbx;
    this._w = qaw * qbw - qax * qbx - qay * qby - qaz * qbz;

    this._onChangeCallback();

    return this;
  },
  slerp: function slerp(qb, t) {
    if (t === 0) return this;
    if (t === 1) return this.copy(qb);
    var x = this._x;
    var y = this._y;
    var z = this._z;
    var w = this._w; // http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/slerp/

    var cosHalfTheta = w * qb._w + x * qb._x + y * qb._y + z * qb._z;

    if (cosHalfTheta < 0) {
      this._w = -qb._w;
      this._x = -qb._x;
      this._y = -qb._y;
      this._z = -qb._z;
      cosHalfTheta = -cosHalfTheta;
    } else {
      this.copy(qb);
    }

    if (cosHalfTheta >= 1.0) {
      this._w = w;
      this._x = x;
      this._y = y;
      this._z = z;
      return this;
    }

    var sqrSinHalfTheta = 1.0 - cosHalfTheta * cosHalfTheta;

    if (sqrSinHalfTheta <= Number.EPSILON) {
      var s = 1 - t;
      this._w = s * w + t * this._w;
      this._x = s * x + t * this._x;
      this._y = s * y + t * this._y;
      this._z = s * z + t * this._z;
      this.normalize();

      this._onChangeCallback();

      return this;
    }

    var sinHalfTheta = Math.sqrt(sqrSinHalfTheta);
    var halfTheta = Math.atan2(sinHalfTheta, cosHalfTheta);
    var ratioA = Math.sin((1 - t) * halfTheta) / sinHalfTheta;
    var ratioB = Math.sin(t * halfTheta) / sinHalfTheta;
    this._w = w * ratioA + this._w * ratioB;
    this._x = x * ratioA + this._x * ratioB;
    this._y = y * ratioA + this._y * ratioB;
    this._z = z * ratioA + this._z * ratioB;

    this._onChangeCallback();

    return this;
  },
  equals: function equals(quaternion) {
    return quaternion._x === this._x && quaternion._y === this._y && quaternion._z === this._z && quaternion._w === this._w;
  },
  fromArray: function fromArray(array, offset) {
    if (offset === undefined) offset = 0;
    this._x = array[offset];
    this._y = array[offset + 1];
    this._z = array[offset + 2];
    this._w = array[offset + 3];

    this._onChangeCallback();

    return this;
  },
  toArray: function toArray(array, offset) {
    if (array === undefined) array = [];
    if (offset === undefined) offset = 0;
    array[offset] = this._x;
    array[offset + 1] = this._y;
    array[offset + 2] = this._z;
    array[offset + 3] = this._w;
    return array;
  },
  fromBufferAttribute: function fromBufferAttribute(attribute, index) {
    this._x = attribute.getX(index);
    this._y = attribute.getY(index);
    this._z = attribute.getZ(index);
    this._w = attribute.getW(index);
    return this;
  },
  _onChange: function _onChange(callback) {
    this._onChangeCallback = callback;
    return this;
  },
  _onChangeCallback: function _onChangeCallback() {}
});

/**
 * @author mrdoob / http://mrdoob.com/
 * @author kile / http://kile.stravaganza.org/
 * @author philogb / http://blog.thejit.org/
 * @author mikael emtinger / http://gomo.se/
 * @author egraether / http://egraether.com/
 * @author WestLangley / http://github.com/WestLangley
 */

var _vector = new Vector3();

var _quaternion = new Quaternion();

function Vector3() {
  var x = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
  var y = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
  var z = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;
  this.x = x;
  this.y = y;
  this.z = z;
}

Object.assign(Vector3.prototype, {
  isVector3: true,
  set: function set(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
    return this;
  },
  setScalar: function setScalar(scalar) {
    this.x = scalar;
    this.y = scalar;
    this.z = scalar;
    return this;
  },
  setX: function setX(x) {
    this.x = x;
    return this;
  },
  setY: function setY(y) {
    this.y = y;
    return this;
  },
  setZ: function setZ(z) {
    this.z = z;
    return this;
  },
  setComponent: function setComponent(index, value) {
    switch (index) {
      case 0:
        this.x = value;
        break;

      case 1:
        this.y = value;
        break;

      case 2:
        this.z = value;
        break;

      default:
        throw new Error("index is out of range: ".concat(index));
    }

    return this;
  },
  getComponent: function getComponent(index) {
    switch (index) {
      case 0:
        return this.x;

      case 1:
        return this.y;

      case 2:
        return this.z;

      default:
        throw new Error("index is out of range: ".concat(index));
    }
  },
  clone: function clone() {
    return new this.constructor(this.x, this.y, this.z);
  },
  copy: function copy(v) {
    this.x = v.x;
    this.y = v.y;
    this.z = v.z;
    return this;
  },
  add: function add(v, w) {
    if (w !== undefined) {
      console.warn('THREE.Vector3: .add() now only accepts one argument. Use .addVectors( a, b ) instead.');
      return this.addVectors(v, w);
    }

    this.x += v.x;
    this.y += v.y;
    this.z += v.z;
    return this;
  },
  addScalar: function addScalar(s) {
    this.x += s;
    this.y += s;
    this.z += s;
    return this;
  },
  addVectors: function addVectors(a, b) {
    this.x = a.x + b.x;
    this.y = a.y + b.y;
    this.z = a.z + b.z;
    return this;
  },
  addScaledVector: function addScaledVector(v, s) {
    this.x += v.x * s;
    this.y += v.y * s;
    this.z += v.z * s;
    return this;
  },
  sub: function sub(v, w) {
    if (w !== undefined) {
      console.warn('THREE.Vector3: .sub() now only accepts one argument. Use .subVectors( a, b ) instead.');
      return this.subVectors(v, w);
    }

    this.x -= v.x;
    this.y -= v.y;
    this.z -= v.z;
    return this;
  },
  subScalar: function subScalar(s) {
    this.x -= s;
    this.y -= s;
    this.z -= s;
    return this;
  },
  subVectors: function subVectors(a, b) {
    this.x = a.x - b.x;
    this.y = a.y - b.y;
    this.z = a.z - b.z;
    return this;
  },
  multiply: function multiply(v, w) {
    if (w !== undefined) {
      console.warn('THREE.Vector3: .multiply() now only accepts one argument. Use .multiplyVectors( a, b ) instead.');
      return this.multiplyVectors(v, w);
    }

    this.x *= v.x;
    this.y *= v.y;
    this.z *= v.z;
    return this;
  },
  multiplyScalar: function multiplyScalar(scalar) {
    this.x *= scalar;
    this.y *= scalar;
    this.z *= scalar;
    return this;
  },
  multiplyVectors: function multiplyVectors(a, b) {
    this.x = a.x * b.x;
    this.y = a.y * b.y;
    this.z = a.z * b.z;
    return this;
  },
  applyEuler: function applyEuler(euler) {
    if (!(euler && euler.isEuler)) {
      console.error('THREE.Vector3: .applyEuler() now expects an Euler rotation rather than a Vector3 and order.');
    }

    return this.applyQuaternion(_quaternion.setFromEuler(euler));
  },
  applyAxisAngle: function applyAxisAngle(axis, angle) {
    return this.applyQuaternion(_quaternion.setFromAxisAngle(axis, angle));
  },
  applyMatrix3: function applyMatrix3(m) {
    var x = this.x;
    var y = this.y;
    var z = this.z;
    var e = m.elements;
    this.x = e[0] * x + e[3] * y + e[6] * z;
    this.y = e[1] * x + e[4] * y + e[7] * z;
    this.z = e[2] * x + e[5] * y + e[8] * z;
    return this;
  },
  applyNormalMatrix: function applyNormalMatrix(m) {
    return this.applyMatrix3(m).normalize();
  },
  applyMatrix4: function applyMatrix4(m) {
    var x = this.x;
    var y = this.y;
    var z = this.z;
    var e = m.elements;
    var w = 1 / (e[3] * x + e[7] * y + e[11] * z + e[15]);
    this.x = (e[0] * x + e[4] * y + e[8] * z + e[12]) * w;
    this.y = (e[1] * x + e[5] * y + e[9] * z + e[13]) * w;
    this.z = (e[2] * x + e[6] * y + e[10] * z + e[14]) * w;
    return this;
  },
  applyQuaternion: function applyQuaternion(q) {
    var x = this.x;
    var y = this.y;
    var z = this.z;
    var qx = q.x;
    var qy = q.y;
    var qz = q.z;
    var qw = q.w; // calculate quat * vector

    var ix = qw * x + qy * z - qz * y;
    var iy = qw * y + qz * x - qx * z;
    var iz = qw * z + qx * y - qy * x;
    var iw = -qx * x - qy * y - qz * z; // calculate result * inverse quat

    this.x = ix * qw + iw * -qx + iy * -qz - iz * -qy;
    this.y = iy * qw + iw * -qy + iz * -qx - ix * -qz;
    this.z = iz * qw + iw * -qz + ix * -qy - iy * -qx;
    return this;
  },
  project: function project(camera) {
    return this.applyMatrix4(camera.matrixWorldInverse).applyMatrix4(camera.projectionMatrix);
  },
  unproject: function unproject(camera) {
    return this.applyMatrix4(camera.projectionMatrixInverse).applyMatrix4(camera.matrixWorld);
  },
  transformDirection: function transformDirection(m) {
    // input: THREE.Matrix4 affine matrix
    // vector interpreted as a direction
    var x = this.x;
    var y = this.y;
    var z = this.z;
    var e = m.elements;
    this.x = e[0] * x + e[4] * y + e[8] * z;
    this.y = e[1] * x + e[5] * y + e[9] * z;
    this.z = e[2] * x + e[6] * y + e[10] * z;
    return this.normalize();
  },
  divide: function divide(v) {
    this.x /= v.x;
    this.y /= v.y;
    this.z /= v.z;
    return this;
  },
  divideScalar: function divideScalar(scalar) {
    return this.multiplyScalar(1 / scalar);
  },
  min: function min(v) {
    this.x = Math.min(this.x, v.x);
    this.y = Math.min(this.y, v.y);
    this.z = Math.min(this.z, v.z);
    return this;
  },
  max: function max(v) {
    this.x = Math.max(this.x, v.x);
    this.y = Math.max(this.y, v.y);
    this.z = Math.max(this.z, v.z);
    return this;
  },
  clamp: function clamp(min, max) {
    // assumes min < max, componentwise
    this.x = Math.max(min.x, Math.min(max.x, this.x));
    this.y = Math.max(min.y, Math.min(max.y, this.y));
    this.z = Math.max(min.z, Math.min(max.z, this.z));
    return this;
  },
  clampScalar: function clampScalar(minVal, maxVal) {
    this.x = Math.max(minVal, Math.min(maxVal, this.x));
    this.y = Math.max(minVal, Math.min(maxVal, this.y));
    this.z = Math.max(minVal, Math.min(maxVal, this.z));
    return this;
  },
  clampLength: function clampLength(min, max) {
    var length = this.length();
    return this.divideScalar(length || 1).multiplyScalar(Math.max(min, Math.min(max, length)));
  },
  floor: function floor() {
    this.x = Math.floor(this.x);
    this.y = Math.floor(this.y);
    this.z = Math.floor(this.z);
    return this;
  },
  ceil: function ceil() {
    this.x = Math.ceil(this.x);
    this.y = Math.ceil(this.y);
    this.z = Math.ceil(this.z);
    return this;
  },
  round: function round() {
    this.x = Math.round(this.x);
    this.y = Math.round(this.y);
    this.z = Math.round(this.z);
    return this;
  },
  roundToZero: function roundToZero() {
    this.x = this.x < 0 ? Math.ceil(this.x) : Math.floor(this.x);
    this.y = this.y < 0 ? Math.ceil(this.y) : Math.floor(this.y);
    this.z = this.z < 0 ? Math.ceil(this.z) : Math.floor(this.z);
    return this;
  },
  negate: function negate() {
    this.x = -this.x;
    this.y = -this.y;
    this.z = -this.z;
    return this;
  },
  dot: function dot(v) {
    return this.x * v.x + this.y * v.y + this.z * v.z;
  },
  // TODO lengthSquared?
  lengthSq: function lengthSq() {
    return this.x * this.x + this.y * this.y + this.z * this.z;
  },
  length: function length() {
    return Math.sqrt(this.x * this.x + this.y * this.y + this.z * this.z);
  },
  manhattanLength: function manhattanLength() {
    return Math.abs(this.x) + Math.abs(this.y) + Math.abs(this.z);
  },
  normalize: function normalize() {
    return this.divideScalar(this.length() || 1);
  },
  setLength: function setLength(length) {
    return this.normalize().multiplyScalar(length);
  },
  lerp: function lerp(v, alpha) {
    this.x += (v.x - this.x) * alpha;
    this.y += (v.y - this.y) * alpha;
    this.z += (v.z - this.z) * alpha;
    return this;
  },
  lerpVectors: function lerpVectors(v1, v2, alpha) {
    this.x = v1.x + (v2.x - v1.x) * alpha;
    this.y = v1.y + (v2.y - v1.y) * alpha;
    this.z = v1.z + (v2.z - v1.z) * alpha;
    return this;
  },
  cross: function cross(v, w) {
    if (w !== undefined) {
      console.warn('THREE.Vector3: .cross() now only accepts one argument. Use .crossVectors( a, b ) instead.');
      return this.crossVectors(v, w);
    }

    return this.crossVectors(this, v);
  },
  crossVectors: function crossVectors(a, b) {
    var ax = a.x;
    var ay = a.y;
    var az = a.z;
    var bx = b.x;
    var by = b.y;
    var bz = b.z;
    this.x = ay * bz - az * by;
    this.y = az * bx - ax * bz;
    this.z = ax * by - ay * bx;
    return this;
  },
  projectOnVector: function projectOnVector(v) {
    var denominator = v.lengthSq();
    if (denominator === 0) return this.set(0, 0, 0);
    var scalar = v.dot(this) / denominator;
    return this.copy(v).multiplyScalar(scalar);
  },
  projectOnPlane: function projectOnPlane(planeNormal) {
    _vector.copy(this).projectOnVector(planeNormal);

    return this.sub(_vector);
  },
  reflect: function reflect(normal) {
    // reflect incident vector off plane orthogonal to normal
    // normal is assumed to have unit length
    return this.sub(_vector.copy(normal).multiplyScalar(2 * this.dot(normal)));
  },
  angleTo: function angleTo(v) {
    var denominator = Math.sqrt(this.lengthSq() * v.lengthSq());
    if (denominator === 0) return Math.PI / 2;
    var theta = this.dot(v) / denominator; // clamp, to handle numerical problems

    return Math.acos(MathUtils.clamp(theta, -1, 1));
  },
  distanceTo: function distanceTo(v) {
    return Math.sqrt(this.distanceToSquared(v));
  },
  distanceToSquared: function distanceToSquared(v) {
    var dx = this.x - v.x;
    var dy = this.y - v.y;
    var dz = this.z - v.z;
    return dx * dx + dy * dy + dz * dz;
  },
  manhattanDistanceTo: function manhattanDistanceTo(v) {
    return Math.abs(this.x - v.x) + Math.abs(this.y - v.y) + Math.abs(this.z - v.z);
  },
  setFromSpherical: function setFromSpherical(s) {
    return this.setFromSphericalCoords(s.radius, s.phi, s.theta);
  },
  setFromSphericalCoords: function setFromSphericalCoords(radius, phi, theta) {
    var sinPhiRadius = Math.sin(phi) * radius;
    this.x = sinPhiRadius * Math.sin(theta);
    this.y = Math.cos(phi) * radius;
    this.z = sinPhiRadius * Math.cos(theta);
    return this;
  },
  setFromCylindrical: function setFromCylindrical(c) {
    return this.setFromCylindricalCoords(c.radius, c.theta, c.y);
  },
  setFromCylindricalCoords: function setFromCylindricalCoords(radius, theta, y) {
    this.x = radius * Math.sin(theta);
    this.y = y;
    this.z = radius * Math.cos(theta);
    return this;
  },
  setFromMatrixPosition: function setFromMatrixPosition(m) {
    var e = m.elements;
    this.x = e[12];
    this.y = e[13];
    this.z = e[14];
    return this;
  },
  setFromMatrixScale: function setFromMatrixScale(m) {
    var sx = this.setFromMatrixColumn(m, 0).length();
    var sy = this.setFromMatrixColumn(m, 1).length();
    var sz = this.setFromMatrixColumn(m, 2).length();
    this.x = sx;
    this.y = sy;
    this.z = sz;
    return this;
  },
  setFromMatrixColumn: function setFromMatrixColumn(m, index) {
    return this.fromArray(m.elements, index * 4);
  },
  setFromMatrix3Column: function setFromMatrix3Column(m, index) {
    return this.fromArray(m.elements, index * 3);
  },
  equals: function equals(v) {
    return v.x === this.x && v.y === this.y && v.z === this.z;
  },
  fromArray: function fromArray(array, offset) {
    if (offset === undefined) offset = 0;
    this.x = array[offset];
    this.y = array[offset + 1];
    this.z = array[offset + 2];
    return this;
  },
  toArray: function toArray(array, offset) {
    if (array === undefined) array = [];
    if (offset === undefined) offset = 0;
    array[offset] = this.x;
    array[offset + 1] = this.y;
    array[offset + 2] = this.z;
    return array;
  },
  fromBufferAttribute: function fromBufferAttribute(attribute, index, offset) {
    if (offset !== undefined) {
      console.warn('THREE.Vector3: offset has been removed from .fromBufferAttribute().');
    }

    this.x = attribute.getX(index);
    this.y = attribute.getY(index);
    this.z = attribute.getZ(index);
    return this;
  },
  random: function random() {
    this.x = Math.random();
    this.y = Math.random();
    this.z = Math.random();
    return this;
  }
});

var _v1 = new Vector3();

var _m1 = new Matrix4();

var _zero = new Vector3(0, 0, 0);

var _one = new Vector3(1, 1, 1);

var _x = new Vector3();

var _y = new Vector3();

var _z = new Vector3();
/**
 * @author mrdoob / http://mrdoob.com/
 * @author supereggbert / http://www.paulbrunt.co.uk/
 * @author philogb / http://blog.thejit.org/
 * @author jordi_ros / http://plattsoft.com
 * @author D1plo1d / http://github.com/D1plo1d
 * @author alteredq / http://alteredqualia.com/
 * @author mikael emtinger / http://gomo.se/
 * @author timknip / http://www.floorplanner.com/
 * @author bhouston / http://clara.io
 * @author WestLangley / http://github.com/WestLangley
 */


function Matrix4() {
  this.elements = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];

  if (arguments.length > 0) {
    console.error('THREE.Matrix4: the constructor no longer reads arguments. use .set() instead.');
  }
}

Object.assign(Matrix4.prototype, {
  isMatrix4: true,
  set: function set(n11, n12, n13, n14, n21, n22, n23, n24, n31, n32, n33, n34, n41, n42, n43, n44) {
    var te = this.elements;
    te[0] = n11;
    te[4] = n12;
    te[8] = n13;
    te[12] = n14;
    te[1] = n21;
    te[5] = n22;
    te[9] = n23;
    te[13] = n24;
    te[2] = n31;
    te[6] = n32;
    te[10] = n33;
    te[14] = n34;
    te[3] = n41;
    te[7] = n42;
    te[11] = n43;
    te[15] = n44;
    return this;
  },
  identity: function identity() {
    this.set(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
    return this;
  },
  clone: function clone() {
    return new Matrix4().fromArray(this.elements);
  },
  copy: function copy(m) {
    var te = this.elements;
    var me = m.elements;
    te[0] = me[0];
    te[1] = me[1];
    te[2] = me[2];
    te[3] = me[3];
    te[4] = me[4];
    te[5] = me[5];
    te[6] = me[6];
    te[7] = me[7];
    te[8] = me[8];
    te[9] = me[9];
    te[10] = me[10];
    te[11] = me[11];
    te[12] = me[12];
    te[13] = me[13];
    te[14] = me[14];
    te[15] = me[15];
    return this;
  },
  copyPosition: function copyPosition(m) {
    var te = this.elements;
    var me = m.elements;
    te[12] = me[12];
    te[13] = me[13];
    te[14] = me[14];
    return this;
  },
  extractBasis: function extractBasis(xAxis, yAxis, zAxis) {
    xAxis.setFromMatrixColumn(this, 0);
    yAxis.setFromMatrixColumn(this, 1);
    zAxis.setFromMatrixColumn(this, 2);
    return this;
  },
  makeBasis: function makeBasis(xAxis, yAxis, zAxis) {
    this.set(xAxis.x, yAxis.x, zAxis.x, 0, xAxis.y, yAxis.y, zAxis.y, 0, xAxis.z, yAxis.z, zAxis.z, 0, 0, 0, 0, 1);
    return this;
  },
  extractRotation: function extractRotation(m) {
    // this method does not support reflection matrices
    var te = this.elements;
    var me = m.elements;

    var scaleX = 1 / _v1.setFromMatrixColumn(m, 0).length();

    var scaleY = 1 / _v1.setFromMatrixColumn(m, 1).length();

    var scaleZ = 1 / _v1.setFromMatrixColumn(m, 2).length();

    te[0] = me[0] * scaleX;
    te[1] = me[1] * scaleX;
    te[2] = me[2] * scaleX;
    te[3] = 0;
    te[4] = me[4] * scaleY;
    te[5] = me[5] * scaleY;
    te[6] = me[6] * scaleY;
    te[7] = 0;
    te[8] = me[8] * scaleZ;
    te[9] = me[9] * scaleZ;
    te[10] = me[10] * scaleZ;
    te[11] = 0;
    te[12] = 0;
    te[13] = 0;
    te[14] = 0;
    te[15] = 1;
    return this;
  },
  makeRotationFromEuler: function makeRotationFromEuler(euler) {
    if (!(euler && euler.isEuler)) {
      console.error('THREE.Matrix4: .makeRotationFromEuler() now expects a Euler rotation rather than a Vector3 and order.');
    }

    var te = this.elements;
    var x = euler.x;
    var y = euler.y;
    var z = euler.z;
    var a = Math.cos(x);
    var b = Math.sin(x);
    var c = Math.cos(y);
    var d = Math.sin(y);
    var e = Math.cos(z);
    var f = Math.sin(z);

    if (euler.order === 'XYZ') {
      var ae = a * e;
      var af = a * f;
      var be = b * e;
      var bf = b * f;
      te[0] = c * e;
      te[4] = -c * f;
      te[8] = d;
      te[1] = af + be * d;
      te[5] = ae - bf * d;
      te[9] = -b * c;
      te[2] = bf - ae * d;
      te[6] = be + af * d;
      te[10] = a * c;
    } else if (euler.order === 'YXZ') {
      var ce = c * e;
      var cf = c * f;
      var de = d * e;
      var df = d * f;
      te[0] = ce + df * b;
      te[4] = de * b - cf;
      te[8] = a * d;
      te[1] = a * f;
      te[5] = a * e;
      te[9] = -b;
      te[2] = cf * b - de;
      te[6] = df + ce * b;
      te[10] = a * c;
    } else if (euler.order === 'ZXY') {
      var _ce = c * e;

      var _cf = c * f;

      var _de = d * e;

      var _df = d * f;

      te[0] = _ce - _df * b;
      te[4] = -a * f;
      te[8] = _de + _cf * b;
      te[1] = _cf + _de * b;
      te[5] = a * e;
      te[9] = _df - _ce * b;
      te[2] = -a * d;
      te[6] = b;
      te[10] = a * c;
    } else if (euler.order === 'ZYX') {
      var _ae = a * e;

      var _af = a * f;

      var _be = b * e;

      var _bf = b * f;

      te[0] = c * e;
      te[4] = _be * d - _af;
      te[8] = _ae * d + _bf;
      te[1] = c * f;
      te[5] = _bf * d + _ae;
      te[9] = _af * d - _be;
      te[2] = -d;
      te[6] = b * c;
      te[10] = a * c;
    } else if (euler.order === 'YZX') {
      var ac = a * c;
      var ad = a * d;
      var bc = b * c;
      var bd = b * d;
      te[0] = c * e;
      te[4] = bd - ac * f;
      te[8] = bc * f + ad;
      te[1] = f;
      te[5] = a * e;
      te[9] = -b * e;
      te[2] = -d * e;
      te[6] = ad * f + bc;
      te[10] = ac - bd * f;
    } else if (euler.order === 'XZY') {
      var _ac = a * c;

      var _ad = a * d;

      var _bc = b * c;

      var _bd = b * d;

      te[0] = c * e;
      te[4] = -f;
      te[8] = d * e;
      te[1] = _ac * f + _bd;
      te[5] = a * e;
      te[9] = _ad * f - _bc;
      te[2] = _bc * f - _ad;
      te[6] = b * e;
      te[10] = _bd * f + _ac;
    } // bottom row


    te[3] = 0;
    te[7] = 0;
    te[11] = 0; // last column

    te[12] = 0;
    te[13] = 0;
    te[14] = 0;
    te[15] = 1;
    return this;
  },
  makeRotationFromQuaternion: function makeRotationFromQuaternion(q) {
    return this.compose(_zero, q, _one);
  },
  lookAt: function lookAt(eye, target, up) {
    var te = this.elements;

    _z.subVectors(eye, target);

    if (_z.lengthSq() === 0) {
      // eye and target are in the same position
      _z.z = 1;
    }

    _z.normalize();

    _x.crossVectors(up, _z);

    if (_x.lengthSq() === 0) {
      // up and z are parallel
      if (Math.abs(up.z) === 1) {
        _z.x += 0.0001;
      } else {
        _z.z += 0.0001;
      }

      _z.normalize();

      _x.crossVectors(up, _z);
    }

    _x.normalize();

    _y.crossVectors(_z, _x);

    te[0] = _x.x;
    te[4] = _y.x;
    te[8] = _z.x;
    te[1] = _x.y;
    te[5] = _y.y;
    te[9] = _z.y;
    te[2] = _x.z;
    te[6] = _y.z;
    te[10] = _z.z;
    return this;
  },
  multiply: function multiply(m, n) {
    if (n !== undefined) {
      console.warn('THREE.Matrix4: .multiply() now only accepts one argument. Use .multiplyMatrices( a, b ) instead.');
      return this.multiplyMatrices(m, n);
    }

    return this.multiplyMatrices(this, m);
  },
  premultiply: function premultiply(m) {
    return this.multiplyMatrices(m, this);
  },
  multiplyMatrices: function multiplyMatrices(a, b) {
    var ae = a.elements;
    var be = b.elements;
    var te = this.elements;
    var a11 = ae[0];
    var a12 = ae[4];
    var a13 = ae[8];
    var a14 = ae[12];
    var a21 = ae[1];
    var a22 = ae[5];
    var a23 = ae[9];
    var a24 = ae[13];
    var a31 = ae[2];
    var a32 = ae[6];
    var a33 = ae[10];
    var a34 = ae[14];
    var a41 = ae[3];
    var a42 = ae[7];
    var a43 = ae[11];
    var a44 = ae[15];
    var b11 = be[0];
    var b12 = be[4];
    var b13 = be[8];
    var b14 = be[12];
    var b21 = be[1];
    var b22 = be[5];
    var b23 = be[9];
    var b24 = be[13];
    var b31 = be[2];
    var b32 = be[6];
    var b33 = be[10];
    var b34 = be[14];
    var b41 = be[3];
    var b42 = be[7];
    var b43 = be[11];
    var b44 = be[15];
    te[0] = a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41;
    te[4] = a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42;
    te[8] = a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43;
    te[12] = a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44;
    te[1] = a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41;
    te[5] = a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42;
    te[9] = a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43;
    te[13] = a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44;
    te[2] = a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41;
    te[6] = a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42;
    te[10] = a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43;
    te[14] = a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44;
    te[3] = a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41;
    te[7] = a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42;
    te[11] = a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43;
    te[15] = a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44;
    return this;
  },
  multiplyScalar: function multiplyScalar(s) {
    var te = this.elements;
    te[0] *= s;
    te[4] *= s;
    te[8] *= s;
    te[12] *= s;
    te[1] *= s;
    te[5] *= s;
    te[9] *= s;
    te[13] *= s;
    te[2] *= s;
    te[6] *= s;
    te[10] *= s;
    te[14] *= s;
    te[3] *= s;
    te[7] *= s;
    te[11] *= s;
    te[15] *= s;
    return this;
  },
  determinant: function determinant() {
    var te = this.elements;
    var n11 = te[0];
    var n12 = te[4];
    var n13 = te[8];
    var n14 = te[12];
    var n21 = te[1];
    var n22 = te[5];
    var n23 = te[9];
    var n24 = te[13];
    var n31 = te[2];
    var n32 = te[6];
    var n33 = te[10];
    var n34 = te[14];
    var n41 = te[3];
    var n42 = te[7];
    var n43 = te[11];
    var n44 = te[15]; // TODO: make this more efficient
    // ( based on http://www.euclideanspace.com/maths/algebra/matrix/functions/inverse/fourD/index.htm )

    return n41 * (+n14 * n23 * n32 - n13 * n24 * n32 - n14 * n22 * n33 + n12 * n24 * n33 + n13 * n22 * n34 - n12 * n23 * n34) + n42 * (+n11 * n23 * n34 - n11 * n24 * n33 + n14 * n21 * n33 - n13 * n21 * n34 + n13 * n24 * n31 - n14 * n23 * n31) + n43 * (+n11 * n24 * n32 - n11 * n22 * n34 - n14 * n21 * n32 + n12 * n21 * n34 + n14 * n22 * n31 - n12 * n24 * n31) + n44 * (-n13 * n22 * n31 - n11 * n23 * n32 + n11 * n22 * n33 + n13 * n21 * n32 - n12 * n21 * n33 + n12 * n23 * n31);
  },
  transpose: function transpose() {
    var te = this.elements;
    var tmp;
    tmp = te[1];
    te[1] = te[4];
    te[4] = tmp;
    tmp = te[2];
    te[2] = te[8];
    te[8] = tmp;
    tmp = te[6];
    te[6] = te[9];
    te[9] = tmp;
    tmp = te[3];
    te[3] = te[12];
    te[12] = tmp;
    tmp = te[7];
    te[7] = te[13];
    te[13] = tmp;
    tmp = te[11];
    te[11] = te[14];
    te[14] = tmp;
    return this;
  },
  setPosition: function setPosition(x, y, z) {
    var te = this.elements;

    if (x.isVector3) {
      te[12] = x.x;
      te[13] = x.y;
      te[14] = x.z;
    } else {
      te[12] = x;
      te[13] = y;
      te[14] = z;
    }

    return this;
  },
  getInverse: function getInverse(m, throwOnDegenerate) {
    if (throwOnDegenerate !== undefined) {
      console.warn('THREE.Matrix4: .getInverse() can no longer be configured to throw on degenerate.');
    } // based on http://www.euclideanspace.com/maths/algebra/matrix/functions/inverse/fourD/index.htm


    var te = this.elements;
    var me = m.elements;
    var n11 = me[0];
    var n21 = me[1];
    var n31 = me[2];
    var n41 = me[3];
    var n12 = me[4];
    var n22 = me[5];
    var n32 = me[6];
    var n42 = me[7];
    var n13 = me[8];
    var n23 = me[9];
    var n33 = me[10];
    var n43 = me[11];
    var n14 = me[12];
    var n24 = me[13];
    var n34 = me[14];
    var n44 = me[15];
    var t11 = n23 * n34 * n42 - n24 * n33 * n42 + n24 * n32 * n43 - n22 * n34 * n43 - n23 * n32 * n44 + n22 * n33 * n44;
    var t12 = n14 * n33 * n42 - n13 * n34 * n42 - n14 * n32 * n43 + n12 * n34 * n43 + n13 * n32 * n44 - n12 * n33 * n44;
    var t13 = n13 * n24 * n42 - n14 * n23 * n42 + n14 * n22 * n43 - n12 * n24 * n43 - n13 * n22 * n44 + n12 * n23 * n44;
    var t14 = n14 * n23 * n32 - n13 * n24 * n32 - n14 * n22 * n33 + n12 * n24 * n33 + n13 * n22 * n34 - n12 * n23 * n34;
    var det = n11 * t11 + n21 * t12 + n31 * t13 + n41 * t14;
    if (det === 0) return this.set(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    var detInv = 1 / det;
    te[0] = t11 * detInv;
    te[1] = (n24 * n33 * n41 - n23 * n34 * n41 - n24 * n31 * n43 + n21 * n34 * n43 + n23 * n31 * n44 - n21 * n33 * n44) * detInv;
    te[2] = (n22 * n34 * n41 - n24 * n32 * n41 + n24 * n31 * n42 - n21 * n34 * n42 - n22 * n31 * n44 + n21 * n32 * n44) * detInv;
    te[3] = (n23 * n32 * n41 - n22 * n33 * n41 - n23 * n31 * n42 + n21 * n33 * n42 + n22 * n31 * n43 - n21 * n32 * n43) * detInv;
    te[4] = t12 * detInv;
    te[5] = (n13 * n34 * n41 - n14 * n33 * n41 + n14 * n31 * n43 - n11 * n34 * n43 - n13 * n31 * n44 + n11 * n33 * n44) * detInv;
    te[6] = (n14 * n32 * n41 - n12 * n34 * n41 - n14 * n31 * n42 + n11 * n34 * n42 + n12 * n31 * n44 - n11 * n32 * n44) * detInv;
    te[7] = (n12 * n33 * n41 - n13 * n32 * n41 + n13 * n31 * n42 - n11 * n33 * n42 - n12 * n31 * n43 + n11 * n32 * n43) * detInv;
    te[8] = t13 * detInv;
    te[9] = (n14 * n23 * n41 - n13 * n24 * n41 - n14 * n21 * n43 + n11 * n24 * n43 + n13 * n21 * n44 - n11 * n23 * n44) * detInv;
    te[10] = (n12 * n24 * n41 - n14 * n22 * n41 + n14 * n21 * n42 - n11 * n24 * n42 - n12 * n21 * n44 + n11 * n22 * n44) * detInv;
    te[11] = (n13 * n22 * n41 - n12 * n23 * n41 - n13 * n21 * n42 + n11 * n23 * n42 + n12 * n21 * n43 - n11 * n22 * n43) * detInv;
    te[12] = t14 * detInv;
    te[13] = (n13 * n24 * n31 - n14 * n23 * n31 + n14 * n21 * n33 - n11 * n24 * n33 - n13 * n21 * n34 + n11 * n23 * n34) * detInv;
    te[14] = (n14 * n22 * n31 - n12 * n24 * n31 - n14 * n21 * n32 + n11 * n24 * n32 + n12 * n21 * n34 - n11 * n22 * n34) * detInv;
    te[15] = (n12 * n23 * n31 - n13 * n22 * n31 + n13 * n21 * n32 - n11 * n23 * n32 - n12 * n21 * n33 + n11 * n22 * n33) * detInv;
    return this;
  },
  scale: function scale(v) {
    var te = this.elements;
    var x = v.x;
    var y = v.y;
    var z = v.z;
    te[0] *= x;
    te[4] *= y;
    te[8] *= z;
    te[1] *= x;
    te[5] *= y;
    te[9] *= z;
    te[2] *= x;
    te[6] *= y;
    te[10] *= z;
    te[3] *= x;
    te[7] *= y;
    te[11] *= z;
    return this;
  },
  getMaxScaleOnAxis: function getMaxScaleOnAxis() {
    var te = this.elements;
    var scaleXSq = te[0] * te[0] + te[1] * te[1] + te[2] * te[2];
    var scaleYSq = te[4] * te[4] + te[5] * te[5] + te[6] * te[6];
    var scaleZSq = te[8] * te[8] + te[9] * te[9] + te[10] * te[10];
    return Math.sqrt(Math.max(scaleXSq, scaleYSq, scaleZSq));
  },
  makeTranslation: function makeTranslation(x, y, z) {
    this.set(1, 0, 0, x, 0, 1, 0, y, 0, 0, 1, z, 0, 0, 0, 1);
    return this;
  },
  makeRotationX: function makeRotationX(theta) {
    var c = Math.cos(theta);
    var s = Math.sin(theta);
    this.set(1, 0, 0, 0, 0, c, -s, 0, 0, s, c, 0, 0, 0, 0, 1);
    return this;
  },
  makeRotationY: function makeRotationY(theta) {
    var c = Math.cos(theta);
    var s = Math.sin(theta);
    this.set(c, 0, s, 0, 0, 1, 0, 0, -s, 0, c, 0, 0, 0, 0, 1);
    return this;
  },
  makeRotationZ: function makeRotationZ(theta) {
    var c = Math.cos(theta);
    var s = Math.sin(theta);
    this.set(c, -s, 0, 0, s, c, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
    return this;
  },
  makeRotationAxis: function makeRotationAxis(axis, angle) {
    // Based on http://www.gamedev.net/reference/articles/article1199.asp
    var c = Math.cos(angle);
    var s = Math.sin(angle);
    var t = 1 - c;
    var x = axis.x;
    var y = axis.y;
    var z = axis.z;
    var tx = t * x;
    var ty = t * y;
    this.set(tx * x + c, tx * y - s * z, tx * z + s * y, 0, tx * y + s * z, ty * y + c, ty * z - s * x, 0, tx * z - s * y, ty * z + s * x, t * z * z + c, 0, 0, 0, 0, 1);
    return this;
  },
  makeScale: function makeScale(x, y, z) {
    this.set(x, 0, 0, 0, 0, y, 0, 0, 0, 0, z, 0, 0, 0, 0, 1);
    return this;
  },
  makeShear: function makeShear(x, y, z) {
    this.set(1, y, z, 0, x, 1, z, 0, x, y, 1, 0, 0, 0, 0, 1);
    return this;
  },
  compose: function compose(position, quaternion, scale) {
    var te = this.elements;
    var x = quaternion._x;
    var y = quaternion._y;
    var z = quaternion._z;
    var w = quaternion._w;
    var x2 = x + x;
    var y2 = y + y;
    var z2 = z + z;
    var xx = x * x2;
    var xy = x * y2;
    var xz = x * z2;
    var yy = y * y2;
    var yz = y * z2;
    var zz = z * z2;
    var wx = w * x2;
    var wy = w * y2;
    var wz = w * z2;
    var sx = scale.x;
    var sy = scale.y;
    var sz = scale.z;
    te[0] = (1 - (yy + zz)) * sx;
    te[1] = (xy + wz) * sx;
    te[2] = (xz - wy) * sx;
    te[3] = 0;
    te[4] = (xy - wz) * sy;
    te[5] = (1 - (xx + zz)) * sy;
    te[6] = (yz + wx) * sy;
    te[7] = 0;
    te[8] = (xz + wy) * sz;
    te[9] = (yz - wx) * sz;
    te[10] = (1 - (xx + yy)) * sz;
    te[11] = 0;
    te[12] = position.x;
    te[13] = position.y;
    te[14] = position.z;
    te[15] = 1;
    return this;
  },
  decompose: function decompose(position, quaternion, scale) {
    var te = this.elements;

    var sx = _v1.set(te[0], te[1], te[2]).length();

    var sy = _v1.set(te[4], te[5], te[6]).length();

    var sz = _v1.set(te[8], te[9], te[10]).length(); // if determine is negative, we need to invert one scale


    var det = this.determinant();
    if (det < 0) sx = -sx;
    position.x = te[12];
    position.y = te[13];
    position.z = te[14]; // scale the rotation part

    _m1.copy(this);

    var invSX = 1 / sx;
    var invSY = 1 / sy;
    var invSZ = 1 / sz;
    _m1.elements[0] *= invSX;
    _m1.elements[1] *= invSX;
    _m1.elements[2] *= invSX;
    _m1.elements[4] *= invSY;
    _m1.elements[5] *= invSY;
    _m1.elements[6] *= invSY;
    _m1.elements[8] *= invSZ;
    _m1.elements[9] *= invSZ;
    _m1.elements[10] *= invSZ;
    quaternion.setFromRotationMatrix(_m1);
    scale.x = sx;
    scale.y = sy;
    scale.z = sz;
    return this;
  },
  makePerspective: function makePerspective(left, right, top, bottom, near, far) {
    if (far === undefined) {
      console.warn('THREE.Matrix4: .makePerspective() has been redefined and has a new signature. Please check the docs.');
    }

    var te = this.elements;
    var x = 2 * near / (right - left);
    var y = 2 * near / (top - bottom);
    var a = (right + left) / (right - left);
    var b = (top + bottom) / (top - bottom);
    var c = -(far + near) / (far - near);
    var d = -2 * far * near / (far - near);
    te[0] = x;
    te[4] = 0;
    te[8] = a;
    te[12] = 0;
    te[1] = 0;
    te[5] = y;
    te[9] = b;
    te[13] = 0;
    te[2] = 0;
    te[6] = 0;
    te[10] = c;
    te[14] = d;
    te[3] = 0;
    te[7] = 0;
    te[11] = -1;
    te[15] = 0;
    return this;
  },
  makeOrthographic: function makeOrthographic(left, right, top, bottom, near, far) {
    var te = this.elements;
    var w = 1.0 / (right - left);
    var h = 1.0 / (top - bottom);
    var p = 1.0 / (far - near);
    var x = (right + left) * w;
    var y = (top + bottom) * h;
    var z = (far + near) * p;
    te[0] = 2 * w;
    te[4] = 0;
    te[8] = 0;
    te[12] = -x;
    te[1] = 0;
    te[5] = 2 * h;
    te[9] = 0;
    te[13] = -y;
    te[2] = 0;
    te[6] = 0;
    te[10] = -2 * p;
    te[14] = -z;
    te[3] = 0;
    te[7] = 0;
    te[11] = 0;
    te[15] = 1;
    return this;
  },
  equals: function equals(matrix) {
    var te = this.elements;
    var me = matrix.elements;

    for (var i = 0; i < 16; i++) {
      if (te[i] !== me[i]) return false;
    }

    return true;
  },
  fromArray: function fromArray(array, offset) {
    if (offset === undefined) offset = 0;

    for (var i = 0; i < 16; i++) {
      this.elements[i] = array[i + offset];
    }

    return this;
  },
  toArray: function toArray(array, offset) {
    if (array === undefined) array = [];
    if (offset === undefined) offset = 0;
    var te = this.elements;
    array[offset] = te[0];
    array[offset + 1] = te[1];
    array[offset + 2] = te[2];
    array[offset + 3] = te[3];
    array[offset + 4] = te[4];
    array[offset + 5] = te[5];
    array[offset + 6] = te[6];
    array[offset + 7] = te[7];
    array[offset + 8] = te[8];
    array[offset + 9] = te[9];
    array[offset + 10] = te[10];
    array[offset + 11] = te[11];
    array[offset + 12] = te[12];
    array[offset + 13] = te[13];
    array[offset + 14] = te[14];
    array[offset + 15] = te[15];
    return array;
  }
});

/**
 * @author mrdoob / http://mrdoob.com/
 * @author WestLangley / http://github.com/WestLangley
 * @author bhouston / http://clara.io
 */

var _matrix = new Matrix4();

var _quaternion$1 = new Quaternion();

function Euler() {
  var x = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
  var y = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
  var z = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;
  var order = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : Euler.DefaultOrder;
  this._x = x;
  this._y = y;
  this._z = z;
  this._order = order;
}

Euler.RotationOrders = ['XYZ', 'YZX', 'ZXY', 'XZY', 'YXZ', 'ZYX'];
Euler.DefaultOrder = 'XYZ';
Object.defineProperties(Euler.prototype, {
  x: {
    get: function get() {
      return this._x;
    },
    set: function set(value) {
      this._x = value;

      this._onChangeCallback();
    }
  },
  y: {
    get: function get() {
      return this._y;
    },
    set: function set(value) {
      this._y = value;

      this._onChangeCallback();
    }
  },
  z: {
    get: function get() {
      return this._z;
    },
    set: function set(value) {
      this._z = value;

      this._onChangeCallback();
    }
  },
  order: {
    get: function get() {
      return this._order;
    },
    set: function set(value) {
      this._order = value;

      this._onChangeCallback();
    }
  }
});
Object.assign(Euler.prototype, {
  isEuler: true,
  set: function set(x, y, z, order) {
    this._x = x;
    this._y = y;
    this._z = z;
    this._order = order || this._order;

    this._onChangeCallback();

    return this;
  },
  clone: function clone() {
    return new this.constructor(this._x, this._y, this._z, this._order);
  },
  copy: function copy(euler) {
    this._x = euler._x;
    this._y = euler._y;
    this._z = euler._z;
    this._order = euler._order;

    this._onChangeCallback();

    return this;
  },
  setFromRotationMatrix: function setFromRotationMatrix(m, order, update) {
    var clamp = MathUtils.clamp; // assumes the upper 3x3 of m is a pure rotation matrix (i.e, unscaled)

    var te = m.elements;
    var m11 = te[0];
    var m12 = te[4];
    var m13 = te[8];
    var m21 = te[1];
    var m22 = te[5];
    var m23 = te[9];
    var m31 = te[2];
    var m32 = te[6];
    var m33 = te[10];
    order = order || this._order;

    switch (order) {
      case 'XYZ':
        this._y = Math.asin(clamp(m13, -1, 1));

        if (Math.abs(m13) < 0.9999999) {
          this._x = Math.atan2(-m23, m33);
          this._z = Math.atan2(-m12, m11);
        } else {
          this._x = Math.atan2(m32, m22);
          this._z = 0;
        }

        break;

      case 'YXZ':
        this._x = Math.asin(-clamp(m23, -1, 1));

        if (Math.abs(m23) < 0.9999999) {
          this._y = Math.atan2(m13, m33);
          this._z = Math.atan2(m21, m22);
        } else {
          this._y = Math.atan2(-m31, m11);
          this._z = 0;
        }

        break;

      case 'ZXY':
        this._x = Math.asin(clamp(m32, -1, 1));

        if (Math.abs(m32) < 0.9999999) {
          this._y = Math.atan2(-m31, m33);
          this._z = Math.atan2(-m12, m22);
        } else {
          this._y = 0;
          this._z = Math.atan2(m21, m11);
        }

        break;

      case 'ZYX':
        this._y = Math.asin(-clamp(m31, -1, 1));

        if (Math.abs(m31) < 0.9999999) {
          this._x = Math.atan2(m32, m33);
          this._z = Math.atan2(m21, m11);
        } else {
          this._x = 0;
          this._z = Math.atan2(-m12, m22);
        }

        break;

      case 'YZX':
        this._z = Math.asin(clamp(m21, -1, 1));

        if (Math.abs(m21) < 0.9999999) {
          this._x = Math.atan2(-m23, m22);
          this._y = Math.atan2(-m31, m11);
        } else {
          this._x = 0;
          this._y = Math.atan2(m13, m33);
        }

        break;

      case 'XZY':
        this._z = Math.asin(-clamp(m12, -1, 1));

        if (Math.abs(m12) < 0.9999999) {
          this._x = Math.atan2(m32, m22);
          this._y = Math.atan2(m13, m11);
        } else {
          this._x = Math.atan2(-m23, m33);
          this._y = 0;
        }

        break;

      default:
        console.warn("THREE.Euler: .setFromRotationMatrix() encountered an unknown order: ".concat(order));
    }

    this._order = order;
    if (update !== false) this._onChangeCallback();
    return this;
  },
  setFromQuaternion: function setFromQuaternion(q, order, update) {
    _matrix.makeRotationFromQuaternion(q);

    return this.setFromRotationMatrix(_matrix, order, update);
  },
  setFromVector3: function setFromVector3(v, order) {
    return this.set(v.x, v.y, v.z, order || this._order);
  },
  reorder: function reorder(newOrder) {
    // WARNING: this discards revolution information -bhouston
    _quaternion$1.setFromEuler(this);

    return this.setFromQuaternion(_quaternion$1, newOrder);
  },
  equals: function equals(euler) {
    return euler._x === this._x && euler._y === this._y && euler._z === this._z && euler._order === this._order;
  },
  fromArray: function fromArray(array) {
    this._x = array[0];
    this._y = array[1];
    this._z = array[2];
    if (array[3] !== undefined) this._order = array[3];

    this._onChangeCallback();

    return this;
  },
  toArray: function toArray(array, offset) {
    if (array === undefined) array = [];
    if (offset === undefined) offset = 0;
    array[offset] = this._x;
    array[offset + 1] = this._y;
    array[offset + 2] = this._z;
    array[offset + 3] = this._order;
    return array;
  },
  toVector3: function toVector3(optionalResult) {
    if (optionalResult) {
      return optionalResult.set(this._x, this._y, this._z);
    }

    return new Vector3(this._x, this._y, this._z);
  },
  _onChange: function _onChange(callback) {
    this._onChangeCallback = callback;
    return this;
  },
  _onChangeCallback: function _onChangeCallback() {}
});

/**
 * @author mrdoob / http://mrdoob.com/
 * @author philogb / http://blog.thejit.org/
 * @author egraether / http://egraether.com/
 * @author zz85 / http://www.lab4games.net/zz85/blog
 */
function Vector2() {
  var x = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
  var y = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
  this.x = x;
  this.y = y;
}

Object.defineProperties(Vector2.prototype, {
  width: {
    get: function get() {
      return this.x;
    },
    set: function set(value) {
      this.x = value;
    }
  },
  height: {
    get: function get() {
      return this.y;
    },
    set: function set(value) {
      this.y = value;
    }
  }
});
Object.assign(Vector2.prototype, {
  isVector2: true,
  set: function set(x, y) {
    this.x = x;
    this.y = y;
    return this;
  },
  setScalar: function setScalar(scalar) {
    this.x = scalar;
    this.y = scalar;
    return this;
  },
  setX: function setX(x) {
    this.x = x;
    return this;
  },
  setY: function setY(y) {
    this.y = y;
    return this;
  },
  setComponent: function setComponent(index, value) {
    switch (index) {
      case 0:
        this.x = value;
        break;

      case 1:
        this.y = value;
        break;

      default:
        throw new Error("index is out of range: ".concat(index));
    }

    return this;
  },
  getComponent: function getComponent(index) {
    switch (index) {
      case 0:
        return this.x;

      case 1:
        return this.y;

      default:
        throw new Error("index is out of range: ".concat(index));
    }
  },
  clone: function clone() {
    return new this.constructor(this.x, this.y);
  },
  copy: function copy(v) {
    this.x = v.x;
    this.y = v.y;
    return this;
  },
  add: function add(v, w) {
    if (w !== undefined) {
      console.warn('THREE.Vector2: .add() now only accepts one argument. Use .addVectors( a, b ) instead.');
      return this.addVectors(v, w);
    }

    this.x += v.x;
    this.y += v.y;
    return this;
  },
  addScalar: function addScalar(s) {
    this.x += s;
    this.y += s;
    return this;
  },
  addVectors: function addVectors(a, b) {
    this.x = a.x + b.x;
    this.y = a.y + b.y;
    return this;
  },
  addScaledVector: function addScaledVector(v, s) {
    this.x += v.x * s;
    this.y += v.y * s;
    return this;
  },
  sub: function sub(v, w) {
    if (w !== undefined) {
      console.warn('THREE.Vector2: .sub() now only accepts one argument. Use .subVectors( a, b ) instead.');
      return this.subVectors(v, w);
    }

    this.x -= v.x;
    this.y -= v.y;
    return this;
  },
  subScalar: function subScalar(s) {
    this.x -= s;
    this.y -= s;
    return this;
  },
  subVectors: function subVectors(a, b) {
    this.x = a.x - b.x;
    this.y = a.y - b.y;
    return this;
  },
  multiply: function multiply(v) {
    this.x *= v.x;
    this.y *= v.y;
    return this;
  },
  multiplyScalar: function multiplyScalar(scalar) {
    this.x *= scalar;
    this.y *= scalar;
    return this;
  },
  divide: function divide(v) {
    this.x /= v.x;
    this.y /= v.y;
    return this;
  },
  divideScalar: function divideScalar(scalar) {
    return this.multiplyScalar(1 / scalar);
  },
  applyMatrix3: function applyMatrix3(m) {
    var x = this.x;
    var y = this.y;
    var e = m.elements;
    this.x = e[0] * x + e[3] * y + e[6];
    this.y = e[1] * x + e[4] * y + e[7];
    return this;
  },
  min: function min(v) {
    this.x = Math.min(this.x, v.x);
    this.y = Math.min(this.y, v.y);
    return this;
  },
  max: function max(v) {
    this.x = Math.max(this.x, v.x);
    this.y = Math.max(this.y, v.y);
    return this;
  },
  clamp: function clamp(min, max) {
    // assumes min < max, componentwise
    this.x = Math.max(min.x, Math.min(max.x, this.x));
    this.y = Math.max(min.y, Math.min(max.y, this.y));
    return this;
  },
  clampScalar: function clampScalar(minVal, maxVal) {
    this.x = Math.max(minVal, Math.min(maxVal, this.x));
    this.y = Math.max(minVal, Math.min(maxVal, this.y));
    return this;
  },
  clampLength: function clampLength(min, max) {
    var length = this.length();
    return this.divideScalar(length || 1).multiplyScalar(Math.max(min, Math.min(max, length)));
  },
  floor: function floor() {
    this.x = Math.floor(this.x);
    this.y = Math.floor(this.y);
    return this;
  },
  ceil: function ceil() {
    this.x = Math.ceil(this.x);
    this.y = Math.ceil(this.y);
    return this;
  },
  round: function round() {
    this.x = Math.round(this.x);
    this.y = Math.round(this.y);
    return this;
  },
  roundToZero: function roundToZero() {
    this.x = this.x < 0 ? Math.ceil(this.x) : Math.floor(this.x);
    this.y = this.y < 0 ? Math.ceil(this.y) : Math.floor(this.y);
    return this;
  },
  negate: function negate() {
    this.x = -this.x;
    this.y = -this.y;
    return this;
  },
  dot: function dot(v) {
    return this.x * v.x + this.y * v.y;
  },
  cross: function cross(v) {
    return this.x * v.y - this.y * v.x;
  },
  lengthSq: function lengthSq() {
    return this.x * this.x + this.y * this.y;
  },
  length: function length() {
    return Math.sqrt(this.x * this.x + this.y * this.y);
  },
  manhattanLength: function manhattanLength() {
    return Math.abs(this.x) + Math.abs(this.y);
  },
  normalize: function normalize() {
    return this.divideScalar(this.length() || 1);
  },
  angle: function angle() {
    // computes the angle in radians with respect to the positive x-axis
    var angle1 = Math.atan2(-this.y, -this.x) + Math.PI;
    return angle1;
  },
  distanceTo: function distanceTo(v) {
    return Math.sqrt(this.distanceToSquared(v));
  },
  distanceToSquared: function distanceToSquared(v) {
    var dx = this.x - v.x;
    var dy = this.y - v.y;
    return dx * dx + dy * dy;
  },
  manhattanDistanceTo: function manhattanDistanceTo(v) {
    return Math.abs(this.x - v.x) + Math.abs(this.y - v.y);
  },
  setLength: function setLength(length) {
    return this.normalize().multiplyScalar(length);
  },
  lerp: function lerp(v, alpha) {
    this.x += (v.x - this.x) * alpha;
    this.y += (v.y - this.y) * alpha;
    return this;
  },
  lerpVectors: function lerpVectors(v1, v2, alpha) {
    this.x = v1.x + (v2.x - v1.x) * alpha;
    this.y = v1.y + (v2.y - v1.y) * alpha;
    return this;
  },
  equals: function equals(v) {
    return v.x === this.x && v.y === this.y;
  },
  fromArray: function fromArray(array, offset) {
    if (offset === undefined) offset = 0;
    this.x = array[offset];
    this.y = array[offset + 1];
    return this;
  },
  toArray: function toArray(array, offset) {
    if (array === undefined) array = [];
    if (offset === undefined) offset = 0;
    array[offset] = this.x;
    array[offset + 1] = this.y;
    return array;
  },
  fromBufferAttribute: function fromBufferAttribute(attribute, index, offset) {
    if (offset !== undefined) {
      console.warn('THREE.Vector2: offset has been removed from .fromBufferAttribute().');
    }

    this.x = attribute.getX(index);
    this.y = attribute.getY(index);
    return this;
  },
  rotateAround: function rotateAround(center, angle) {
    var c = Math.cos(angle);
    var s = Math.sin(angle);
    var x = this.x - center.x;
    var y = this.y - center.y;
    this.x = x * c - y * s + center.x;
    this.y = x * s + y * c + center.y;
    return this;
  },
  random: function random() {
    this.x = Math.random();
    this.y = Math.random();
    return this;
  }
});

var constant = function(x){
  return function(){
    return x;
  }
};

function linear(a, d) {
  return function(t) {
    return a + t * d;
  };
}

function exponential(a, b, y) {
  return a = Math.pow(a, y), b = Math.pow(b, y) - a, y = 1 / y, function(t) {
    return Math.pow(a + t * b, y);
  };
}

function gamma(y) {
  return (y = +y) === 1 ? nogamma : function(a, b) {
    return b - a ? exponential(a, b, y) : constant(isNaN(a) ? b : a);
  };
}

function nogamma(a, b) {
  var d = b - a;
  return d ? linear(a, d) : constant(isNaN(a) ? b : a);
}

function define(constructor, factory, prototype) {
  constructor.prototype = factory.prototype = prototype;
  prototype.constructor = constructor;
}

function extend(parent, definition) {
  var prototype = Object.create(parent.prototype);
  for (var key in definition) prototype[key] = definition[key];
  return prototype;
}

function Color() {}

var darker = 0.7;
var brighter = 1 / darker;

var reI = "\\s*([+-]?\\d+)\\s*",
    reN = "\\s*([+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)\\s*",
    reP = "\\s*([+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)%\\s*",
    reHex = /^#([0-9a-f]{3,8})$/,
    reRgbInteger = new RegExp("^rgb\\(" + [reI, reI, reI] + "\\)$"),
    reRgbPercent = new RegExp("^rgb\\(" + [reP, reP, reP] + "\\)$"),
    reRgbaInteger = new RegExp("^rgba\\(" + [reI, reI, reI, reN] + "\\)$"),
    reRgbaPercent = new RegExp("^rgba\\(" + [reP, reP, reP, reN] + "\\)$"),
    reHslPercent = new RegExp("^hsl\\(" + [reN, reP, reP] + "\\)$"),
    reHslaPercent = new RegExp("^hsla\\(" + [reN, reP, reP, reN] + "\\)$");

var named = {
  aliceblue: 0xf0f8ff,
  antiquewhite: 0xfaebd7,
  aqua: 0x00ffff,
  aquamarine: 0x7fffd4,
  azure: 0xf0ffff,
  beige: 0xf5f5dc,
  bisque: 0xffe4c4,
  black: 0x000000,
  blanchedalmond: 0xffebcd,
  blue: 0x0000ff,
  blueviolet: 0x8a2be2,
  brown: 0xa52a2a,
  burlywood: 0xdeb887,
  cadetblue: 0x5f9ea0,
  chartreuse: 0x7fff00,
  chocolate: 0xd2691e,
  coral: 0xff7f50,
  cornflowerblue: 0x6495ed,
  cornsilk: 0xfff8dc,
  crimson: 0xdc143c,
  cyan: 0x00ffff,
  darkblue: 0x00008b,
  darkcyan: 0x008b8b,
  darkgoldenrod: 0xb8860b,
  darkgray: 0xa9a9a9,
  darkgreen: 0x006400,
  darkgrey: 0xa9a9a9,
  darkkhaki: 0xbdb76b,
  darkmagenta: 0x8b008b,
  darkolivegreen: 0x556b2f,
  darkorange: 0xff8c00,
  darkorchid: 0x9932cc,
  darkred: 0x8b0000,
  darksalmon: 0xe9967a,
  darkseagreen: 0x8fbc8f,
  darkslateblue: 0x483d8b,
  darkslategray: 0x2f4f4f,
  darkslategrey: 0x2f4f4f,
  darkturquoise: 0x00ced1,
  darkviolet: 0x9400d3,
  deeppink: 0xff1493,
  deepskyblue: 0x00bfff,
  dimgray: 0x696969,
  dimgrey: 0x696969,
  dodgerblue: 0x1e90ff,
  firebrick: 0xb22222,
  floralwhite: 0xfffaf0,
  forestgreen: 0x228b22,
  fuchsia: 0xff00ff,
  gainsboro: 0xdcdcdc,
  ghostwhite: 0xf8f8ff,
  gold: 0xffd700,
  goldenrod: 0xdaa520,
  gray: 0x808080,
  green: 0x008000,
  greenyellow: 0xadff2f,
  grey: 0x808080,
  honeydew: 0xf0fff0,
  hotpink: 0xff69b4,
  indianred: 0xcd5c5c,
  indigo: 0x4b0082,
  ivory: 0xfffff0,
  khaki: 0xf0e68c,
  lavender: 0xe6e6fa,
  lavenderblush: 0xfff0f5,
  lawngreen: 0x7cfc00,
  lemonchiffon: 0xfffacd,
  lightblue: 0xadd8e6,
  lightcoral: 0xf08080,
  lightcyan: 0xe0ffff,
  lightgoldenrodyellow: 0xfafad2,
  lightgray: 0xd3d3d3,
  lightgreen: 0x90ee90,
  lightgrey: 0xd3d3d3,
  lightpink: 0xffb6c1,
  lightsalmon: 0xffa07a,
  lightseagreen: 0x20b2aa,
  lightskyblue: 0x87cefa,
  lightslategray: 0x778899,
  lightslategrey: 0x778899,
  lightsteelblue: 0xb0c4de,
  lightyellow: 0xffffe0,
  lime: 0x00ff00,
  limegreen: 0x32cd32,
  linen: 0xfaf0e6,
  magenta: 0xff00ff,
  maroon: 0x800000,
  mediumaquamarine: 0x66cdaa,
  mediumblue: 0x0000cd,
  mediumorchid: 0xba55d3,
  mediumpurple: 0x9370db,
  mediumseagreen: 0x3cb371,
  mediumslateblue: 0x7b68ee,
  mediumspringgreen: 0x00fa9a,
  mediumturquoise: 0x48d1cc,
  mediumvioletred: 0xc71585,
  midnightblue: 0x191970,
  mintcream: 0xf5fffa,
  mistyrose: 0xffe4e1,
  moccasin: 0xffe4b5,
  navajowhite: 0xffdead,
  navy: 0x000080,
  oldlace: 0xfdf5e6,
  olive: 0x808000,
  olivedrab: 0x6b8e23,
  orange: 0xffa500,
  orangered: 0xff4500,
  orchid: 0xda70d6,
  palegoldenrod: 0xeee8aa,
  palegreen: 0x98fb98,
  paleturquoise: 0xafeeee,
  palevioletred: 0xdb7093,
  papayawhip: 0xffefd5,
  peachpuff: 0xffdab9,
  peru: 0xcd853f,
  pink: 0xffc0cb,
  plum: 0xdda0dd,
  powderblue: 0xb0e0e6,
  purple: 0x800080,
  rebeccapurple: 0x663399,
  red: 0xff0000,
  rosybrown: 0xbc8f8f,
  royalblue: 0x4169e1,
  saddlebrown: 0x8b4513,
  salmon: 0xfa8072,
  sandybrown: 0xf4a460,
  seagreen: 0x2e8b57,
  seashell: 0xfff5ee,
  sienna: 0xa0522d,
  silver: 0xc0c0c0,
  skyblue: 0x87ceeb,
  slateblue: 0x6a5acd,
  slategray: 0x708090,
  slategrey: 0x708090,
  snow: 0xfffafa,
  springgreen: 0x00ff7f,
  steelblue: 0x4682b4,
  tan: 0xd2b48c,
  teal: 0x008080,
  thistle: 0xd8bfd8,
  tomato: 0xff6347,
  turquoise: 0x40e0d0,
  violet: 0xee82ee,
  wheat: 0xf5deb3,
  white: 0xffffff,
  whitesmoke: 0xf5f5f5,
  yellow: 0xffff00,
  yellowgreen: 0x9acd32
};

define(Color, color, {
  copy: function(channels) {
    return Object.assign(new this.constructor, this, channels);
  },
  displayable: function() {
    return this.rgb().displayable();
  },
  hex: color_formatHex, // Deprecated! Use color.formatHex.
  formatHex: color_formatHex,
  formatHsl: color_formatHsl,
  formatRgb: color_formatRgb,
  toString: color_formatRgb
});

function color_formatHex() {
  return this.rgb().formatHex();
}

function color_formatHsl() {
  return hslConvert(this).formatHsl();
}

function color_formatRgb() {
  return this.rgb().formatRgb();
}

function color(format) {
  var m, l;
  format = (format + "").trim().toLowerCase();
  return (m = reHex.exec(format)) ? (l = m[1].length, m = parseInt(m[1], 16), l === 6 ? rgbn(m) // #ff0000
      : l === 3 ? new Rgb((m >> 8 & 0xf) | (m >> 4 & 0xf0), (m >> 4 & 0xf) | (m & 0xf0), ((m & 0xf) << 4) | (m & 0xf), 1) // #f00
      : l === 8 ? rgba(m >> 24 & 0xff, m >> 16 & 0xff, m >> 8 & 0xff, (m & 0xff) / 0xff) // #ff000000
      : l === 4 ? rgba((m >> 12 & 0xf) | (m >> 8 & 0xf0), (m >> 8 & 0xf) | (m >> 4 & 0xf0), (m >> 4 & 0xf) | (m & 0xf0), (((m & 0xf) << 4) | (m & 0xf)) / 0xff) // #f000
      : null) // invalid hex
      : (m = reRgbInteger.exec(format)) ? new Rgb(m[1], m[2], m[3], 1) // rgb(255, 0, 0)
      : (m = reRgbPercent.exec(format)) ? new Rgb(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, 1) // rgb(100%, 0%, 0%)
      : (m = reRgbaInteger.exec(format)) ? rgba(m[1], m[2], m[3], m[4]) // rgba(255, 0, 0, 1)
      : (m = reRgbaPercent.exec(format)) ? rgba(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, m[4]) // rgb(100%, 0%, 0%, 1)
      : (m = reHslPercent.exec(format)) ? hsla(m[1], m[2] / 100, m[3] / 100, 1) // hsl(120, 50%, 50%)
      : (m = reHslaPercent.exec(format)) ? hsla(m[1], m[2] / 100, m[3] / 100, m[4]) // hsla(120, 50%, 50%, 1)
      : named.hasOwnProperty(format) ? rgbn(named[format]) // eslint-disable-line no-prototype-builtins
      : format === "transparent" ? new Rgb(NaN, NaN, NaN, 0)
      : null;
}

function rgbn(n) {
  return new Rgb(n >> 16 & 0xff, n >> 8 & 0xff, n & 0xff, 1);
}

function rgba(r, g, b, a) {
  if (a <= 0) r = g = b = NaN;
  return new Rgb(r, g, b, a);
}

function rgbConvert(o) {
  if (!(o instanceof Color)) o = color(o);
  if (!o) return new Rgb;
  o = o.rgb();
  return new Rgb(o.r, o.g, o.b, o.opacity);
}

function rgb(r, g, b, opacity) {
  return arguments.length === 1 ? rgbConvert(r) : new Rgb(r, g, b, opacity == null ? 1 : opacity);
}

function Rgb(r, g, b, opacity) {
  this.r = +r;
  this.g = +g;
  this.b = +b;
  this.opacity = +opacity;
}

define(Rgb, rgb, extend(Color, {
  brighter: function(k) {
    k = k == null ? brighter : Math.pow(brighter, k);
    return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
  },
  darker: function(k) {
    k = k == null ? darker : Math.pow(darker, k);
    return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
  },
  rgb: function() {
    return this;
  },
  displayable: function() {
    return (-0.5 <= this.r && this.r < 255.5)
        && (-0.5 <= this.g && this.g < 255.5)
        && (-0.5 <= this.b && this.b < 255.5)
        && (0 <= this.opacity && this.opacity <= 1);
  },
  hex: rgb_formatHex, // Deprecated! Use color.formatHex.
  formatHex: rgb_formatHex,
  formatRgb: rgb_formatRgb,
  toString: rgb_formatRgb
}));

function rgb_formatHex() {
  return "#" + hex(this.r) + hex(this.g) + hex(this.b);
}

function rgb_formatRgb() {
  var a = this.opacity; a = isNaN(a) ? 1 : Math.max(0, Math.min(1, a));
  return (a === 1 ? "rgb(" : "rgba(")
      + Math.max(0, Math.min(255, Math.round(this.r) || 0)) + ", "
      + Math.max(0, Math.min(255, Math.round(this.g) || 0)) + ", "
      + Math.max(0, Math.min(255, Math.round(this.b) || 0))
      + (a === 1 ? ")" : ", " + a + ")");
}

function hex(value) {
  value = Math.max(0, Math.min(255, Math.round(value) || 0));
  return (value < 16 ? "0" : "") + value.toString(16);
}

function hsla(h, s, l, a) {
  if (a <= 0) h = s = l = NaN;
  else if (l <= 0 || l >= 1) h = s = NaN;
  else if (s <= 0) h = NaN;
  return new Hsl(h, s, l, a);
}

function hslConvert(o) {
  if (o instanceof Hsl) return new Hsl(o.h, o.s, o.l, o.opacity);
  if (!(o instanceof Color)) o = color(o);
  if (!o) return new Hsl;
  if (o instanceof Hsl) return o;
  o = o.rgb();
  var r = o.r / 255,
      g = o.g / 255,
      b = o.b / 255,
      min = Math.min(r, g, b),
      max = Math.max(r, g, b),
      h = NaN,
      s = max - min,
      l = (max + min) / 2;
  if (s) {
    if (r === max) h = (g - b) / s + (g < b) * 6;
    else if (g === max) h = (b - r) / s + 2;
    else h = (r - g) / s + 4;
    s /= l < 0.5 ? max + min : 2 - max - min;
    h *= 60;
  } else {
    s = l > 0 && l < 1 ? 0 : h;
  }
  return new Hsl(h, s, l, o.opacity);
}

function hsl(h, s, l, opacity) {
  return arguments.length === 1 ? hslConvert(h) : new Hsl(h, s, l, opacity == null ? 1 : opacity);
}

function Hsl(h, s, l, opacity) {
  this.h = +h;
  this.s = +s;
  this.l = +l;
  this.opacity = +opacity;
}

define(Hsl, hsl, extend(Color, {
  brighter: function(k) {
    k = k == null ? brighter : Math.pow(brighter, k);
    return new Hsl(this.h, this.s, this.l * k, this.opacity);
  },
  darker: function(k) {
    k = k == null ? darker : Math.pow(darker, k);
    return new Hsl(this.h, this.s, this.l * k, this.opacity);
  },
  rgb: function() {
    var h = this.h % 360 + (this.h < 0) * 360,
        s = isNaN(h) || isNaN(this.s) ? 0 : this.s,
        l = this.l,
        m2 = l + (l < 0.5 ? l : 1 - l) * s,
        m1 = 2 * l - m2;
    return new Rgb(
      hsl2rgb(h >= 240 ? h - 240 : h + 120, m1, m2),
      hsl2rgb(h, m1, m2),
      hsl2rgb(h < 120 ? h + 240 : h - 120, m1, m2),
      this.opacity
    );
  },
  displayable: function() {
    return (0 <= this.s && this.s <= 1 || isNaN(this.s))
        && (0 <= this.l && this.l <= 1)
        && (0 <= this.opacity && this.opacity <= 1);
  },
  formatHsl: function() {
    var a = this.opacity; a = isNaN(a) ? 1 : Math.max(0, Math.min(1, a));
    return (a === 1 ? "hsl(" : "hsla(")
        + (this.h || 0) + ", "
        + (this.s || 0) * 100 + "%, "
        + (this.l || 0) * 100 + "%"
        + (a === 1 ? ")" : ", " + a + ")");
  }
}));

/* From FvD 13.37, CSS Color Module Level 3 */
function hsl2rgb(h, m1, m2) {
  return (h < 60 ? m1 + (m2 - m1) * h / 60
      : h < 180 ? m2
      : h < 240 ? m1 + (m2 - m1) * (240 - h) / 60
      : m1) * 255;
}

var interpolateRgb = (function rgbGamma(y) {
  var color = gamma(y);

  function rgb$1(start, end) {
    var r = color((start = rgb(start)).r, (end = rgb(end)).r),
        g = color(start.g, end.g),
        b = color(start.b, end.b),
        opacity = nogamma(start.opacity, end.opacity);
    return function(t) {
      start.r = r(t);
      start.g = g(t);
      start.b = b(t);
      start.opacity = opacity(t);
      return start + "";
    };
  }

  rgb$1.gamma = rgbGamma;

  return rgb$1;
})(1);

/**
 * @param keyframesValue "255,255,0,50" 表示rgba
 * @return rgba(255, 255, 0, 0.5)
 */
/**
 * @param hex "#fbafff" 表示16进制
 * @return rgba(251,175,255,1)
 */
function transformHexToRgba(hex) {
    hex = hex.replace('#', '0x');
    var colorNum = Number(hex);
    var r = (colorNum >>> 24);
    var g = (colorNum >>> 16) & 0x00ff;
    var b = (colorNum >>> 8) & 0x0000ff;
    var a = colorNum & 0x000000ff;
    return "rgba(" + r + "," + g + "," + b + "," + a / 255.0 + ")";
}
/**
 * @param hex "#fbafff" 表示16进制
 * @return { r: 251, g: 175, b: 255, a: 100 }
 */
function transformHexToRgbaObj(hex) {
    hex = hex.replace('#', '0x');
    var colorNum = Number(hex);
    var r = (colorNum >>> 24);
    var g = (colorNum >>> 16) & 0x00ff;
    var b = (colorNum >>> 8) & 0x0000ff;
    var a = (colorNum & 0x000000ff) / 255.0 * 100;
    return { r: r, g: g, b: b, a: a };
}
/** rgba(0, 233, 250, 0.9448125164992826)
 * @param rgba rgba(251, 175, 255, 0.5)
 * @return { r: 251, g: 175, b: 255, a: 100 }
 */
function transformRgbaToRgbaObj(rgba) {
    var match = rgba.match(/rgba?\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*([\d\.]+))?\s*\)/);
    if (!match)
        return { r: 0, g: 0, b: 0, a: 0 };
    var _a = [Number(match[1]), Number(match[2]), Number(match[3]), Number(match[4])], r = _a[0], g = _a[1], b = _a[2], a = _a[3];
    return { r: r, g: g, b: b, a: isNaN(a) ? 100 : a * 100 };
}

var InterpolateColor = /** @class */ (function () {
    function InterpolateColor(beginValue, endValue) {
        this.startColor = transformHexToRgba(beginValue);
        this.endColor = transformHexToRgba(endValue);
        this.interpolator = new interpolateRgb(this.startColor, this.endColor);
    }
    InterpolateColor.prototype.value = function (time) {
        var value = this.interpolator(time);
        return transformRgbaToRgbaObj(value);
    };
    return InterpolateColor;
}());

/**
 * Bezier Curves formulas obtained from
 * http://en.wikipedia.org/wiki/Bézier_curve
 */
function CubicBezierP0(t, p) {
    var k = 1 - t;
    return k * k * k * p;
}
function CubicBezierP1(t, p) {
    var k = 1 - t;
    return 3 * k * k * t * p;
}
function CubicBezierP2(t, p) {
    return 3 * (1 - t) * t * t * p;
}
function CubicBezierP3(t, p) {
    return t * t * t * p;
}
function CubicBezier(t, p0, p1, p2, p3) {
    return (CubicBezierP0(t, p0)
        + CubicBezierP1(t, p1)
        + CubicBezierP2(t, p2)
        + CubicBezierP3(t, p3));
}
var Bezier = /** @class */ (function () {
    function Bezier(c1, c2, c3, c4) {
        this.c1 = 0.0;
        this.c2 = 0.0;
        this.c3 = 0.0;
        this.c4 = 0.0;
        this.c1 = c1;
        this.c2 = c2;
        this.c3 = c3;
        this.c4 = c4;
    }
    Bezier.prototype.evaluate = function (t) {
        return CubicBezier(t, this.c1, this.c2, this.c3, this.c4);
    };
    return Bezier;
}());

var InterpolateBezier = /** @class */ (function () {
    function InterpolateBezier(startValue, endValue) {
        this.startValue = startValue;
        this.endValue = endValue;
        var inter = endValue - startValue;
        var c1 = this.startValue;
        var c2 = this.startValue + (inter) * 0.25;
        var c3 = this.startValue + (inter) * 0.75;
        var c4 = this.endValue;
        this.interpolator = new Bezier(c1, c2, c3, c4);
    }
    InterpolateBezier.prototype.value = function (time) {
        return this.interpolator.evaluate(time);
    };
    return InterpolateBezier;
}());

/**
 * Abstract base class of interpolants over parametric samples.
 *
 * The parameter domain is one dimensional, typically the time or a path
 * along a curve defined by the data.
 *
 * The sample values can have any dimensionality and derived classes may
 * apply special interpretations to the data.
 *
 * This class provides the interval seek in a Template Method, deferring
 * the actual interpolation to derived classes.
 *
 * Time complexity is O(1) for linear access crossing at most two points
 * and O(log N) for random access, where N is the number of positions.
 *
 * References:
 *
 * 		http://www.oodesign.com/template-method-pattern.html
 *
 * @author tschw
 */
function Interpolant(parameterPositions, sampleValues, sampleSize, resultBuffer) {
  this.parameterPositions = parameterPositions;
  this._cachedIndex = 0;
  this.resultBuffer = resultBuffer !== undefined ? resultBuffer : new sampleValues.constructor(sampleSize);
  this.sampleValues = sampleValues;
  this.valueSize = sampleSize;
}

Object.assign(Interpolant.prototype, {
  evaluate: function evaluate(t) {
    var pp = this.parameterPositions;
    var i1 = this._cachedIndex;
    var t1 = pp[i1];
    var t0 = pp[i1 - 1];

    validate_interval: {
      seek: {
        var right;

        linear_scan: {
          // - See http://jsperf.com/comparison-to-undefined/3
          // - slower code:
          // -
          // - 				if ( t >= t1 || t1 === undefined ) {
          forward_scan: if (!(t < t1)) {
            for (var giveUpAt = i1 + 2;;) {
              if (t1 === undefined) {
                if (t < t0) break forward_scan; // after end

                i1 = pp.length;
                this._cachedIndex = i1;
                return this.afterEnd_(i1 - 1, t, t0);
              }

              if (i1 === giveUpAt) break; // this loop

              t0 = t1;
              t1 = pp[++i1];

              if (t < t1) {
                // we have arrived at the sought interval
                break seek;
              }
            } // prepare binary search on the right side of the index


            right = pp.length;
            break linear_scan;
          } // - slower code:
          // -					if ( t < t0 || t0 === undefined ) {


          if (!(t >= t0)) {
            // looping?
            var t1global = pp[1];

            if (t < t1global) {
              i1 = 2; // + 1, using the scan for the details

              t0 = t1global;
            } // linear reverse scan


            for (var _giveUpAt = i1 - 2;;) {
              if (t0 === undefined) {
                // before start
                this._cachedIndex = 0;
                return this.beforeStart_(0, t, t1);
              }

              if (i1 === _giveUpAt) break; // this loop

              t1 = t0;
              t0 = pp[--i1 - 1];

              if (t >= t0) {
                // we have arrived at the sought interval
                break seek;
              }
            } // prepare binary search on the left side of the index


            right = i1;
            i1 = 0;
            break linear_scan;
          } // the interval is valid


          break validate_interval;
        } // linear scan
        // binary search


        while (i1 < right) {
          var mid = i1 + right >>> 1;

          if (t < pp[mid]) {
            right = mid;
          } else {
            i1 = mid + 1;
          }
        }

        t1 = pp[i1];
        t0 = pp[i1 - 1]; // check boundary cases, again

        if (t0 === undefined) {
          this._cachedIndex = 0;
          return this.beforeStart_(0, t, t1);
        }

        if (t1 === undefined) {
          i1 = pp.length;
          this._cachedIndex = i1;
          return this.afterEnd_(i1 - 1, t0, t);
        }
      } // seek


      this._cachedIndex = i1;
      this.intervalChanged_(i1, t0, t1);
    } // validate_interval


    return this.interpolate_(i1, t0, t, t1);
  },
  settings: null,
  // optional, subclass-specific settings structure
  // Note: The indirection allows central control of many interpolants.
  // --- Protected interface
  DefaultSettings_: {},
  getSettings_: function getSettings_() {
    return this.settings || this.DefaultSettings_;
  },
  copySampleValue_: function copySampleValue_(index) {
    // copies a sample value to the result buffer
    var result = this.resultBuffer;
    var values = this.sampleValues;
    var stride = this.valueSize;
    var offset = index * stride;

    for (var i = 0; i !== stride; ++i) {
      result[i] = values[offset + i];
    }

    return result;
  },
  // Template methods for derived classes:
  interpolate_: function interpolate_()
  /* i1, t0, t, t1 */
  {
    throw new Error('call to abstract method'); // implementations shall return this.resultBuffer
  },
  intervalChanged_: function intervalChanged_()
  /* i1, t0, t1 */
  {// empty
  }
}); // DECLARE ALIAS AFTER assign prototype

Object.assign(Interpolant.prototype, {
  // ( 0, t, t0 ), returns this.resultBuffer
  beforeStart_: Interpolant.prototype.copySampleValue_,
  // ( N-1, tN-1, t ), returns this.resultBuffer
  afterEnd_: Interpolant.prototype.copySampleValue_
});

/**
 * @author tschw
 */

function LinearInterpolant(parameterPositions, sampleValues, sampleSize, resultBuffer) {
  Interpolant.call(this, parameterPositions, sampleValues, sampleSize, resultBuffer);
}

LinearInterpolant.prototype = Object.assign(Object.create(Interpolant.prototype), {
  constructor: LinearInterpolant,
  interpolate_: function interpolate_(i1, t0, t, t1) {
    var result = this.resultBuffer;
    var values = this.sampleValues;
    var stride = this.valueSize;
    var offset1 = i1 * stride;
    var offset0 = offset1 - stride;
    var weight1 = (t - t0) / (t1 - t0);
    var weight0 = 1 - weight1;

    for (var i = 0; i !== stride; ++i) {
      result[i] = values[offset0 + i] * weight0 + values[offset1 + i] * weight1;
    }

    return result;
  }
});

var InterpolateLinear = /** @class */ (function () {
    function InterpolateLinear(startValue, endValue) {
        this.startValue = startValue;
        this.endValue = endValue;
        var times = [0, 1];
        var values = [startValue, endValue];
        var valueSize = values.length / times.length;
        this.interpolator = new LinearInterpolant(times, values, valueSize);
    }
    InterpolateLinear.prototype.value = function (time) {
        var value = this.interpolator.evaluate(time);
        return value[0];
    };
    return InterpolateLinear;
}());

// 插值类型
var InterpolationType;
(function (InterpolationType) {
    InterpolationType[InterpolationType["Linear"] = 0] = "Linear";
    InterpolationType[InterpolationType["Bezier"] = 1] = "Bezier";
})(InterpolationType || (InterpolationType = {}));
// 关键帧动画类型
var KeyframeMode;
(function (KeyframeMode) {
    KeyframeMode[KeyframeMode["Continuous"] = 0] = "Continuous";
    KeyframeMode[KeyframeMode["Discontinuous"] = 1] = "Discontinuous";
})(KeyframeMode || (KeyframeMode = {}));
// 关键帧外插模式 (曲线在第一个关键帧之前和最后一个关键帧之后的延伸方式)
var ExtrapolationMode;
(function (ExtrapolationMode) {
    ExtrapolationMode[ExtrapolationMode["Constant"] = 0] = "Constant";
    ExtrapolationMode[ExtrapolationMode["Linear"] = 1] = "Linear";
})(ExtrapolationMode || (ExtrapolationMode = {}));
// 默认的插值类型
var DEFAULT_EASING = InterpolationType.Linear;
// 默认的关键帧
var DEFAULT_KEYFRAME = [0, 0, InterpolationType.Linear];
// 默认的外插模式
var DEFAULT_EXTRAPOLATION_MODE = ExtrapolationMode.Linear;
var AnimationClipState;
(function (AnimationClipState) {
    AnimationClipState[AnimationClipState["Stopped"] = 0] = "Stopped";
    AnimationClipState[AnimationClipState["Playing"] = 1] = "Playing";
    AnimationClipState[AnimationClipState["Paused"] = 2] = "Paused";
})(AnimationClipState || (AnimationClipState = {}));
var AnimationClipType;
(function (AnimationClipType) {
    AnimationClipType[AnimationClipType["Component"] = 0] = "Component";
    AnimationClipType[AnimationClipType["GlTF"] = 1] = "GlTF";
})(AnimationClipType || (AnimationClipType = {}));
var ValueType;
(function (ValueType) {
    ValueType[ValueType["Num"] = 0] = "Num";
    ValueType[ValueType["Hex"] = 1] = "Hex";
    ValueType[ValueType["Mat3f"] = 2] = "Mat3f";
    ValueType[ValueType["Quaternion"] = 3] = "Quaternion";
})(ValueType || (ValueType = {}));
var PropertyValueType;
(function (PropertyValueType) {
    PropertyValueType[PropertyValueType["number"] = 0] = "number";
    PropertyValueType[PropertyValueType["string"] = 1] = "string";
    PropertyValueType[PropertyValueType["quaternion"] = 2] = "quaternion";
})(PropertyValueType || (PropertyValueType = {}));

var InterpolateMat3f = /** @class */ (function () {
    function InterpolateMat3f(startValue, endValue, interpolationType) {
        var _this = this;
        this.startValues = startValue.split(',').map(parseFloat);
        this.endValues = endValue.split(',').map(parseFloat);
        this.interpolators = this.startValues.map(function (v1, index) {
            if (interpolationType === InterpolationType.Bezier) {
                return new InterpolateLinear(v1, _this.endValues[index]);
            }
            return new InterpolateBezier(v1, _this.endValues[index]);
        });
    }
    InterpolateMat3f.prototype.value = function (time) {
        var values = this.interpolators.map(function (interpolator) { return interpolator.value(time); });
        return values.join(',');
    };
    return InterpolateMat3f;
}());

var Keyframe = /** @class */ (function () {
    function Keyframe(keyFrameArray) {
        var _a = keyFrameArray || DEFAULT_KEYFRAME, time = _a[0], value = _a[1], interpolationType = _a[2];
        this.time = time;
        this.value = value;
        this.interpolationType = interpolationType;
    }
    Keyframe.prototype.getTime = function () {
        return this.time;
    };
    Keyframe.prototype.getValue = function () {
        return this.value;
    };
    Keyframe.prototype.getType = function () {
        return this.interpolationType;
    };
    return Keyframe;
}());

/**
     * 欧拉角旋转 转 四元数
     * @param x
     * @param y
     * @param z
     */
function eulerToQuaternion(x, y, z) {
    var degToAngle = Math.PI / 180;
    var v3 = new Euler(x * degToAngle, y * degToAngle, z * degToAngle);
    var quat = new Quaternion();
    quat.setFromEuler(v3);
    var rotation = {
        x: quat.x,
        y: quat.y,
        z: quat.z,
        w: quat.w,
    };
    return rotation;
}
/**
 * 四元数 转 欧拉角旋转
 * @param x
 * @param y
 * @param z
 * @param w
 */
function quaternionToEuler(x, y, z, w) {
    var quat = new Quaternion(x, y, z, w);
    var euler = new Euler();
    var angleToDeg = 180 / Math.PI;
    // 避免奇异点
    var temp = quat.w * quat.y + quat.x * quat.z;
    var threshold = 0.5 - 0.009;
    if (temp < -threshold || temp > threshold) {
        var sign = temp < 0 ? -1 : 1;
        return {
            x: 0,
            y: ((sign * Math.PI) / 2) * angleToDeg,
            z: (360 + 2 * sign * Math.atan2(quat.x, quat.w) * angleToDeg) % 360,
        };
    }
    euler.setFromQuaternion(quat);
    return {
        x: euler.x * angleToDeg,
        y: euler.y * angleToDeg,
        z: euler.z * angleToDeg,
    };
}
/** 渐变方法 **/
var GradientType;
(function (GradientType) {
    GradientType[GradientType["LINEAR"] = 1] = "LINEAR";
    GradientType[GradientType["EASE_IN"] = 2] = "EASE_IN";
    GradientType[GradientType["EASE_OUT"] = 3] = "EASE_OUT";
    GradientType[GradientType["EASE_IN_OUT"] = 4] = "EASE_IN_OUT";
})(GradientType || (GradientType = {}));
// 获取渐变值
function getGradientValue(gradientType, currentTime, startValue, rangeValue, duration) {
    switch (gradientType) {
        case GradientType.LINEAR:
            return linear$1(currentTime, startValue, rangeValue, duration);
        case GradientType.EASE_IN:
            return easeInQuad(currentTime, startValue, rangeValue, duration);
        case GradientType.EASE_OUT:
            return easeOutQuad(currentTime, startValue, rangeValue, duration);
        case GradientType.EASE_IN_OUT:
            return easeInOutQuad(currentTime, startValue, rangeValue, duration);
        default:
            return linear$1(currentTime, startValue, rangeValue, duration);
    }
}
// 慢-快-慢
function easeInOutQuad(currentTime, startValue, rangeValue, duration) {
    currentTime /= duration / 2;
    if (currentTime < 1)
        return rangeValue / 2 * currentTime * currentTime + startValue;
    currentTime -= 1;
    return -rangeValue / 2 * (currentTime * (currentTime - 2) - 1) + startValue;
}
// 快-慢
function easeOutQuad(currentTime, startValue, rangeValue, duration) {
    currentTime /= duration;
    return -rangeValue * currentTime * (currentTime - 2) + startValue;
}
// 慢-快
function easeInQuad(currentTime, startValue, rangeValue, duration) {
    currentTime /= duration;
    return rangeValue * currentTime * currentTime + startValue;
}
// 匀速
function linear$1(currentTime, startValue, rangeValue, duration) {
    return rangeValue * currentTime / duration + startValue;
}

var KeyframeTrack = /** @class */ (function () {
    function KeyframeTrack(propertyTrack, duration) {
        var propertyPath = propertyTrack.propertyPath, keyframes = propertyTrack.keyframes, _a = propertyTrack.valueType, valueType = _a === void 0 ? ValueType.Num : _a;
        this.initPropertyPath(propertyPath);
        this.initKeyframes(keyframes);
        this.valueType = valueType;
        this.duration = duration;
        this.extrapolationMode = DEFAULT_EXTRAPOLATION_MODE;
        this.cacheValueMap = {};
        this.cacheInterpolatorMap = {};
    }
    // "1/BasicTransform/position/x" =>  name, entityId, componentType, attributePath
    KeyframeTrack.prototype.initPropertyPath = function (propertyPath) {
        var _a = propertyPath.split('/'), entityId = _a[0], componentType = _a[1], attributePath = _a.slice(2);
        this.name = propertyPath;
        this.entityId = entityId;
        this.componentType = componentType;
        this.attributePath = attributePath.join('.');
    };
    KeyframeTrack.prototype.initKeyframes = function (keyframes) {
        this.keyframes = keyframes.sort(function (a, b) {
            var _a = [a[0], b[0]], time1 = _a[0], time2 = _a[1];
            return time1 - time2;
        }).map(function (keyframe) { return new Keyframe(keyframe); }); // 根据时间对关键帧排序
        this.firstKeyframe = this.keyframes[0]; // 第一个关键帧
        this.finallyKeyframe = this.keyframes[this.keyframes.length - 1]; // 最后一个关键帧
    };
    KeyframeTrack.prototype.getValueAt = function (deltaTime) {
        // 1. 边界条件判断
        if (isNaN(deltaTime))
            return;
        if (deltaTime <= 0)
            return this.transformValueByType(this.firstKeyframe.getValue());
        if (deltaTime >= this.duration)
            return this.finallyKeyframe.getValue(); // 边界条件 -> 时间大于总时长 (可能吗?)
        // 2. 从cache里面找找看
        if (this.cacheValueMap[deltaTime])
            return this.cacheValueMap[deltaTime];
        // 3. 找到此时的时间在关键帧中所处的位置, 并计算参数
        var _a = this.calcParams(deltaTime), currentTime = _a[0], beginValue = _a[1], endValue = _a[2], duration = _a[3], interpolationType = _a[4];
        // 4. 计算插值
        var value = this.getInterpolationValue(currentTime, beginValue, endValue, duration, interpolationType);
        // 5. 缓存
        this.cacheValueMap[deltaTime] = value;
        return value;
    };
    KeyframeTrack.prototype.getName = function () {
        return this.name;
    };
    KeyframeTrack.prototype.getComponentType = function () {
        return this.componentType;
    };
    KeyframeTrack.prototype.getAttributePath = function () {
        return this.attributePath;
    };
    KeyframeTrack.prototype.getValueType = function () {
        return this.valueType;
    };
    KeyframeTrack.prototype.transformValueByType = function (value) {
        switch (this.valueType) {
            case ValueType.Hex:
                return transformHexToRgbaObj(value);
            case ValueType.Quaternion:
                return eulerToQuaternion(value.x, value.y, value.z);
            default:
                return value;
        }
    };
    KeyframeTrack.prototype.calcParams = function (deltaTime) {
        switch (this.extrapolationMode) {
            case ExtrapolationMode.Constant:
                return this.calcParamsConstant(deltaTime);
            case ExtrapolationMode.Linear:
                return this.calcParamsLinear(deltaTime);
            default:
                return this.calcParamsLinear(deltaTime);
        }
    };
    // 计算 常量 关键帧外插模式时候的参数  (第一帧之前的帧跟第一帧一样, 最后一帧之后的帧和最后一帧一样, 如果前一个关键帧已经是最后一个关键帧，那么后一个关键帧也使用该帧)
    KeyframeTrack.prototype.calcParamsConstant = function (deltaTime) {
        var currentTime;
        var beginValue;
        var endValue;
        var duration;
        var interpolationType;
        if (deltaTime <= this.firstKeyframe.getTime()) { // 2.1 处于第一个关键帧之前
            currentTime = deltaTime;
            beginValue = this.firstKeyframe.getValue();
            endValue = this.firstKeyframe.getValue();
            duration = this.firstKeyframe.getTime();
            interpolationType = this.firstKeyframe.getType();
        }
        else if (deltaTime >= this.finallyKeyframe.getTime()) { // 2.1 处于最后一个关键帧之后
            currentTime = deltaTime - this.finallyKeyframe.getTime();
            beginValue = this.finallyKeyframe.getValue();
            endValue = this.finallyKeyframe.getValue();
            duration = this.duration - this.finallyKeyframe.getTime();
            interpolationType = this.finallyKeyframe.getType();
        }
        else { // 2.3 处于中间关键帧之间, 进行二分查找
            var _a = this.binarySearchKeyframe(deltaTime), beforeKeyframe = _a[0], afterKeyframe = _a[1];
            currentTime = deltaTime - beforeKeyframe.getTime();
            beginValue = beforeKeyframe.getValue();
            endValue = afterKeyframe.getValue();
            duration = afterKeyframe.getTime() - beforeKeyframe.getTime();
            interpolationType = afterKeyframe.getType();
        }
        return [currentTime, beginValue, endValue, duration, interpolationType];
    };
    // 计算 线性 关键帧外插模式时候的参数  (第一帧之前的帧使用最后一帧, 最后一帧之后的帧使用第一帧)
    KeyframeTrack.prototype.calcParamsLinear = function (deltaTime) {
        var currentTime;
        var beginValue;
        var endValue;
        var duration;
        var interpolationType;
        if (deltaTime <= this.firstKeyframe.getTime()) { // 2.1 处于第一个关键帧之前
            currentTime = deltaTime;
            beginValue = this.firstKeyframe.getValue();
            endValue = this.firstKeyframe.getValue();
            duration = this.firstKeyframe.getTime();
            interpolationType = this.firstKeyframe.getType();
        }
        else if (deltaTime >= this.finallyKeyframe.getTime()) { // 2.1 处于最后一个关键帧之后
            currentTime = deltaTime - this.finallyKeyframe.getTime();
            beginValue = this.finallyKeyframe.getValue();
            endValue = this.finallyKeyframe.getValue();
            duration = this.duration - this.finallyKeyframe.getTime();
            interpolationType = this.finallyKeyframe.getType();
        }
        else { // 2.3 处于中间关键帧之间
            var _a = this.binarySearchKeyframe(deltaTime), beforeKeyframe = _a[0], afterKeyframe = _a[1];
            currentTime = deltaTime - beforeKeyframe.getTime();
            beginValue = beforeKeyframe.getValue();
            endValue = afterKeyframe.getValue();
            duration = afterKeyframe.getTime() - beforeKeyframe.getTime();
            interpolationType = afterKeyframe.getType();
        }
        return [currentTime, beginValue, endValue, duration, interpolationType];
    };
    // 二分法查找前后帧
    KeyframeTrack.prototype.binarySearchKeyframe = function (deltaTime) {
        var keyframes = this.keyframes;
        var left = 0;
        var right = keyframes.length - 1;
        while (left <= right) {
            var mid = left + Math.floor((right - left) / 2);
            var currentFrame = keyframes[mid];
            if (deltaTime < currentFrame.getTime()) { // 如果 deltaTime < 当前的时间, 则查找前半部分
                right = mid;
            }
            else if (deltaTime > currentFrame.getTime()) { // 如果 deltaTime > 当前的时间, 则查找后半部分
                left = mid;
            }
            else { // 正好时间是相等的
                return [keyframes[mid], keyframes[mid]];
            }
            if (right - left === 1) { // 找到前后帧
                return [keyframes[left], keyframes[right]];
            }
        }
        return [keyframes[left], keyframes[right]];
    };
    KeyframeTrack.prototype.getInterpolationType = function (beforeKeyframe, afterKeyframe) {
        if (!afterKeyframe)
            return beforeKeyframe.getType();
        return afterKeyframe.getType();
    };
    KeyframeTrack.prototype.getInterpolationValue = function (deltaTime, beginValue, endValue, duration, interpolationType) {
        var interpolator = this.getInterpolator(beginValue, endValue, duration, interpolationType);
        var time = isNaN(deltaTime / duration) ? 0 : deltaTime / duration;
        var value = interpolator.value(time);
        return value;
    };
    KeyframeTrack.prototype.getInterpolator = function (beginValue, endValue, duration, interpolationType) {
        var key = "key-" + beginValue + "-" + endValue + "-" + duration;
        if (this.cacheInterpolatorMap[key]) {
            return this.cacheInterpolatorMap[key];
        }
        var interpolator;
        switch (this.valueType) {
            case ValueType.Hex:
                interpolator = new InterpolateColor(beginValue, endValue);
                break;
            case ValueType.Mat3f:
                interpolator = new InterpolateMat3f(beginValue, endValue, interpolationType);
                break;
            default:
                switch (interpolationType) {
                    case InterpolationType.Bezier:
                        interpolator = new InterpolateBezier(beginValue, endValue);
                        break;
                    case InterpolationType.Linear:
                    default:
                        interpolator = new InterpolateLinear(beginValue, endValue);
                        break;
                }
        }
        this.cacheInterpolatorMap[key] = interpolator;
        return interpolator;
    };
    return KeyframeTrack;
}());

var Animation = /** @class */ (function () {
    function Animation(config) {
        this.propertyTracks = [];
        this.init(config);
    }
    Animation.prototype.init = function (config) {
        var _this = this;
        this.duration = config.duration;
        this.propertyTracks = config.propertyTracks;
        // 1. 根据 propertyTracks 数组生成对应的 KeyframeTrack数组
        this.keyframeTracks = this.propertyTracks.map(function (propertyTrack) { return new KeyframeTrack(propertyTrack, _this.duration); });
    };
    // 根据current time(µs microsecond 微秒)获取插值
    Animation.prototype.getValueAtTime = function (time) {
        var deltaTime = time % this.duration;
        return this.getValueAtDeltaTime(deltaTime);
    };
    // 根据delta time(µs microsecond 微秒)获取插值
    Animation.prototype.getValueAtDeltaTime = function (deltaTime) {
        var values = this.keyframeTracks.map(function (keyframeTrack) { return ({
            componentType: keyframeTrack.getComponentType(),
            propertyPath: keyframeTrack.getAttributePath(),
            valueType: keyframeTrack.getValueType(),
            value: keyframeTrack.getValueAt(deltaTime),
        }); });
        return values;
    };
    // 根据progress(百分比)获取插值
    Animation.prototype.getValueAtProgress = function (progress) {
        var deltaTime = this.duration * progress;
        return this.getValueAtDeltaTime(deltaTime);
    };
    return Animation;
}());

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function patchComponent(component, _propertyPath, _valueType) {
    Object.defineProperties(component, {
        getComponentInstance: {
            value: function (componentType, propertyPath) {
                switch (componentType) {
                    case light.MeshRenderer3DComponent.componentType: {
                        var index = propertyPath.split('.')[0];
                        return component.getPrimitiveMaterial(Number(index)).asPbrMaterial();
                    }
                    default:
                        return component;
                }
            },
        },
        setProperty: {
            value: function (c, _, propertyPath, value, valueType) {
                switch (valueType) {
                    case ValueType.Hex: {
                        var r = value.r, g = value.g, b = value.b, a = value.a;
                        c.base_color_factor_.set(0, parseFloat(r) / 255);
                        c.base_color_factor_.set(1, parseFloat(g) / 255);
                        c.base_color_factor_.set(2, parseFloat(b) / 255);
                        c.base_color_factor_.set(3, parseFloat(a) / 100);
                        break;
                    }
                    case ValueType.Mat3f: {
                        var _a = value.split(',').map(parseFloat), v1 = _a[0], v2 = _a[1], v3 = _a[2], v4 = _a[3], v5 = _a[4], v6 = _a[5], v7 = _a[6], v8 = _a[7], v9 = _a[8];
                        var m1 = light.float3.create(v1, v2, v3);
                        var m2 = light.float3.create(v4, v5, v6);
                        var m3 = light.float3.create(v7, v8, v9);
                        c.base_color_uv_matrix_.set(0, m1);
                        c.base_color_uv_matrix_.set(1, m2);
                        c.base_color_uv_matrix_.set(2, m3);
                        break;
                    }
                    default: {
                        var component_1 = c;
                        var keyPath = propertyPath.split('.');
                        var lastKeyIndex = keyPath.length - 1;
                        for (var i = 0; i < lastKeyIndex; i++) {
                            var key = keyPath[i];
                            if (!(key in component_1)) {
                                component_1[key] = parseInt(key) !== parseInt(key) ? {} : [];
                            }
                            component_1 = component_1[key];
                        }
                        component_1[keyPath[lastKeyIndex]] = value;
                    }
                }
            },
        },
    });
}
var AnimationController = /** @class */ (function (_super) {
    __extends(AnimationController, _super);
    function AnimationController(entityId, entityManager, eventManager, scriptSystem) {
        var _this = _super.call(this, entityId, entityManager, eventManager, scriptSystem) || this;
        _this.cacheClipsProgress = [];
        _this.resourceVersions = [];
        _this.configure();
        return _this;
    }
    AnimationController.prototype.configure = function () {
        console.log('AnimationBehavior ----> configure');
        // 0. 没有组件动画提前return
        var animationClips = this.getAnimationControllerClips();
        var hasComponentAnimation = this.hasComponentAnimation(animationClips);
        if (!hasComponentAnimation)
            return;
        // 1. 读取文件
        var resourceKey = this.getResourceKey();
        if (!resourceKey)
            return;
        var fileData = light.FileSystem.readTextFromResource(this.entityManager, resourceKey);
        // 2. 获取关键帧信息
        var animationData = JSON.parse(fileData);
        var clips = animationData.clips;
        // 3. 初始化动画数组
        this.animationClips = clips.map(function (clip) { return new Animation(clip); });
        // 4. 缓存此时的动画组件播放状态
        this.backupResourceVersion();
    };
    AnimationController.prototype.backupResourceVersion = function () {
        var animationClips = this.getAnimationControllerClips();
        var resourceVersions = [];
        for (var i = 0; i < animationClips.size(); i++) {
            resourceVersions[i] = animationClips.get(i).resourceVersion;
        }
        this.resourceVersions = resourceVersions;
    };
    AnimationController.prototype.backupClipsProgress = function () {
        var animationClips = this.getAnimationControllerClips();
        var cacheClipsProgress = [];
        for (var i = 0; i < animationClips.size(); i++) {
            cacheClipsProgress[i] = animationClips.get(i).progress;
        }
        this.cacheClipsProgress = cacheClipsProgress;
    };
    AnimationController.prototype.getResourceKey = function () {
        var animationClips = this.getAnimationControllerClips();
        if (animationClips.size() === 0)
            return '';
        for (var i = 0; i < animationClips.size(); i++) {
            if (animationClips.get(i).type === 0 /* Component */) {
                return animationClips.get(i).resourceKey;
            }
        }
    };
    AnimationController.prototype.getAnimationControllerClips = function () {
        var animationControllerComp = this.getComponent(light.AnimationController);
        var clips = animationControllerComp.clips;
        return clips;
    };
    AnimationController.prototype.getComponent = function (componentType, propertyPath, valueType) {
        var component = this.entity.getComponent(componentType);
        patchComponent(component);
        return component;
    };
    AnimationController.prototype.checkResourceVersion = function (animationClips) {
        // 1. 还没有初始化
        if (!this.animationClips) {
            this.configure();
            return;
        }
        // 2. 比较动画文件的版本号
        for (var i = 0; i < animationClips.size(); i++) {
            var clip = animationClips.get(i);
            var resourceVersion = clip.resourceVersion;
            var prevResourceVersion = this.resourceVersions[i];
            if (prevResourceVersion !== resourceVersion) {
                this.configure();
                return;
            }
        }
    };
    AnimationController.prototype.hasComponentAnimation = function (animationClips) {
        var hasAnim = false;
        animationClips.forEach(function (clip) {
            if (clip.type === 0 /* Component */) {
                hasAnim = true;
            }
        });
        return hasAnim;
    };
    // 每一帧会调用此方法, 单位为微秒microsecond
    AnimationController.prototype.update = function () {
        var _this = this;
        var animationClips = this.getAnimationControllerClips();
        // 0. 没有组件动画提前return
        var hasComponentAnimation = this.hasComponentAnimation(animationClips);
        if (!hasComponentAnimation)
            return;
        // 1. 检查动画文件版本 是否需要重新读取动画文件
        this.checkResourceVersion(animationClips);
        // 2. 点击动画组件时候播放 找到动画组件, 找到其中progress与上回不一致的, 注: progress其实是time, 并非百分比, 单位为微秒
        for (var i = 0; i < animationClips.size(); i++) {
            var clip = animationClips.get(i);
            var progress = clip.progress, clipIndex = clip.clipIndex, type = clip.type;
            if (type === 1 /* GlTF */)
                continue;
            if (this.cacheClipsProgress[i] === progress)
                continue;
            if (!this.animationClips[clipIndex])
                continue;
            // 2.1 根据progress更新组件
            var clipValues = this.animationClips[clipIndex].getValueAtTime(progress);
            clipValues.forEach(function (clipValue) {
                var componentType = clipValue.componentType, propertyPath = clipValue.propertyPath, value = clipValue.value, valueType = clipValue.valueType;
                _this.updateComponent(componentType, propertyPath, value, valueType);
            });
        }
        // 3. 缓存此时的progress
        this.backupClipsProgress();
    };
    AnimationController.prototype.updateComponent = function (componentType, propertyPath, value, valueType) {
        var component = this.getComponent(componentType, propertyPath, valueType);
        if (!component)
            return;
        var componentInstance = component.getComponentInstance(componentType, propertyPath);
        switch (componentType) {
            case light.BasicTransform.componentType:
            case light.ScreenTransform.componentType:
                this.updateTransformComponent(componentInstance, componentType, propertyPath, value, valueType, component);
                break;
            default:
                component.setProperty(componentInstance, componentType, propertyPath, value, valueType);
                break;
        }
    };
    AnimationController.prototype.updateTransformComponent = function (componentInstance, componentType, propertyPath, value, valueType, component) {
        var pathArray = propertyPath.split('.');
        var lastPath = pathArray[pathArray.length - 1];
        if (propertyPath.indexOf('position') === 0) {
            var componentPosition = componentInstance.position;
            componentPosition[lastPath] = value;
            componentInstance.SetPosition(componentPosition);
        }
        else if (propertyPath.indexOf('eEuler') === 0) {
            if (!this.cacheEuler) {
                this.cacheEuler = {};
            }
            this.cacheEuler[propertyPath.split('.')[1]] = value;
            if (this.cacheEuler.x !== undefined && this.cacheEuler.y !== undefined && this.cacheEuler.z !== undefined) {
                var _a = this.cacheEuler, x = _a.x, y = _a.y, z = _a.z;
                var rotation = eulerToQuaternion(x, y, z);
                var componentRotation = componentInstance.rotation;
                componentRotation.x = rotation.x;
                componentRotation.y = rotation.y;
                componentRotation.z = rotation.z;
                componentRotation.w = rotation.w;
                componentInstance.SetRotation(componentRotation);
                this.cacheEuler = null;
            }
        }
        else if (propertyPath.indexOf('scale') === 0) {
            var componentScale = componentInstance.scale;
            componentScale[lastPath] = value;
            componentInstance.SetScale(componentScale);
        }
        else {
            component.setProperty(componentInstance, componentType, propertyPath, value, valueType);
        }
    };
    AnimationController.prototype.destroy = function () {
        this.animationClips = null;
        this.cacheClipsProgress = null;
        this.resourceVersions = null;
        this.cacheEuler = null;
        _super.prototype.destroy.call(this);
    };
    return AnimationController;
}(BaseBehavior));
light.on('start', function (entityManager, eventManager, scriptSystem) {
    // 1. 从entity里面找到所有包含动画组件的entity
    var entitiesWithAnimation = entityManager.entitiesWithComponents(light.AnimationController.componentType);
    // 2. 对每个entity都注册一个 AnimationController
    entitiesWithAnimation.forEach(function (entity) {
        var idComponent = entity.getComponent(light.EntityIdentifier);
        if (!idComponent)
            return;
        var AnimationBehavior = new AnimationController(idComponent.id, entityManager, eventManager, scriptSystem);
        light.runtime.addBehavior(AnimationBehavior);
    });
});

var AbsNode = /** @class */ (function (_super) {
    __extends(AbsNode, _super);
    function AbsNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AbsNode.prototype.Process = function () {
        if (this.supportedDataType) {
            switch (this.supportedDataType) {
                case 'integer':
                case 'float':
                    this.Result = Math.abs(this.A);
                    break;
                case 'v2':
                    this.Result = { x: Math.abs(this.A.x), y: Math.abs(this.A.y) };
                    break;
                case 'v3':
                    this.Result = { x: Math.abs(this.A.x), y: Math.abs(this.A.y), z: Math.abs(this.A.z) };
                    break;
                case 'v4Rect':
                    this.Result = { x: Math.abs(this.A.x), y: Math.abs(this.A.y), z: Math.abs(this.A.z), w: Math.abs(this.A.w) };
                    break;
            }
        }
    };
    AbsNode.nodeType = 'pro/abs';
    return AbsNode;
}(Node));
light.NodeContext.registerNode(AbsNode);

var AddNode = /** @class */ (function (_super) {
    __extends(AddNode, _super);
    function AddNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AddNode.prototype.Process = function () {
        if (this.supportedDataType) {
            switch (this.supportedDataType) {
                case 'integer':
                case 'float':
                    this.Sum = this.A + this.B;
                    break;
                case 'v2':
                    this.Sum = { x: this.A.x + this.B.x, y: this.A.y + this.B.y };
                    break;
                case 'v3':
                    this.Sum = { x: this.A.x + this.B.x, y: this.A.y + this.B.y, z: this.A.z + this.B.z };
                    break;
                case 'v4Rect':
                    this.Sum = { x: this.A.x + this.B.x, y: this.A.y + this.B.y, z: this.A.z + this.B.z, w: this.A.w + this.B.w };
                    break;
            }
        }
    };
    AddNode.nodeType = 'pro/add';
    return AddNode;
}(Node));
light.NodeContext.registerNode(AddNode);

var AgeDetectNode = /** @class */ (function (_super) {
    __extends(AgeDetectNode, _super);
    function AgeDetectNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AgeDetectNode.prototype.setup = function () {
        openAIFeature(["Age" /* AGE */], this.entityManager, this.eventManager);
    };
    AgeDetectNode.prototype.getData = function () {
        var data = getAIClassData(["Age" /* AGE */], this.entityManager);
        var ageData = data === null || data === void 0 ? void 0 : data.onAge;
        if (!ageData || this.faceIndex > ageData.length) {
            return;
        }
        ageData.sort(function (a, b) { return a[0] - b[0]; });
        this.age = ageData[this.faceIndex - 1][1];
        this.next();
    };
    AgeDetectNode.prototype.next = function () { };
    AgeDetectNode.nodeType = 'pro/Age';
    return AgeDetectNode;
}(Node));
light.NodeContext.registerNode(AgeDetectNode);

var DisPatchNode = /** @class */ (function (_super) {
    __extends(DisPatchNode, _super);
    function DisPatchNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.x = 0;
        _this.y = 0;
        _this.z = 0;
        _this.w = 0;
        return _this;
    }
    DisPatchNode.prototype.Process = function () {
        if (this.supportedDataType) {
            switch (this.supportedDataType) {
                case 'v2':
                    this.x = this.vector.x;
                    this.y = this.vector.y;
                    break;
                case 'v3':
                    this.x = this.vector.x;
                    this.y = this.vector.y;
                    this.z = this.vector.z;
                    break;
                case 'v4Rect':
                case 'quaternionf':
                    this.x = this.vector.x;
                    this.y = this.vector.y;
                    this.z = this.vector.z;
                    this.w = this.vector.w;
                    break;
                case 'area':
                    this.x = this.vector.x;
                    this.y = this.vector.y;
                    this.z = this.vector.w;
                    this.w = this.vector.h;
                    break;
            }
        }
    };
    DisPatchNode.nodeType = 'pro/DisPatch';
    return DisPatchNode;
}(DataProcessNode));
light.NodeContext.registerNode(DisPatchNode);

var EveryFrameNode = /** @class */ (function (_super) {
    __extends(EveryFrameNode, _super);
    function EveryFrameNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.costTime = 0;
        _this._currentTime = 0;
        _this._lastTime = 0;
        return _this;
    }
    EveryFrameNode.prototype.Run = function () {
        this.Next();
        this.addListener('update', this.update);
        this._lastTime = light.getCurrentTime();
    };
    EveryFrameNode.prototype.update = function () {
        this._currentTime = light.getCurrentTime();
        this.costTime = (this._currentTime - this._lastTime) / 1000000;
        this._lastTime = this._currentTime;
        this.Next();
    };
    EveryFrameNode.prototype.Next = function () { };
    EveryFrameNode.nodeType = 'pro/EveryFrame';
    return EveryFrameNode;
}(EventNode));
light.NodeContext.registerNode(EveryFrameNode);

var PatchNode = /** @class */ (function (_super) {
    __extends(PatchNode, _super);
    function PatchNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.x = 0;
        _this.y = 0;
        _this.z = 0;
        _this.w = 0;
        return _this;
    }
    PatchNode.prototype.Process = function () {
        if (this.supportedDataType) {
            switch (this.supportedDataType) {
                case 'v2':
                    this.outputValue = { x: this.x, y: this.y };
                    break;
                case 'v3':
                    this.outputValue = { x: this.x, y: this.y, z: this.z };
                    break;
                case 'v4Rect':
                    this.outputValue = { x: this.x, y: this.y, z: this.z, w: this.w };
                    break;
                case 'quaternionf':
                    this.outputValue = { x: this.x, y: this.y, z: this.z, w: this.w };
                    break;
                case 'area':
                    this.outputValue = { x: this.x, y: this.y, w: this.z, h: this.w };
                    break;
            }
        }
    };
    PatchNode.nodeType = 'pro/Patch';
    return PatchNode;
}(DataProcessNode));
light.NodeContext.registerNode(PatchNode);

var RecordEndNode = /** @class */ (function (_super) {
    __extends(RecordEndNode, _super);
    function RecordEndNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RecordEndNode.prototype.Run = function () {
        var _this = this;
        var recordingEventKey = 'event.script.lightsdk.CameraEndRecord';
        light.on('UpdateInputEvent', function (params) {
            var jsonData = params[recordingEventKey];
            if (jsonData !== undefined && jsonData !== null) {
                _this.StartRecording();
            }
        });
    };
    RecordEndNode.prototype.StartRecording = function () { };
    RecordEndNode.nodeType = 'pro/RecordEndEvent';
    return RecordEndNode;
}(EventNode));
light.NodeContext.registerNode(RecordEndNode);

var RecordStartNode = /** @class */ (function (_super) {
    __extends(RecordStartNode, _super);
    function RecordStartNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RecordStartNode.prototype.Run = function () {
        var _this = this;
        var recordingEventKey = 'event.script.lightsdk.CameraStartRecord';
        light.on('UpdateInputEvent', function (params) {
            var jsonData = params[recordingEventKey];
            if (jsonData !== undefined && jsonData !== null) {
                _this.StartRecording();
            }
        });
    };
    RecordStartNode.prototype.StartRecording = function () { };
    RecordStartNode.nodeType = 'pro/RecordStartEvent';
    return RecordStartNode;
}(EventNode));
light.NodeContext.registerNode(RecordStartNode);

var SomeFrameNode = /** @class */ (function (_super) {
    __extends(SomeFrameNode, _super);
    function SomeFrameNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.frame = 1;
        _this.repeatTimes = 0;
        _this.startupTrigger = false;
        _this._triggeredTimes = 0;
        _this._currentFrame = 0;
        return _this;
    }
    SomeFrameNode.prototype.Run = function () {
        if (this.startupTrigger && this.repeatTimes !== 0) {
            this.Next();
            this._triggeredTimes = this._triggeredTimes + 1;
        }
        this.addListener('update', this.onFrameUpdate);
    };
    SomeFrameNode.prototype.onFrameUpdate = function () {
        if (this._currentFrame !== 0) {
            var trigger = (this._currentFrame % this.frame) === 0;
            var infinite = this.repeatTimes > 9999;
            var eligible = this.repeatTimes > this._triggeredTimes;
            if (infinite || eligible) {
                if (trigger) {
                    this.Next();
                    this._triggeredTimes = this._triggeredTimes + 1;
                }
            }
        }
        this._currentFrame += 1;
    };
    SomeFrameNode.prototype.Next = function () { };
    SomeFrameNode.nodeType = 'pro/SomeFrame';
    return SomeFrameNode;
}(EventNode));
light.NodeContext.registerNode(SomeFrameNode);

var StartNode = /** @class */ (function (_super) {
    __extends(StartNode, _super);
    function StartNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    StartNode.prototype.Run = function () {
        this.Next();
    };
    StartNode.prototype.Next = function () { };
    StartNode.nodeType = 'pro/Start';
    return StartNode;
}(EventNode));
light.NodeContext.registerNode(StartNode);

var TimingNode = /** @class */ (function (_super) {
    __extends(TimingNode, _super);
    function TimingNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.repeatTimes = 1;
        _this.triggerInterval = 1;
        _this.isStartupTriggered = false;
        _this._lastTime = 0;
        _this._triggeredTimes = 0;
        return _this;
    }
    TimingNode.prototype.Run = function () {
        if (this.isStartupTriggered && this.repeatTimes !== 0) {
            this._triggeredTimes = this._triggeredTimes + 1;
            this.Next();
        }
        this._lastTime = light.getCurrentTime();
        this.addListener('update', this.onFrameUpdate);
    };
    TimingNode.prototype.onFrameUpdate = function (currentTime) {
        if ((currentTime - this._lastTime) > this.triggerInterval * 1000000
            && (this.repeatTimes > this._triggeredTimes || this.repeatTimes > 9999)) {
            this._lastTime = currentTime;
            this._triggeredTimes = this._triggeredTimes + 1;
            this.Next();
        }
    };
    TimingNode.prototype.Next = function () { };
    TimingNode.nodeType = 'pro/Timing';
    return TimingNode;
}(EventNode));
light.NodeContext.registerNode(TimingNode);

var AndOrNotNode = /** @class */ (function (_super) {
    __extends(AndOrNotNode, _super);
    function AndOrNotNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = false;
        _this.B = false;
        return _this;
    }
    AndOrNotNode.prototype.Process = function () {
        this.andValue = this.A && this.B;
        this.orValue = this.A || this.B;
        this.notAValue = !this.A;
    };
    AndOrNotNode.nodeType = 'pro/andOrNot';
    return AndOrNotNode;
}(Node));
light.NodeContext.registerNode(AndOrNotNode);

var AndNode = /** @class */ (function (_super) {
    __extends(AndNode, _super);
    function AndNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = false;
        _this.B = false;
        return _this;
    }
    AndNode.prototype.Process = function () {
        this.andValue = this.A && this.B;
    };
    AndNode.nodeType = 'pro/and';
    return AndNode;
}(Node));
light.NodeContext.registerNode(AndNode);

var ApproximatelyEqualNode = /** @class */ (function (_super) {
    __extends(ApproximatelyEqualNode, _super);
    function ApproximatelyEqualNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        _this.C = 0;
        return _this;
    }
    ApproximatelyEqualNode.prototype.Process = function () {
        this.Res = Math.abs(this.A - this.B) < this.C;
    };
    ApproximatelyEqualNode.nodeType = 'pro/approximatelyEqual';
    return ApproximatelyEqualNode;
}(Node));
light.NodeContext.registerNode(ApproximatelyEqualNode);

var ArcCosNode = /** @class */ (function (_super) {
    __extends(ArcCosNode, _super);
    function ArcCosNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    ArcCosNode.prototype.Process = function () {
        this.Result = Math.acos(this.A);
    };
    ArcCosNode.nodeType = 'pro/arcCos';
    return ArcCosNode;
}(Node));
light.NodeContext.registerNode(ArcCosNode);

var ArcSinNode = /** @class */ (function (_super) {
    __extends(ArcSinNode, _super);
    function ArcSinNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    ArcSinNode.prototype.Process = function () {
        this.Result = Math.asin(this.A);
    };
    ArcSinNode.nodeType = 'pro/arcSin';
    return ArcSinNode;
}(Node));
light.NodeContext.registerNode(ArcSinNode);

var ArcTanNode = /** @class */ (function (_super) {
    __extends(ArcTanNode, _super);
    function ArcTanNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    ArcTanNode.prototype.Process = function () {
        this.Result = Math.atan(this.A);
    };
    ArcTanNode.nodeType = 'pro/arcTan';
    return ArcTanNode;
}(Node));
light.NodeContext.registerNode(ArcTanNode);

var BodyPointNode = /** @class */ (function (_super) {
    __extends(BodyPointNode, _super);
    function BodyPointNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    BodyPointNode.prototype.setup = function () {
        openAIFeature(["Body_Point" /* BODY_POINT */], this.entityManager, this.eventManager);
    };
    BodyPointNode.prototype.getData = function () {
        var data = getAIPointData("Body_Point" /* BODY_POINT */, this.entityManager);
        if (data.length === 0) {
            return;
        }
        var bodyInfo = data[0];
        if (2 * this.pointIndex + 1 >= bodyInfo.point_array_.length) {
            return;
        }
        var canvasWidth = bodyInfo.canvas_width_;
        var canvasHeight = bodyInfo.canvas_height_;
        // 归一化
        this.location = {
            x: (2 * bodyInfo.point_array_[2 * this.pointIndex]) / canvasWidth - 1,
            y: 1 - (2 * bodyInfo.point_array_[2 * this.pointIndex + 1]) / canvasHeight,
        };
        this.next();
    };
    BodyPointNode.prototype.next = function () { };
    BodyPointNode.nodeType = 'pro/BodyPoint';
    return BodyPointNode;
}(Node));
light.NodeContext.registerNode(BodyPointNode);

var BodyTriggerNode = /** @class */ (function (_super) {
    __extends(BodyTriggerNode, _super);
    function BodyTriggerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.features = ["Body_Point" /* BODY_POINT */];
        return _this;
    }
    BodyTriggerNode.prototype.shouldTrigger = function () {
        var data = getAIPointData(this.features[0], this.entityManager);
        return data.length > 0;
    };
    BodyTriggerNode.nodeType = 'pro/BodyTrigger';
    return BodyTriggerNode;
}(AITriggerNode));
light.NodeContext.registerNode(BodyTriggerNode);

var CatPointNode = /** @class */ (function (_super) {
    __extends(CatPointNode, _super);
    function CatPointNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    CatPointNode.prototype.setup = function () {
        openAIFeature(["Cat_Point" /* CAT_POINT */], this.entityManager, this.eventManager);
    };
    CatPointNode.prototype.getData = function () {
        var data = getAIPointData("Cat_Point" /* CAT_POINT */, this.entityManager);
        if (data.length === 0) {
            return;
        }
        var catInfo = data[0];
        if (2 * this.pointIndex + 1 >= catInfo.point_array_.length) {
            return;
        }
        var canvasWidth = catInfo.canvas_width_;
        var canvasHeight = catInfo.canvas_height_;
        // 归一化
        this.location = {
            x: (2 * catInfo.point_array_[2 * this.pointIndex]) / canvasWidth - 1,
            y: 1 - (2 * catInfo.point_array_[2 * this.pointIndex + 1]) / canvasHeight,
        };
        this.next();
    };
    CatPointNode.prototype.next = function () { };
    CatPointNode.nodeType = 'pro/CatPoint';
    return CatPointNode;
}(Node));
light.NodeContext.registerNode(CatPointNode);

var CatPoseNode = /** @class */ (function (_super) {
    __extends(CatPoseNode, _super);
    function CatPoseNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    CatPoseNode.prototype.setup = function () {
        openAIFeature(["Cat_Point" /* CAT_POINT */], this.entityManager, this.eventManager);
    };
    CatPoseNode.prototype.getData = function () {
        var data = getAIPointData("Cat_Point" /* CAT_POINT */, this.entityManager);
        if (data.length === 0) {
            return;
        }
        // 按 trace ID 从小到大排序
        var catInfo = data[0];
        var canvasWidth = catInfo.canvas_width_;
        var canvasHeight = catInfo.canvas_height_;
        // 遍历点位
        var minX = canvasWidth;
        var maxX = 0;
        var minY = canvasHeight;
        var maxY = 0;
        for (var i = 0; i < catInfo.point_array_.length; i++) {
            var point = catInfo.point_array_[i];
            if (i % 2 === 0) {
                minX = Math.min(minX, point);
                maxX = Math.max(maxX, point);
            }
            else {
                minY = Math.min(minY, point);
                maxY = Math.max(maxY, point);
            }
        }
        var top = 1 - (2 * minY) / canvasHeight;
        var right = (2 * maxX) / canvasWidth - 1;
        var bottom = 1 - (2 * maxY) / canvasHeight;
        var left = (2 * minX) / canvasWidth - 1;
        this.boundingBox = {
            x: left,
            y: bottom,
            w: right - left,
            h: top - bottom,
        };
        this.rotation = { x: catInfo.roll_, y: catInfo.pitch_, z: catInfo.yaw_ };
        this.next();
    };
    CatPoseNode.prototype.next = function () { };
    CatPoseNode.nodeType = 'pro/CatPose';
    return CatPoseNode;
}(Node));
light.NodeContext.registerNode(CatPoseNode);

var CatTriggerNode = /** @class */ (function (_super) {
    __extends(CatTriggerNode, _super);
    function CatTriggerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.features = ["Cat_Point" /* CAT_POINT */];
        return _this;
    }
    CatTriggerNode.prototype.shouldTrigger = function () {
        var data = getAIPointData(this.features[0], this.entityManager);
        return data.length > 0;
    };
    CatTriggerNode.nodeType = 'pro/CatTrigger';
    return CatTriggerNode;
}(AITriggerNode));
light.NodeContext.registerNode(CatTriggerNode);

var CeilNode = /** @class */ (function (_super) {
    __extends(CeilNode, _super);
    function CeilNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    CeilNode.prototype.Process = function () {
        this.Result = Math.ceil(this.A);
    };
    CeilNode.nodeType = 'pro/ceil';
    return CeilNode;
}(Node));
light.NodeContext.registerNode(CeilNode);

var CosNode = /** @class */ (function (_super) {
    __extends(CosNode, _super);
    function CosNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    CosNode.prototype.Process = function () {
        this.Result = Math.cos(this.A);
    };
    CosNode.nodeType = 'pro/cos';
    return CosNode;
}(Node));
light.NodeContext.registerNode(CosNode);

var DataConvertNode = /** @class */ (function (_super) {
    __extends(DataConvertNode, _super);
    function DataConvertNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.value = 0;
        return _this;
    }
    DataConvertNode.prototype.Process = function () {
        var num = Number(this.value);
        if (!isNaN(num)) {
            this.int = Math.round(Number(this.value));
            this.float = Number(this.value);
        }
        this.string = String(this.value);
    };
    DataConvertNode.nodeType = 'pro/DataConvert';
    return DataConvertNode;
}(Node));
light.NodeContext.registerNode(DataConvertNode);

var DelayNode = /** @class */ (function (_super) {
    __extends(DelayNode, _super);
    function DelayNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.duration = 1;
        _this.lastTime = 0;
        return _this;
    }
    DelayNode.prototype.Run = function () {
        this.lastTime = light.getCurrentTime();
        this.Update(light.getCurrentTime());
        this.addListener('update', this.Update);
    };
    DelayNode.prototype.Update = function (time) {
        if (time - this.lastTime >= this.duration * 1000000) {
            this.removeListener('update');
            this.Next();
        }
    };
    DelayNode.prototype.Next = function () { };
    DelayNode.nodeType = 'pro/Delay';
    return DelayNode;
}(Node));
light.NodeContext.registerNode(DelayNode);

var DivideNode = /** @class */ (function (_super) {
    __extends(DivideNode, _super);
    function DivideNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    DivideNode.prototype.Process = function () {
        if (this.supportedDataType) {
            switch (this.supportedDataType) {
                case 'integer':
                    this.Res = Math.floor(this.A / this.B);
                    break;
                case 'float':
                    this.Res = this.A / this.B;
                    break;
                case 'v2':
                    this.Res = { x: this.A.x / this.B.x, y: this.A.y / this.B.y };
                    break;
                case 'v3':
                    this.Res = { x: this.A.x / this.B.x, y: this.A.y / this.B.y, z: this.A.z / this.B.z };
                    break;
                case 'v4Rect':
                    this.Res = { x: this.A.x / this.B.x, y: this.A.y / this.B.y, z: this.A.z / this.B.z, w: this.A.w / this.B.w };
                    break;
            }
        }
    };
    DivideNode.nodeType = 'pro/divide';
    return DivideNode;
}(Node));
light.NodeContext.registerNode(DivideNode);

var DoTimesNode = /** @class */ (function (_super) {
    __extends(DoTimesNode, _super);
    function DoTimesNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.times = 1;
        _this.CurrentTimes = 0;
        return _this;
    }
    DoTimesNode.prototype.Run = function () {
        if (this.CurrentTimes < this.times) {
            this.CurrentTimes += 1;
            this.Next();
        }
    };
    DoTimesNode.prototype.Next = function () { };
    DoTimesNode.nodeType = 'pro/doTimes';
    return DoTimesNode;
}(Node));
light.NodeContext.registerNode(DoTimesNode);

var TouchAction;
(function (TouchAction) {
    TouchAction[TouchAction["DOWN"] = 0] = "DOWN";
    TouchAction[TouchAction["UP"] = 1] = "UP";
    TouchAction[TouchAction["MOVE"] = 2] = "MOVE";
    TouchAction[TouchAction["POINTER_DOWN"] = 3] = "POINTER_DOWN";
    TouchAction[TouchAction["POINTER_UP"] = 4] = "POINTER_UP";
})(TouchAction || (TouchAction = {}));
function getNormalizedCoords(event) {
    var width = light.DeviceUtils.GetSurfaceWidth(event._entityManager);
    var height = light.DeviceUtils.GetSurfaceHeight(event._entityManager);
    return {
        x: (2 * event.getX()) / ((1280 * width) / height) - 1,
        y: 1 - (2 * event.getY()) / 1280,
    };
}

var DragGestureNode = /** @class */ (function (_super) {
    __extends(DragGestureNode, _super);
    function DragGestureNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    DragGestureNode.prototype.Run = function () {
        var _this = this;
        light.on('TouchEvent', function (event) {
            if (event.getPointerCount() !== 1) {
                return;
            }
            if (event.getAction() === TouchAction.DOWN) {
                _this.touchDownLocation = {
                    x: event.getX(),
                    y: event.getY(),
                };
                _this.normalizedTouchDownLocation = getNormalizedCoords(event);
            }
            else if (event.getAction() === TouchAction.MOVE) {
                if (Math.abs(event.getX() - _this.touchDownLocation.x) < 20
                    && Math.abs(event.getY() - _this.touchDownLocation.y) < 20) {
                    return;
                }
                _this.location = getNormalizedCoords(event);
                _this.next();
            }
        });
    };
    DragGestureNode.prototype.next = function () { };
    DragGestureNode.nodeType = 'pro/DragGesture';
    return DragGestureNode;
}(EventNode));
light.NodeContext.registerNode(DragGestureNode);

var MAX_FACE_NUMBER = 5;
var Queue = [
    light.Image,
    light.PAGAsset,
    light.HairColor,
    light.PinchFace,
    light.Liquefaction,
    light.LiquefactionV6,
    light.MakeupFaceV6,
    light.MakeupLipsV6,
    light.MakeupIrisV6,
    light.Stretch,
    light.LUTFilter,
    light.GAN,
    light.PostEffect,
    light.MakeupFace,
    light.MakeupLips,
    light.MakeupIris,
    light.MakeupEyeV6,
    light.MakeupEyebrowV6,
    light.Smooth,
    light.Beauty,
    light.BeautyBody,
    light.CustomShader,
    light.CustomGraph,
    light.Tips,
    light.CrazyFace,
    light.Sticker3D,
    light.Snapshot,
    light.Particle,
    light.ExpressionTransfer,
];
var getRelateEntities = function (entityId, entityManager) {
    var entities = [];
    var entity = entityManager.getEntityById(entityId);
    if (entity) {
        var faceTrack = entity.getComponent(light.FaceTracking);
        entities.push(entity);
        if (faceTrack) {
            if (faceTrack.duplicate_entity_id_) { // 新版
                for (var i = 0; i < faceTrack.duplicate_entity_id_.size(); i += 1) {
                    var id = faceTrack.duplicate_entity_id_.get(i);
                    var relateEntity = entityManager.getEntityById(id);
                    if (relateEntity === null || relateEntity === void 0 ? void 0 : relateEntity.transform) {
                        entities.push(relateEntity);
                    }
                }
            }
            else {
                var faceIds = new Array(MAX_FACE_NUMBER - 1).fill(0) // 兼容旧版
                    .map(function (value, i) { return entityId + ((i + 1) * 10000); });
                faceIds.forEach(function (id) {
                    var relateEntity = entityManager.getEntityById(id);
                    if (relateEntity === null || relateEntity === void 0 ? void 0 : relateEntity.transform) {
                        entities.push(relateEntity);
                    }
                });
            }
        }
    }
    return entities;
};
var showEntities = function (ids, entityManager) {
    if (ids === void 0) { ids = []; }
    ids.forEach(function (entityId) {
        var entities = getRelateEntities(entityId, entityManager); // 获取人脸跟随的对象集
        entities.forEach(function (entity) {
            var timeOffset = entity.getComponent(light.TimeOffset);
            var timeContrl = entity.getComponent(light.TimeControl);
            if (timeOffset) {
                timeOffset.startOffset = light.getCurrentTime();
            }
            if (timeContrl) {
                timeContrl.reset();
                timeContrl.currentTime = 0;
            }
            var setTransform = true;
            Queue.forEach(function (queueItem) {
                var cmp = entity.getComponent(queueItem);
                if (cmp) {
                    cmp.enabled = true;
                    setTransform = false;
                }
            });
            if (setTransform) {
                [
                    light.BasicTransform,
                    light.ScreenTransform,
                ].forEach(function (item) {
                    var cmp = entity.getComponent(item);
                    if (cmp) {
                        cmp.objectEnabled = true;
                        cmp.enabled = true;
                    }
                });
            }
        });
    });
};
var hideEntities = function (ids, entityManager) {
    if (ids === void 0) { ids = []; }
    ids.forEach(function (entityId) {
        getRelateEntities(entityId, entityManager).forEach(function (entity) {
            var setTransform = true;
            Queue.forEach(function (queueItem) {
                var cmp = entity.getComponent(queueItem);
                if (cmp) {
                    cmp.enabled = false;
                    setTransform = false;
                }
            });
            if (setTransform) {
                [
                    light.BasicTransform,
                    light.ScreenTransform,
                ].forEach(function (item) {
                    var cmp = entity.getComponent(item);
                    if (cmp) {
                        cmp.objectEnabled = false;
                        cmp.enabled = false;
                    }
                });
            }
        });
    });
};
var eventBus = {};
var emitEvent = function (eventName) {
    var args = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        args[_i - 1] = arguments[_i];
    }
    if (eventBus[eventName]) {
        eventBus[eventName].forEach(function (f) {
            f.apply(void 0, args);
        });
    }
};
var submitEvent = function (eventName, callback) {
    if (!eventBus[eventName]) {
        eventBus[eventName] = [];
    }
    eventBus[eventName].push(callback);
};

var EntityLoopNode = /** @class */ (function (_super) {
    __extends(EntityLoopNode, _super);
    function EntityLoopNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.random = false;
        _this.times = 1;
        _this.duration = 1;
        _this.num = 2;
        _this.toShow = [];
        _this.toHide = [];
        _this.lastExecuteTime = 0;
        _this.totalExecuteTimes = 0;
        _this.isStop = false;
        _this.isFirstTriggered = false;
        _this.indexToDisplay = 0;
        return _this;
    }
    EntityLoopNode.prototype.Run = function () {
        this.Reset();
        this.addListener('update', this.Update);
        if (this.random) {
            this.indexToDisplay = Math.floor(Math.random() * this.num);
        }
        this.lastExecuteTime = light.getCurrentTime();
        this.Update(this.lastExecuteTime);
        this.Next();
    };
    EntityLoopNode.prototype.Update = function (time) {
        // 延时未到，暂不执行
        if (!this.isFirstTriggered && time / 1000 - this.lastExecuteTime / 1000 < this.duration * 1000) {
            return;
        }
        this.isFirstTriggered = false;
        // 达到循环次数或手动停止，不继续执行
        if (this.totalExecuteTimes >= this.times * this.num || this.isStop) {
            this.removeListener('update');
            return;
        }
        this.toShow = [];
        this.toHide = [];
        for (var i = 0; i < this.num; i++) {
            var entity = this.getEntityById(this["entity" + (i + 1)]);
            if (entity) {
                if (i === this.indexToDisplay % this.num) {
                    this.toShow.push(this["entity" + (i + 1)]);
                }
                else {
                    this.toHide.push(this["entity" + (i + 1)]);
                }
            }
        }
        this.indexToDisplay = this.indexToDisplay + 1;
        this.totalExecuteTimes += 1;
        this.lastExecuteTime = light.getCurrentTime();
        showEntities(this.toShow, this.entityManager);
        hideEntities(this.toHide, this.entityManager);
        this.Loop();
    };
    EntityLoopNode.prototype.Reset = function () {
        this.isFirstTriggered = true;
        this.isStop = false;
        this.totalExecuteTimes = 0;
        this.removeListener('update');
    };
    EntityLoopNode.prototype.Stop = function () {
        this.isStop = true;
    };
    EntityLoopNode.prototype.Next = function () { };
    EntityLoopNode.prototype.Loop = function () { };
    EntityLoopNode.nodeType = 'pro/entityLoop';
    return EntityLoopNode;
}(Node));
light.NodeContext.registerNode(EntityLoopNode);

var EntityPulseNode = /** @class */ (function (_super) {
    __extends(EntityPulseNode, _super);
    function EntityPulseNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.toShow = [];
        _this.toHide = [];
        _this.num = 2;
        _this.init = false;
        _this.random = false;
        _this.isFirstRun = true;
        _this.indexToDisplay = 0;
        return _this;
    }
    EntityPulseNode.prototype.Run = function () {
        this.toShow = [];
        this.toHide = [];
        if (this.random && this.isFirstRun) {
            this.indexToDisplay = Math.floor(Math.random() * this.num);
            this.isFirstRun = false;
        }
        for (var i = 0; i < this.num; i++) {
            var entity = this.getEntityById(this["entity" + (i + 1)]);
            if (entity) {
                if (i === this.indexToDisplay % this.num) {
                    this.toShow.push(this["entity" + (i + 1)]);
                }
                else {
                    this.toHide.push(this["entity" + (i + 1)]);
                }
            }
        }
        this.indexToDisplay = this.indexToDisplay + 1;
        showEntities(this.toShow, this.entityManager);
        hideEntities(this.toHide, this.entityManager);
        this.Next();
    };
    EntityPulseNode.prototype.Next = function () { };
    EntityPulseNode.nodeType = 'pro/entityPulse';
    return EntityPulseNode;
}(Node));
light.NodeContext.registerNode(EntityPulseNode);

var EntityRandomNode = /** @class */ (function (_super) {
    __extends(EntityRandomNode, _super);
    function EntityRandomNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.num = 2;
        _this.toShow = [];
        _this.toHide = [];
        _this.indexToDisplay = 0;
        return _this;
    }
    EntityRandomNode.prototype.Run = function () {
        this.toShow = [];
        this.toHide = [];
        this.indexToDisplay = Math.floor(Math.random() * this.num);
        for (var i = 0; i < this.num; i++) {
            var entity = this.getEntityById(this["entity" + (i + 1)]);
            if (entity) {
                if (i === this.indexToDisplay % this.num) {
                    this.toShow.push(this["entity" + (i + 1)]);
                }
                else {
                    this.toHide.push(this["entity" + (i + 1)]);
                }
            }
        }
        showEntities(this.toShow, this.entityManager);
        hideEntities(this.toHide, this.entityManager);
        this.Next();
    };
    EntityRandomNode.prototype.Next = function () { };
    EntityRandomNode.nodeType = 'pro/entityRandom';
    return EntityRandomNode;
}(Node));
light.NodeContext.registerNode(EntityRandomNode);

var EqualNode = /** @class */ (function (_super) {
    __extends(EqualNode, _super);
    function EqualNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    EqualNode.prototype.Process = function () {
        this.Res = this.A === this.B;
    };
    EqualNode.nodeType = 'pro/equal';
    return EqualNode;
}(Node));
light.NodeContext.registerNode(EqualNode);

var ExponentiationNode = /** @class */ (function (_super) {
    __extends(ExponentiationNode, _super);
    function ExponentiationNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    ExponentiationNode.prototype.Process = function () {
        this.Result = Math.exp(this.A);
    };
    ExponentiationNode.nodeType = 'pro/exponentiation';
    return ExponentiationNode;
}(Node));
light.NodeContext.registerNode(ExponentiationNode);

var ExpressionTriggerNode = /** @class */ (function (_super) {
    __extends(ExpressionTriggerNode, _super);
    function ExpressionTriggerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.features = [
            "Face_Point" /* FACE_POINT */,
            "Expression" /* EXPRESSION */,
            "Pout" /* POUT */,
            "Smile" /* SMILE */,
        ];
        return _this;
    }
    ExpressionTriggerNode.prototype.shouldTrigger = function () {
        var pointData = getAIPointData("Face_Point" /* FACE_POINT */, this.entityManager);
        var ids = pointData.map(function (x) { return x.id_; });
        ids.sort();
        if (this.faceIndex > ids.length) {
            return;
        }
        var actionEventName = actionNameToEvent(light.FaceAction[this.expression]);
        var classData = getAIClassData([
            "Expression" /* EXPRESSION */,
            "Pout" /* POUT */,
            "Smile" /* SMILE */,
        ], this.entityManager);
        var expressionIds = classData === null || classData === void 0 ? void 0 : classData[actionEventName];
        if (this.faceIndex === 0) {
            // 检测任意人脸
            return (expressionIds === null || expressionIds === void 0 ? void 0 : expressionIds.length) > 0;
        }
        // 要检测人脸对应的 trace ID
        var id = ids[this.faceIndex - 1];
        return expressionIds === null || expressionIds === void 0 ? void 0 : expressionIds.includes(id);
    };
    ExpressionTriggerNode.nodeType = 'pro/ExpressionTrigger';
    return ExpressionTriggerNode;
}(AITriggerNode));
light.NodeContext.registerNode(ExpressionTriggerNode);

var FacePointNode = /** @class */ (function (_super) {
    __extends(FacePointNode, _super);
    function FacePointNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    FacePointNode.prototype.setup = function () {
        openAIFeature(["Face_Point" /* FACE_POINT */], this.entityManager, this.eventManager);
    };
    FacePointNode.prototype.getData = function () {
        var data = getAIPointData("Face_Point" /* FACE_POINT */, this.entityManager);
        if (this.faceIndex > data.length) {
            return;
        }
        // 按 trace ID 从小到大排序
        data.sort(function (a, b) { return a.id_ - b.id_; });
        var faceInfo = data[this.faceIndex - 1];
        if (2 * this.pointIndex + 1 >= faceInfo.point_array_.length) {
            return;
        }
        var canvasWidth = faceInfo.canvas_width_;
        var canvasHeight = faceInfo.canvas_height_;
        // 归一化
        this.location = {
            x: (2 * faceInfo.point_array_[2 * this.pointIndex]) / canvasWidth - 1,
            y: 1 - (2 * faceInfo.point_array_[2 * this.pointIndex + 1]) / canvasHeight,
        };
        this.next();
    };
    FacePointNode.prototype.next = function () { };
    FacePointNode.nodeType = 'pro/FacePoint';
    return FacePointNode;
}(Node));
light.NodeContext.registerNode(FacePointNode);

var FacePoseNode = /** @class */ (function (_super) {
    __extends(FacePoseNode, _super);
    function FacePoseNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    FacePoseNode.prototype.setup = function () {
        openAIFeature(["Face_Point" /* FACE_POINT */], this.entityManager, this.eventManager);
    };
    FacePoseNode.prototype.getData = function () {
        var data = getAIPointData("Face_Point" /* FACE_POINT */, this.entityManager);
        if (this.faceIndex > data.length) {
            return;
        }
        // 按 trace ID 从小到大排序
        data.sort(function (a, b) { return a.id_ - b.id_; });
        var faceInfo = data[this.faceIndex - 1];
        var canvasWidth = faceInfo.canvas_width_;
        var canvasHeight = faceInfo.canvas_height_;
        // 遍历点位
        var minX = canvasWidth;
        var maxX = 0;
        var minY = canvasHeight;
        var maxY = 0;
        for (var i = 0; i < faceInfo.point_array_.length; i++) {
            var point = faceInfo.point_array_[i];
            if (i % 2 === 0) {
                minX = Math.min(minX, point);
                maxX = Math.max(maxX, point);
            }
            else {
                minY = Math.min(minY, point);
                maxY = Math.max(maxY, point);
            }
        }
        var top = 1 - (2 * minY) / canvasHeight;
        var right = (2 * maxX) / canvasWidth - 1;
        var bottom = 1 - (2 * maxY) / canvasHeight;
        var left = (2 * minX) / canvasWidth - 1;
        this.boundingBox = {
            x: left,
            y: bottom,
            w: right - left,
            h: top - bottom,
        };
        this.rotation = { x: faceInfo.roll_, y: faceInfo.pitch_, z: faceInfo.yaw_ };
        this.next();
    };
    FacePoseNode.prototype.next = function () { };
    FacePoseNode.nodeType = 'pro/FacePose';
    return FacePoseNode;
}(Node));
light.NodeContext.registerNode(FacePoseNode);

var FaceTriggerNode = /** @class */ (function (_super) {
    __extends(FaceTriggerNode, _super);
    function FaceTriggerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.features = ["Face_Point" /* FACE_POINT */];
        return _this;
    }
    FaceTriggerNode.prototype.shouldTrigger = function () {
        var data = getAIPointData(this.features[0], this.entityManager);
        if (data.length === 0) {
            return false;
        }
        return this.faceIndex <= data.length;
    };
    FaceTriggerNode.nodeType = 'pro/FaceTrigger';
    return FaceTriggerNode;
}(AITriggerNode));
light.NodeContext.registerNode(FaceTriggerNode);

var FloorNode = /** @class */ (function (_super) {
    __extends(FloorNode, _super);
    function FloorNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    FloorNode.prototype.Process = function () {
        this.Result = Math.floor(this.A);
    };
    FloorNode.nodeType = 'pro/floor';
    return FloorNode;
}(Node));
light.NodeContext.registerNode(FloorNode);

var ForLoopNode = /** @class */ (function (_super) {
    __extends(ForLoopNode, _super);
    function ForLoopNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.From = 0;
        _this.To = 1;
        _this.Step = 1;
        _this.CurrentValue = 0;
        return _this;
    }
    ForLoopNode.prototype.Run = function () {
        // 不会终止的循环则不执行
        if ((this.From < this.To && this.Step < 0) || (this.From > this.To && this.Step > 0) || this.Step === 0) {
            return;
        }
        this.CurrentValue = this.From;
        this.addListener('update', this.update);
        this.update();
    };
    ForLoopNode.prototype.update = function () {
        if (this.CurrentValue < this.To) {
            this.CurrentValue += this.Step;
            this.Loop();
        }
        else {
            this.CurrentValue = this.To;
            this.removeListener('update');
            this.Finish();
        }
    };
    ForLoopNode.prototype.Loop = function () { };
    ForLoopNode.prototype.Finish = function () { };
    ForLoopNode.nodeType = 'pro/forLoop';
    return ForLoopNode;
}(Node));
light.NodeContext.registerNode(ForLoopNode);

var GenderDetectNode = /** @class */ (function (_super) {
    __extends(GenderDetectNode, _super);
    function GenderDetectNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    GenderDetectNode.prototype.setup = function () {
        openAIFeature(["Gender" /* GENDER */], this.entityManager, this.eventManager);
    };
    GenderDetectNode.prototype.getData = function () {
        var data = getAIClassData(["Gender" /* GENDER */], this.entityManager);
        // 男性人脸的 trace ID
        var males = data === null || data === void 0 ? void 0 : data.onMale;
        // 女性人脸的 trace ID
        var females = data === null || data === void 0 ? void 0 : data.onFemale;
        // 检测任意人脸
        if (this.faceIndex === 0) {
            if ((males === null || males === void 0 ? void 0 : males.length) > 0) {
                this.maleDetected();
            }
            if ((females === null || females === void 0 ? void 0 : females.length) > 0) {
                this.femaleDetected();
            }
            return;
        }
        // 按 trace ID 从小到大排序
        var ids = [];
        if (males) {
            ids = ids.concat(males);
        }
        if (females) {
            ids = ids.concat(females);
        }
        ids.sort();
        if (this.faceIndex > ids.length) {
            return;
        }
        var id = ids[this.faceIndex - 1];
        if (males.includes(id)) {
            this.maleDetected();
        }
        else if (females.includes(id)) {
            this.femaleDetected();
        }
    };
    GenderDetectNode.prototype.maleDetected = function () { };
    GenderDetectNode.prototype.femaleDetected = function () { };
    GenderDetectNode.nodeType = 'pro/Gender';
    return GenderDetectNode;
}(Node));
light.NodeContext.registerNode(GenderDetectNode);

var GestureTriggerNode = /** @class */ (function (_super) {
    __extends(GestureTriggerNode, _super);
    function GestureTriggerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.features = [
            "Hand_Point" /* HAND_POINT */,
            "Hand_Gesture" /* HAND_GESTURE */,
        ];
        return _this;
    }
    GestureTriggerNode.prototype.shouldTrigger = function () {
        var pointData = getAIPointData("Hand_Point" /* HAND_POINT */, this.entityManager);
        var ids = pointData.map(function (x) { return x.id_; });
        ids.sort();
        if (this.handIndex > ids.length) {
            return;
        }
        var actionEventName = actionNameToEvent(light.GestureAction[this.gesture]);
        var classData = getAIClassData(["Hand_Gesture" /* HAND_GESTURE */], this.entityManager);
        var gestureIds = classData === null || classData === void 0 ? void 0 : classData[actionEventName];
        if (this.handIndex === 0) {
            // 检测任意人手
            return (gestureIds === null || gestureIds === void 0 ? void 0 : gestureIds.length) > 0;
        }
        // 要检测人手对应的 trace ID
        var id = ids[this.handIndex - 1];
        return gestureIds === null || gestureIds === void 0 ? void 0 : gestureIds.includes(id);
    };
    GestureTriggerNode.nodeType = 'pro/GestureTrigger';
    return GestureTriggerNode;
}(AITriggerNode));
light.NodeContext.registerNode(GestureTriggerNode);

var GradientNode = /** @class */ (function (_super) {
    __extends(GradientNode, _super);
    function GradientNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.currentTime = 0;
        _this.timeStart = -1;
        _this.pauseTime = 0;
        _this.pauseDuration = 0;
        _this.loopTimes = 1;
        _this.currentLoopTime = 0;
        // 用于控制暂停与继续
        _this.isUpdating = true;
        /*
          是否调用过 Finish 接口。设置该标志位，处理预览面板进度条快速拖动的情况
          比如，该node设置1~3秒中进行渐变，用户将预览进度条从2秒直接拖到5秒
          2秒时，未调用 Finish 接口，isFinished 为false
          5秒时，增加 Finish 接口 的调用，确保预览面板显示正确
          */
        _this.isFinished = false;
        /*
          是否需要重置。设置该标志位，处理多次 run 的情况
          比如，定时触发 + 渐变。
          定时周期为1秒，渐变周期为3秒
          */
        _this.isNeedReset = false;
        return _this;
    }
    GradientNode.prototype.Run = function () {
        this.DataReset();
        if (this.to === this.from || this.duration === 0) { // 校验输入值
            return;
        }
        this.Start();
        this.onUpdate(light.getCurrentTime());
        this.addListener('update', this.onUpdate);
    };
    GradientNode.prototype.Pause = function () {
        this.removeListener('update');
        this.isUpdating = false;
        this.pauseTime = light.getCurrentTime();
    };
    GradientNode.prototype.Continue = function () {
        this.isUpdating = true;
        this.pauseDuration = light.getCurrentTime() - this.pauseTime;
        this.onUpdate(light.getCurrentTime());
        this.addListener('update', this.onUpdate);
    };
    GradientNode.prototype.Stop = function () {
        this.isUpdating = false;
        this.removeListener('update');
        this.DataReset();
    };
    GradientNode.prototype.Start = function () { };
    GradientNode.prototype.Next = function () { };
    GradientNode.prototype.Finish = function () { };
    GradientNode.prototype.transformHexToRgbaObj = function (hexColor) {
        var hex = hexColor.replace('#', '0x');
        var colorNum = Number(hex);
        var r = (colorNum >>> 24);
        var g = (colorNum >>> 16) & 0x00ff;
        var b = (colorNum >>> 8) & 0x0000ff;
        var a = (colorNum & 0x000000ff) / 255.0 * 100;
        return { r: r, g: g, b: b, a: a };
    };
    GradientNode.prototype.onUpdate = function (time) {
        var _this = this;
        // 暂停或停止后不进行更新
        if (!this.isUpdating) {
            return;
        }
        // currentTime为初始值0时，更新timeStart
        if (this.currentTime === undefined || this.isNeedReset) {
            this.timeStart = time;
            this.isNeedReset = false;
        }
        this.currentTime = time;
        if (this.isFinished && this.currentTime >= this.timeStart + this.duration * 1000000) {
            return;
        }
        this.isFinished = false;
        if (time / 1000 - this.timeStart / 1000 - this.pauseDuration / 1000 <= this.duration * 1000) {
            var current_1 = (this.currentTime - this.timeStart - this.pauseDuration) / 1000;
            if (current_1 < 0) {
                return;
            }
            var start = this.from;
            var duration_1 = this.duration * 1000;
            var getValue = function (startValue, rangeValue) { return getGradientValue(_this.gradientType, current_1, startValue, rangeValue, duration_1); };
            if (this.supportedDataType) {
                switch (this.supportedDataType) {
                    case 'integer':
                    case 'float': {
                        var range = this.to - this.from;
                        this.CurrentValue = getValue(start, range);
                        break;
                    }
                    case 'color': {
                        // 十六进制颜色每两位分别代表R、G、B通道，这里分割处理
                        var fromRGB = this.transformHexToRgbaObj(this.from);
                        var toRGB = this.transformHexToRgbaObj(this.to);
                        var R = Math.round(getValue(fromRGB.r, toRGB.r - fromRGB.r)).toString(16);
                        var G = Math.round(getValue(fromRGB.g, toRGB.g - fromRGB.g)).toString(16);
                        var B = Math.round(getValue(fromRGB.b, toRGB.b - fromRGB.b)).toString(16);
                        var A = Math.round(getValue(fromRGB.a, toRGB.a - fromRGB.a) / 100 * 255).toString(16);
                        // 不足两位补齐
                        var hexR = R.length < 2 ? "0" + R : R;
                        var hexG = G.length < 2 ? "0" + G : G;
                        var hexB = B.length < 2 ? "0" + B : B;
                        var hexA = A.length < 2 ? "0" + A : A;
                        this.CurrentValue = "#" + hexR + hexG + hexB + hexA;
                        break;
                    }
                    case 'v2': {
                        var rangeX = this.to.x - this.from.x;
                        var rangeY = this.to.y - this.from.y;
                        this.CurrentValue = { x: getValue(start.x, rangeX), y: getValue(start.y, rangeY) };
                        break;
                    }
                    case 'v3': {
                        var rangeX = this.to.x - this.from.x;
                        var rangeY = this.to.y - this.from.y;
                        var rangeZ = this.to.z - this.from.z;
                        this.CurrentValue = {
                            x: getValue(start.x, rangeX),
                            y: getValue(start.y, rangeY),
                            z: getValue(start.z, rangeZ),
                        };
                        break;
                    }
                    case 'v4Rect': {
                        var rangeX = this.to.x - this.from.x;
                        var rangeY = this.to.y - this.from.y;
                        var rangeZ = this.to.z - this.from.z;
                        var rangeW = this.to.w - this.from.w;
                        this.CurrentValue = {
                            x: getValue(start.x, rangeX),
                            y: getValue(start.y, rangeY),
                            z: getValue(start.z, rangeZ),
                            w: getValue(start.w, rangeW),
                        };
                        break;
                    }
                }
            }
        }
        else if (time < this.timeStart) {
            this.CurrentValue = this.from;
        }
        else {
            this.currentLoopTime += 1;
            // 循环次数达到上限，则终止更新
            if (this.currentLoopTime >= this.loopTimes) {
                this.CurrentValue = this.to;
                this.removeListener('update');
                this.isFinished = true;
                this.Finish();
            }
            else {
                // 一次循环结束，重置开始时间及当前值，开始下一次循环
                this.timeStart = time;
                this.CurrentValue = this.from;
            }
        }
        this.Next();
    };
    GradientNode.prototype.DataReset = function () {
        this.isNeedReset = true;
        this.isUpdating = true;
        this.isFinished = false;
        this.pauseTime = 0;
        this.pauseDuration = 0;
        this.currentLoopTime = 0;
        this.CurrentValue = this.from;
    };
    GradientNode.nodeType = 'pro/Gradient';
    return GradientNode;
}(Node));
light.NodeContext.registerNode(GradientNode);

var GreaterEqualNode = /** @class */ (function (_super) {
    __extends(GreaterEqualNode, _super);
    function GreaterEqualNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    GreaterEqualNode.prototype.Process = function () {
        this.Res = this.A >= this.B;
    };
    GreaterEqualNode.nodeType = 'pro/greaterEqual';
    return GreaterEqualNode;
}(Node));
light.NodeContext.registerNode(GreaterEqualNode);

var GreaterNode = /** @class */ (function (_super) {
    __extends(GreaterNode, _super);
    function GreaterNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    GreaterNode.prototype.Process = function () {
        this.Res = this.A > this.B;
    };
    GreaterNode.nodeType = 'pro/greater';
    return GreaterNode;
}(Node));
light.NodeContext.registerNode(GreaterNode);

var HandPointNode = /** @class */ (function (_super) {
    __extends(HandPointNode, _super);
    function HandPointNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    HandPointNode.prototype.setup = function () {
        openAIFeature(["Hand_Point" /* HAND_POINT */], this.entityManager, this.eventManager);
    };
    HandPointNode.prototype.getData = function () {
        var data = getAIPointData("Hand_Point" /* HAND_POINT */, this.entityManager);
        if (this.handIndex > data.length) {
            return;
        }
        // 按 trace ID 从小到大排序
        data.sort(function (a, b) { return a.id_ - b.id_; });
        var handInfo = data[this.handIndex - 1];
        if (2 * this.pointIndex + 1 >= handInfo.point_array_.length) {
            return;
        }
        var canvasWidth = handInfo.canvas_width_;
        var canvasHeight = handInfo.canvas_height_;
        // 归一化
        this.location = {
            x: (2 * handInfo.point_array_[2 * this.pointIndex]) / canvasWidth - 1,
            y: 1 - (2 * handInfo.point_array_[2 * this.pointIndex + 1]) / canvasHeight,
        };
        this.next();
    };
    HandPointNode.prototype.next = function () { };
    HandPointNode.nodeType = 'pro/HandPoint';
    return HandPointNode;
}(Node));
light.NodeContext.registerNode(HandPointNode);

var HandPoseNode = /** @class */ (function (_super) {
    __extends(HandPoseNode, _super);
    function HandPoseNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    HandPoseNode.prototype.setup = function () {
        openAIFeature(["Hand_Point" /* HAND_POINT */], this.entityManager, this.eventManager);
    };
    HandPoseNode.prototype.getData = function () {
        var data = getAIPointData("Hand_Point" /* HAND_POINT */, this.entityManager);
        if (this.handIndex > data.length) {
            return;
        }
        // 按 trace ID 从小到大排序
        data.sort(function (a, b) { return a.id_ - b.id_; });
        var handInfo = data[this.handIndex - 1];
        var canvasWidth = handInfo.canvas_width_;
        var canvasHeight = handInfo.canvas_height_;
        // 遍历点位
        var minX = canvasWidth;
        var maxX = 0;
        var minY = canvasHeight;
        var maxY = 0;
        for (var i = 0; i < handInfo.point_array_.length; i++) {
            var point = handInfo.point_array_[i];
            if (i % 2 === 0) {
                minX = Math.min(minX, point);
                maxX = Math.max(maxX, point);
            }
            else {
                minY = Math.min(minY, point);
                maxY = Math.max(maxY, point);
            }
        }
        var top = 1 - (2 * minY) / canvasHeight;
        var right = (2 * maxX) / canvasWidth - 1;
        var bottom = 1 - (2 * maxY) / canvasHeight;
        var left = (2 * minX) / canvasWidth - 1;
        this.boundingBox = {
            x: left,
            y: bottom,
            w: right - left,
            h: top - bottom,
        };
        this.rotation = { x: handInfo.roll_, y: handInfo.pitch_, z: handInfo.yaw_ };
        this.next();
    };
    HandPoseNode.prototype.next = function () { };
    HandPoseNode.nodeType = 'pro/HandPose';
    return HandPoseNode;
}(Node));
light.NodeContext.registerNode(HandPoseNode);

var HandTriggerNode = /** @class */ (function (_super) {
    __extends(HandTriggerNode, _super);
    function HandTriggerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.features = ["Hand_Point" /* HAND_POINT */];
        return _this;
    }
    HandTriggerNode.prototype.shouldTrigger = function () {
        var data = getAIPointData(this.features[0], this.entityManager);
        if (data.length === 0) {
            return false;
        }
        return this.handIndex <= data.length;
    };
    HandTriggerNode.nodeType = 'pro/HandTrigger';
    return HandTriggerNode;
}(AITriggerNode));
light.NodeContext.registerNode(HandTriggerNode);

var IfNode = /** @class */ (function (_super) {
    __extends(IfNode, _super);
    function IfNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.Dep = false;
        return _this;
    }
    IfNode.prototype.Run = function () {
        if (this.Dep) {
            this.True();
        }
        else {
            this.False();
        }
    };
    IfNode.prototype.True = function () { };
    IfNode.prototype.False = function () { };
    IfNode.nodeType = 'pro/if';
    return IfNode;
}(Node));
light.NodeContext.registerNode(IfNode);

var LessEqualNode = /** @class */ (function (_super) {
    __extends(LessEqualNode, _super);
    function LessEqualNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    LessEqualNode.prototype.Process = function () {
        this.Res = this.A <= this.B;
    };
    LessEqualNode.nodeType = 'pro/lessEqual';
    return LessEqualNode;
}(Node));
light.NodeContext.registerNode(LessEqualNode);

var LessNode = /** @class */ (function (_super) {
    __extends(LessNode, _super);
    function LessNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    LessNode.prototype.Process = function () {
        this.Res = this.A < this.B;
    };
    LessNode.nodeType = 'pro/less';
    return LessNode;
}(Node));
light.NodeContext.registerNode(LessNode);

var ModNode = /** @class */ (function (_super) {
    __extends(ModNode, _super);
    function ModNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    ModNode.prototype.Process = function () {
        this.Result = this.A % this.B;
    };
    ModNode.nodeType = 'pro/mod';
    return ModNode;
}(Node));
light.NodeContext.registerNode(ModNode);

var MultiplyNode = /** @class */ (function (_super) {
    __extends(MultiplyNode, _super);
    function MultiplyNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    MultiplyNode.prototype.Process = function () {
        if (this.supportedDataType) {
            switch (this.supportedDataType) {
                case 'integer':
                case 'float':
                    this.Res = this.A * this.B;
                    break;
                case 'v2':
                    this.Res = { x: this.A.x * this.B.x, y: this.A.y * this.B.y };
                    break;
                case 'v3':
                    this.Res = { x: this.A.x * this.B.x, y: this.A.y * this.B.y, z: this.A.z * this.B.z };
                    break;
                case 'v4Rect':
                    this.Res = { x: this.A.x * this.B.x, y: this.A.y * this.B.y, z: this.A.z * this.B.z, w: this.A.w * this.B.w };
                    break;
            }
        }
    };
    MultiplyNode.nodeType = 'pro/multiply';
    return MultiplyNode;
}(Node));
light.NodeContext.registerNode(MultiplyNode);

var NotNode = /** @class */ (function (_super) {
    __extends(NotNode, _super);
    function NotNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = false;
        return _this;
    }
    NotNode.prototype.Process = function () {
        this.notValue = !this.A;
    };
    NotNode.nodeType = 'pro/not';
    return NotNode;
}(Node));
light.NodeContext.registerNode(NotNode);

var NumberCompareNode = /** @class */ (function (_super) {
    __extends(NumberCompareNode, _super);
    function NumberCompareNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    NumberCompareNode.prototype.Run = function () {
        if (this.A > this.B) {
            this.Greater();
        }
        if (this.A >= this.B) {
            this.GreaterEqual();
        }
        if (this.A === this.B) {
            this.Equal();
        }
        if (this.A <= this.B) {
            this.LessEqual();
        }
        if (this.A < this.B) {
            this.Less();
        }
    };
    NumberCompareNode.prototype.Greater = function () { };
    NumberCompareNode.prototype.GreaterEqual = function () { };
    NumberCompareNode.prototype.Equal = function () { };
    NumberCompareNode.prototype.LessEqual = function () { };
    NumberCompareNode.prototype.Less = function () { };
    NumberCompareNode.nodeType = 'pro/numberCompare';
    return NumberCompareNode;
}(Node));
light.NodeContext.registerNode(NumberCompareNode);

var HEIGHT = 1280; // 画布标准高
var ScreenTransformUtils = /** @class */ (function () {
    function ScreenTransformUtils() {
    }
    ScreenTransformUtils.getEntitySize = function (entityManager, entityId) {
        var entity = entityManager.getEntityById(entityId);
        var transform = entity.screenTransform;
        var parentSize = this.getParentSize(entityManager, entityId);
        return this.getSizeFromTransform(transform, parentSize);
    };
    // 相对父 entity 中心的归一化坐标
    ScreenTransformUtils.getEntityPosition = function (entityManager, entityId) {
        var entity = entityManager.getEntityById(entityId);
        var transform = entity.screenTransform;
        var parentSize = this.getParentSize(entityManager, entityId);
        var position = this.getPositionFromTransform(transform, parentSize);
        position.x /= parentSize.width;
        position.y /= parentSize.height;
        return position;
    };
    ScreenTransformUtils.getEntityGlobalPosition = function (entityManager, entityId) {
        var entity = entityManager.getEntityById(entityId);
        var transform = entity.screenTransform;
        // 中心点坐标
        var position = this.getEntityPosition(entityManager, entityId);
        var transformArray = this.getParentTransforms(entityManager, transform);
        transformArray.splice(0, 1); // 删除自己
        // 变换到全局坐标系
        for (var _i = 0, transformArray_1 = transformArray; _i < transformArray_1.length; _i++) {
            var parentTransform = transformArray_1[_i];
            var parentPosition = this.getEntityPosition(entityManager, parentTransform.entityId);
            var parentSize = this.getEntitySize(entityManager, parentTransform.entityId);
            var pSize = this.getParentSize(entityManager, parentTransform.entityId);
            var parentPivot = parentTransform.pivot;
            position.x = parentPosition.x
                + (position.x - parentPivot.x) * (parentSize.width / pSize.width);
            position.y = parentPosition.y
                + (position.y - parentPivot.y) * (parentSize.height / pSize.height);
            if (pSize.height === HEIGHT) {
                break;
            }
        }
        return position;
    };
    ScreenTransformUtils.getEntityRotation = function (entityManager, entityId) {
        var entity = entityManager.getEntityById(entityId);
        var transform = entity.screenTransform;
        return quaternionToEuler(transform.rotation.x, transform.rotation.y, transform.rotation.z, transform.rotation.w).z;
    };
    ScreenTransformUtils.getParentTransforms = function (entityManager, transform) {
        var result = [transform];
        if (!entityManager.valid(transform.parent)) {
            return [transform];
        }
        var parent = entityManager.get(transform.parent);
        var parentTransform = parent.getComponent(light.ScreenTransform);
        if (parentTransform !== null) {
            result = result.concat(this.getParentTransforms(entityManager, parentTransform));
        }
        else {
            return [transform];
        }
        return result;
    };
    ScreenTransformUtils.getParentSize = function (entityManager, entityId) {
        var entity = entityManager.getEntityById(entityId);
        var transform = entity.screenTransform;
        if (!transform) {
            return undefined;
        }
        var transformArray = this.getParentTransforms(entityManager, transform);
        transformArray.splice(0, 1); // 删除自己
        var deviceWidth = light.DeviceUtils.GetSurfaceWidth(entityManager);
        var deviceHeight = light.DeviceUtils.GetSurfaceHeight(entityManager);
        var ratio = deviceWidth / deviceHeight;
        var parentSize = { width: ratio * HEIGHT, height: HEIGHT };
        for (var i = transformArray.length - 1; i >= 0; i--) {
            parentSize = this.getSizeFromTransform(transformArray[i], parentSize);
        }
        return parentSize;
    };
    ScreenTransformUtils.getSizeFromTransform = function (transform, parentSize) {
        var width = ((transform.anchor.right - transform.anchor.left) / 2)
            * parentSize.width
            + (transform.offset.right - transform.offset.left);
        var height = ((transform.anchor.top - transform.anchor.bottom) / 2)
            * parentSize.height
            + (transform.offset.top - transform.offset.bottom);
        return { width: width, height: height };
    };
    ScreenTransformUtils.getPositionFromTransform = function (transform, pSize) {
        var left = transform.anchor.left * pSize.width + transform.offset.left;
        var right = transform.anchor.right * pSize.width + transform.offset.right;
        var top = transform.anchor.top * pSize.height + transform.offset.top;
        var bottom = transform.anchor.bottom * pSize.height + transform.offset.bottom;
        var position = { x: (right + left) / 2, y: (top + bottom) / 2 };
        position.x += ((right - left) / 2) * transform.pivot.x;
        position.y += ((top - bottom) / 2) * transform.pivot.y;
        return position;
    };
    return ScreenTransformUtils;
}());

var Object2DPositionNode = /** @class */ (function (_super) {
    __extends(Object2DPositionNode, _super);
    function Object2DPositionNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object2DPositionNode.prototype.getData = function () {
        var _this = this;
        light.once('RenderWillStartEvent', function () {
            var entityId = _this.component.entityId;
            var entity = _this.getEntityById(entityId);
            if (!entity) {
                return;
            }
            var deviceWidth = light.DeviceUtils.GetSurfaceWidth(_this.entityManager) || 720;
            var deviceHeight = light.DeviceUtils.GetSurfaceHeight(_this.entityManager) || 1280;
            var transform = entity.screenTransform;
            if (!transform) {
                return;
            }
            var position = ScreenTransformUtils.getEntityGlobalPosition(_this.entityManager, entityId);
            var size = ScreenTransformUtils.getEntitySize(_this.entityManager, entityId);
            var pSize = {
                width: (deviceWidth / deviceHeight) * 1280,
                height: 1280,
            };
            _this.rect = {
                x: position.x - ((transform.pivot.x + 1) * size.width) / pSize.width,
                y: position.y - ((transform.pivot.y + 1) * size.height) / pSize.height,
                w: (size.width / pSize.width) * 2,
                h: (size.height / pSize.height) * 2,
            };
        });
        this.next();
    };
    Object2DPositionNode.prototype.next = function () { };
    Object2DPositionNode.nodeType = 'pro/Object2DPosition';
    return Object2DPositionNode;
}(Node));
light.NodeContext.registerNode(Object2DPositionNode);

var OrNode = /** @class */ (function (_super) {
    __extends(OrNode, _super);
    function OrNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = false;
        _this.B = false;
        return _this;
    }
    OrNode.prototype.Process = function () {
        this.orValue = this.A || this.B;
    };
    OrNode.nodeType = 'pro/or';
    return OrNode;
}(Node));
light.NodeContext.registerNode(OrNode);

var PowerNode = /** @class */ (function (_super) {
    __extends(PowerNode, _super);
    function PowerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    PowerNode.prototype.Process = function () {
        this.Result = Math.pow(this.A, this.B);
    };
    PowerNode.nodeType = 'pro/power';
    return PowerNode;
}(Node));
light.NodeContext.registerNode(PowerNode);

var ProjectNode = /** @class */ (function (_super) {
    __extends(ProjectNode, _super);
    function ProjectNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ProjectNode.prototype.Process = function () {
        var entityId = this.cameraComponent.entityId;
        var cameraEntity = this.getEntityById(entityId);
        if (!cameraEntity) {
            return;
        }
        var position = light.CameraUtils.Project(this.entityManager, cameraEntity, new light.Vec3(this.input.x, this.input.y, this.input.z), false, false);
        this.output = {
            x: position.x,
            y: position.y,
        };
    };
    ProjectNode.nodeType = 'pro/Project';
    return ProjectNode;
}(DataProcessNode));
light.NodeContext.registerNode(ProjectNode);

var RandomValueNode = /** @class */ (function (_super) {
    __extends(RandomValueNode, _super);
    function RandomValueNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.from = 0;
        _this.to = 1;
        _this.outValue = 0;
        _this.notRepeat = false;
        _this.randomValueList = [];
        return _this;
    }
    RandomValueNode.prototype.Run = function () {
        if (this.from === this.to) {
            return this.from;
        }
        if (this.supportedDataType) {
            this.outValue = this.getRandomValue();
        }
        this.Next();
    };
    RandomValueNode.prototype.Next = function () { };
    RandomValueNode.prototype.getRandomValue = function (count) {
        if (count === void 0) { count = 100; }
        if (count < 0) {
            return this.lastRandomValue || this.to;
        }
        var randomValue = this.getRandomValueByType();
        if (!this.notRepeat) {
            return randomValue;
        }
        if (this.randomValueList.find(function (item) { return item === randomValue; }) == undefined) {
            this.lastRandomValue = randomValue;
            this.randomValueList.push(randomValue);
            return randomValue;
        }
        else if (this.supportedDataType === 'integer' && this.randomValueList.length > Math.abs(this.to - this.from)) {
            return this.lastRandomValue || this.to;
        }
        else {
            return this.getRandomValue(count - 1);
        }
    };
    RandomValueNode.prototype.getRandomValueByType = function () {
        var randomValue;
        switch (this.supportedDataType) {
            case 'integer':
                randomValue = Math.round(Math.round(this.from) + (Math.random() * (this.to - this.from)));
                break;
            case 'float':
                randomValue = this.from + (Math.random() * (this.to - this.from));
                break;
        }
        return randomValue;
    };
    RandomValueNode.nodeType = 'pro/randomValue';
    return RandomValueNode;
}(Node));
light.NodeContext.registerNode(RandomValueNode);

var RangeCounterNode = /** @class */ (function (_super) {
    __extends(RangeCounterNode, _super);
    function RangeCounterNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.Value = 0;
        return _this;
    }
    RangeCounterNode.prototype.add = function () {
        if (this.current === undefined) {
            this.current = this.from;
        }
        if (this.current < this.max) {
            this.current += 1;
        }
        this.Value = this.current;
        this.Next();
    };
    RangeCounterNode.prototype.minus = function () {
        if (this.current === undefined) {
            this.current = this.from;
        }
        if (this.current > this.min) {
            this.current -= 1;
        }
        this.Value = this.current;
        this.Next();
    };
    RangeCounterNode.prototype.reset = function () {
        this.current = this.from;
        this.Next();
    };
    RangeCounterNode.prototype.Next = function () { };
    RangeCounterNode.nodeType = 'pro/rangeNumber';
    return RangeCounterNode;
}(Node));
light.NodeContext.registerNode(RangeCounterNode);

var RectIntersectDetectionNode = /** @class */ (function (_super) {
    __extends(RectIntersectDetectionNode, _super);
    function RectIntersectDetectionNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.areaA = { x: 0, y: 0, w: 0, h: 0 };
        _this.areaB = { x: 0, y: 0, w: 0, h: 0 };
        return _this;
    }
    RectIntersectDetectionNode.prototype.Run = function () {
        if (this.isNull(this.areaA) || this.isNull(this.areaB)) {
            return;
        }
        if (this.isAContainedB()) {
            this.AContainB();
        }
        if (this.isBContainedA()) {
            this.BContainA();
        }
        var isIntersected = this.isIntersected();
        if (isIntersected) {
            this.Intersect();
        }
        else {
            this.NotIntersected();
        }
    };
    RectIntersectDetectionNode.prototype.Intersect = function () { };
    RectIntersectDetectionNode.prototype.AContainB = function () { };
    RectIntersectDetectionNode.prototype.BContainA = function () { };
    RectIntersectDetectionNode.prototype.NotIntersected = function () { };
    RectIntersectDetectionNode.prototype.isNull = function (area) {
        return area.x === 0 && area.y === 0 && area.w === 0 && area.h === 0;
    };
    // 检查是否相交
    RectIntersectDetectionNode.prototype.isIntersected = function () {
        return !(this.areaA.x + this.areaA.w < this.areaB.x
            || this.areaB.x + this.areaB.w < this.areaA.x
            || this.areaA.y + this.areaA.h < this.areaB.y
            || this.areaB.y + this.areaB.h < this.areaA.y);
    };
    // 检查是否完全包含
    RectIntersectDetectionNode.prototype.isAContainedB = function () {
        return this.areaB.x >= this.areaA.x && this.areaB.x + this.areaB.w <= this.areaA.x + this.areaA.w
            && this.areaB.y >= this.areaA.y && this.areaB.y + this.areaB.h <= this.areaA.y + this.areaA.h;
    };
    RectIntersectDetectionNode.prototype.isBContainedA = function () {
        return (this.areaA.x >= this.areaB.x && this.areaA.x + this.areaA.w <= this.areaB.x + this.areaB.w
            && this.areaA.y >= this.areaB.y && this.areaA.y + this.areaA.h <= this.areaB.y + this.areaB.h);
    };
    RectIntersectDetectionNode.nodeType = 'pro/rectIntersectDetection';
    return RectIntersectDetectionNode;
}(Node));
light.NodeContext.registerNode(RectIntersectDetectionNode);

var RoundNode = /** @class */ (function (_super) {
    __extends(RoundNode, _super);
    function RoundNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    RoundNode.prototype.Process = function () {
        this.Result = Math.round(this.A);
    };
    RoundNode.nodeType = 'pro/round';
    return RoundNode;
}(Node));
light.NodeContext.registerNode(RoundNode);

var SelectNode = /** @class */ (function (_super) {
    __extends(SelectNode, _super);
    function SelectNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = false;
        return _this;
    }
    SelectNode.prototype.Process = function () {
        if (this.A) {
            this.Res = this.TrueInput;
        }
        else {
            this.Res = this.FalseInput;
        }
    };
    SelectNode.nodeType = 'pro/select';
    return SelectNode;
}(Node));
light.NodeContext.registerNode(SelectNode);

var SequenceLoopNode = /** @class */ (function (_super) {
    __extends(SequenceLoopNode, _super);
    function SequenceLoopNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.isRandom = false;
        _this.times = 1;
        _this.delay = 1;
        _this.num = 2;
        _this.stopLoop = false;
        _this.current = 0;
        _this.lastExecuteTime = 0;
        _this.executeTimes = 0;
        _this.microsecondToSecond = 1000000;
        _this.isFirstTriggered = false;
        return _this;
    }
    SequenceLoopNode.prototype.Run = function () {
        this.DataReset();
        if (this.isRandom) {
            // 生成[1,n]内的随机数
            this.current = Math.floor(Math.random() * this.num);
        }
        this.lastExecuteTime = light.getCurrentTime();
        this.Update(this.lastExecuteTime);
        this.addListener('update', this.Update);
    };
    SequenceLoopNode.prototype.Update = function (time) {
        var _a;
        // 超过执行总次数或停止运行则不继续执行
        if (this.executeTimes >= this.num * this.times || this.stopLoop) {
            this.removeListener('update');
            return;
        }
        if (this.isFirstTriggered || time - this.lastExecuteTime >= this.microsecondToSecond * this.delay) {
            this.current += 1;
            this.lastExecuteTime = time;
            this.executeTimes += 1;
            this.isFirstTriggered = false;
            var index = this.current % this.num;
            (_a = this["Next" + (index + 1)]) === null || _a === void 0 ? void 0 : _a.call(this);
        }
    };
    SequenceLoopNode.prototype.Stop = function () {
        this.stopLoop = true;
    };
    SequenceLoopNode.prototype.Next1 = function () { };
    SequenceLoopNode.prototype.Next2 = function () { };
    SequenceLoopNode.prototype.DataReset = function () {
        this.isFirstTriggered = true;
        this.stopLoop = false;
        this.executeTimes = 0;
        this.current = 0;
        this.removeListener('update');
    };
    SequenceLoopNode.nodeType = 'pro/sequenceLoop';
    return SequenceLoopNode;
}(Node));
light.NodeContext.registerNode(SequenceLoopNode);

var SequencePulseNode = /** @class */ (function (_super) {
    __extends(SequencePulseNode, _super);
    function SequencePulseNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.num = 2;
        _this.isRandom = false;
        _this.current = 0;
        return _this;
    }
    SequencePulseNode.prototype.Run = function () {
        var _a;
        // 首次执行
        if (this.current === 0) {
            if (this.isRandom) {
                // 生成[1,n]内的随机数
                this.current = Math.floor(Math.random() * this.num) + 1;
            }
            else {
                this.current = 1;
            }
        }
        this.current = (this.current % this.num) + 1;
        (_a = this["Next" + this.current]) === null || _a === void 0 ? void 0 : _a.call(this);
    };
    SequencePulseNode.prototype.Next1 = function () { };
    SequencePulseNode.prototype.Next2 = function () { };
    SequencePulseNode.nodeType = 'pro/sequencePulse';
    return SequencePulseNode;
}(Node));
light.NodeContext.registerNode(SequencePulseNode);

var SequenceRandomNode = /** @class */ (function (_super) {
    __extends(SequenceRandomNode, _super);
    function SequenceRandomNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.num = 2;
        return _this;
    }
    SequenceRandomNode.prototype.Run = function () {
        var _a;
        // 生成[1,n]内的随机数
        var index = Math.floor(Math.random() * this.num) + 1;
        (_a = this["Next" + index]) === null || _a === void 0 ? void 0 : _a.call(this);
    };
    SequenceRandomNode.prototype.Next1 = function () { };
    SequenceRandomNode.prototype.Next2 = function () { };
    SequenceRandomNode.nodeType = 'pro/sequenceRandom';
    return SequenceRandomNode;
}(Node));
light.NodeContext.registerNode(SequenceRandomNode);

var SequenceNode = /** @class */ (function (_super) {
    __extends(SequenceNode, _super);
    function SequenceNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.num = 2;
        return _this;
    }
    SequenceNode.prototype.Run = function () {
        var _a;
        for (var i = 0; i < this.num; i++) {
            (_a = this["Next" + (i + 1)]) === null || _a === void 0 ? void 0 : _a.call(this);
        }
    };
    SequenceNode.prototype.Next1 = function () { };
    SequenceNode.prototype.Next2 = function () { };
    SequenceNode.nodeType = 'pro/sequence';
    return SequenceNode;
}(Node));
light.NodeContext.registerNode(SequenceNode);

var SinNode = /** @class */ (function (_super) {
    __extends(SinNode, _super);
    function SinNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        return _this;
    }
    SinNode.prototype.Process = function () {
        this.Result = Math.sin(this.A);
    };
    SinNode.nodeType = 'pro/sin';
    return SinNode;
}(Node));
light.NodeContext.registerNode(SinNode);

var SquareNode = /** @class */ (function (_super) {
    __extends(SquareNode, _super);
    function SquareNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 1;
        return _this;
    }
    SquareNode.prototype.Process = function () {
        if (this.A < 0) {
            return;
        }
        this.Result = Math.sqrt(this.A);
    };
    SquareNode.nodeType = 'pro/square';
    return SquareNode;
}(Node));
light.NodeContext.registerNode(SquareNode);

var SubtractNode = /** @class */ (function (_super) {
    __extends(SubtractNode, _super);
    function SubtractNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    SubtractNode.prototype.Process = function () {
        if (this.supportedDataType) {
            switch (this.supportedDataType) {
                case 'integer':
                case 'float':
                    this.Res = this.A - this.B;
                    break;
                case 'v2':
                    this.Res = { x: this.A.x - this.B.x, y: this.A.y - this.B.y };
                    break;
                case 'v3':
                    this.Res = { x: this.A.x - this.B.x, y: this.A.y - this.B.y, z: this.A.z - this.B.z };
                    break;
                case 'v4Rect':
                    this.Res = { x: this.A.x - this.B.x, y: this.A.y - this.B.y, z: this.A.z - this.B.z, w: this.A.w - this.B.w };
                    break;
            }
        }
    };
    SubtractNode.nodeType = 'pro/subtract';
    return SubtractNode;
}(Node));
light.NodeContext.registerNode(SubtractNode);

var SwitchCaseNode = /** @class */ (function (_super) {
    __extends(SwitchCaseNode, _super);
    function SwitchCaseNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.InputValue = 0;
        _this.num = 1;
        _this.isContained = false;
        return _this;
    }
    SwitchCaseNode.prototype.Run = function () {
        var _a;
        this.isContained = false;
        for (var i = 0; i < this.num; i++) {
            if (JSON.stringify(this["value" + (i + 1)]) === JSON.stringify(this.InputValue)) {
                this.isContained = true;
                (_a = this["Next" + (i + 1)]) === null || _a === void 0 ? void 0 : _a.call(this);
            }
        }
        if (this.isContained) {
            this.Contained();
        }
        else {
            this.NotContained();
        }
        this.Next();
    };
    SwitchCaseNode.prototype.Next = function () { };
    SwitchCaseNode.prototype.Contained = function () { };
    SwitchCaseNode.prototype.NotContained = function () { };
    SwitchCaseNode.nodeType = 'pro/switchCase';
    return SwitchCaseNode;
}(Node));
light.NodeContext.registerNode(SwitchCaseNode);

var TapGestureNode = /** @class */ (function (_super) {
    __extends(TapGestureNode, _super);
    function TapGestureNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.lastUpTime = -300;
        return _this;
    }
    TapGestureNode.prototype.Run = function () {
        var _this = this;
        var tapCount = this.count;
        light.on('TouchEvent', function (event) {
            if (event.getPointerCount() !== 1) {
                return;
            }
            if (event.getAction() === TouchAction.DOWN) {
                if (event.getEventTime() - _this.lastUpTime > 300) {
                    tapCount = _this.count;
                    _this.touchDownLocation = {
                        x: event.getX(),
                        y: event.getY(),
                    };
                }
            }
            else if (event.getAction() === TouchAction.UP) {
                if (event.getEventTime() - event.getDownTime() < _this.interval
                    && Math.abs(event.getX() - _this.touchDownLocation.x) < 20
                    && Math.abs(event.getY() - _this.touchDownLocation.y) < 20) {
                    tapCount -= 1;
                    if (tapCount === 0) {
                        tapCount = _this.count;
                        _this.location = getNormalizedCoords(event);
                        _this.next();
                    }
                    _this.lastUpTime = event.getEventTime();
                }
            }
        });
    };
    TapGestureNode.prototype.next = function () { };
    TapGestureNode.nodeType = 'pro/TapGesture';
    return TapGestureNode;
}(EventNode));
light.NodeContext.registerNode(TapGestureNode);

var TimeScheduleNode = /** @class */ (function (_super) {
    __extends(TimeScheduleNode, _super);
    function TimeScheduleNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.duration = 1;
        _this.Percent = 0;
        _this.currentTime = 0;
        _this.invoked = false;
        return _this;
    }
    TimeScheduleNode.prototype.Run = function () {
        this.invoked = false;
        this.currentTime = light.getCurrentTime();
        this.Update(this.currentTime);
        this.addListener('update', this.Update);
        this.Next();
    };
    TimeScheduleNode.prototype.Update = function (time) {
        if (time - this.currentTime < this.duration * 1000000) {
            this.Percent = (time - this.currentTime) / (this.duration * 1000000);
            this.Continuous();
        }
        else if (!this.invoked) {
            this.Percent = 1;
            this.invoked = true;
            this.removeListener('update');
            this.Finish();
        }
    };
    TimeScheduleNode.prototype.Next = function () { };
    TimeScheduleNode.prototype.Continuous = function () { };
    TimeScheduleNode.prototype.Finish = function () { };
    TimeScheduleNode.nodeType = 'pro/timeSchedule';
    return TimeScheduleNode;
}(Node));
light.NodeContext.registerNode(TimeScheduleNode);

var TwoPointGestureNode = /** @class */ (function (_super) {
    __extends(TwoPointGestureNode, _super);
    function TwoPointGestureNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.angle = 0;
        _this.centerPoint = { x: 0, y: 0 };
        _this.lastScale = 1;
        return _this;
    }
    TwoPointGestureNode.prototype.Run = function () {
        var _this = this;
        light.on('TouchEvent', function (event) {
            if (event.getPointerCount() !== 2) {
                return;
            }
            if (event.getAction() === TouchAction.POINTER_DOWN) {
                _this.beginDistance = _this.calculateDistance(event);
                _this.lastAngle = _this.calculateAngle(event);
            }
            else if (event.getAction() === TouchAction.MOVE) {
                // 计算缩放
                var distance = _this.calculateDistance(event);
                _this.scale = _this.lastScale * (distance / _this.beginDistance);
                // 计算旋转
                var currentAngle = _this.calculateAngle(event);
                if (currentAngle - _this.lastAngle > 100) {
                    currentAngle -= 180;
                }
                else if (currentAngle - _this.lastAngle < -100) {
                    currentAngle += 180;
                }
                _this.angle += currentAngle - _this.lastAngle;
                _this.lastAngle = currentAngle;
                // 计算中心点坐标
                _this.centerPoint = _this.calculateCenter(event);
            }
            else if (event.getAction() === TouchAction.UP) {
                _this.lastScale = _this.scale;
            }
        });
    };
    TwoPointGestureNode.prototype.next = function () { };
    TwoPointGestureNode.prototype.calculateDistance = function (event) {
        var dx = event.getX(0) - event.getX(1);
        var dy = event.getY(0) - event.getY(1);
        return Math.sqrt(dx * dx + dy * dy);
    };
    TwoPointGestureNode.prototype.calculateCenter = function (event) {
        var centerX = (event.getX(0) + event.getX(1)) / 2;
        var centerY = (event.getY(0) + event.getY(1)) / 2;
        return { x: centerX, y: centerY };
    };
    TwoPointGestureNode.prototype.calculateAngle = function (event) {
        var x1 = event.getX(0);
        var y1 = event.getY(0);
        var x2 = event.getX(1);
        var y2 = event.getY(1);
        var dx = x2 - x1;
        var dy = y2 - y1;
        if (Math.abs(dx) < 1e-3) {
            return y1 >= y2 ? 90 : 270;
        }
        var tan = Math.atan(dy / dx);
        var angle = (tan * 180) / Math.PI;
        if (tan > 0) {
            return x1 > x2 ? angle : angle + 180;
        }
        return x1 > x2 ? angle + 360 : angle + 180;
    };
    TwoPointGestureNode.nodeType = 'pro/TwoPointGesture';
    return TwoPointGestureNode;
}(EventNode));
light.NodeContext.registerNode(TwoPointGestureNode);

var UnprojectNode = /** @class */ (function (_super) {
    __extends(UnprojectNode, _super);
    function UnprojectNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    UnprojectNode.prototype.Process = function () {
        var entityId = this.cameraComponent.entityId;
        var cameraEntity = this.getEntityById(entityId);
        if (!cameraEntity) {
            return;
        }
        var position = light.CameraUtils.UnProject(this.entityManager, cameraEntity, new light.Vec2(this.input.x, this.input.y), this.depth);
        this.output = {
            x: position.x,
            y: position.y,
            z: position.z,
        };
    };
    UnprojectNode.nodeType = 'pro/Unproject';
    return UnprojectNode;
}(DataProcessNode));
light.NodeContext.registerNode(UnprojectNode);

var ValueCalculateNode = /** @class */ (function (_super) {
    __extends(ValueCalculateNode, _super);
    function ValueCalculateNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    ValueCalculateNode.prototype.Process = function () {
        if (this.supportedDataType) {
            switch (this.supportedDataType) {
                case 'integer':
                    this.Sum = this.A + this.B;
                    this.Diff = this.A - this.B;
                    this.Multiply = this.A * this.B;
                    this.Divide = this.B === 0 ? 0 : Math.floor(this.A / this.B);
                    this.Complement = this.B === 0 ? 0 : this.A % this.B;
                    break;
                case 'float':
                    this.Sum = this.A + this.B;
                    this.Diff = this.A - this.B;
                    this.Multiply = this.A * this.B;
                    this.Divide = this.B === 0 ? 0 : this.A / this.B;
                    this.Complement = this.B === 0 ? 0 : this.A % this.B;
            }
        }
    };
    ValueCalculateNode.nodeType = 'pro/valueCalculate';
    return ValueCalculateNode;
}(Node));
light.NodeContext.registerNode(ValueCalculateNode);

var ValueDelayNode = /** @class */ (function (_super) {
    __extends(ValueDelayNode, _super);
    function ValueDelayNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.value = 1;
        _this.duration = 1;
        _this.triggerTime = 0;
        return _this;
    }
    ValueDelayNode.prototype.Run = function () {
        this.triggerTime = light.getCurrentTime();
        this.Update(this.triggerTime);
        this.addListener('update', this.Update);
        this.Next();
    };
    ValueDelayNode.prototype.Next = function () { };
    ValueDelayNode.prototype.Continuous = function () { };
    ValueDelayNode.prototype.Update = function (time) {
        if (time - this.triggerTime >= this.duration * 1000000) {
            this.delayValue = this.value;
            this.removeListener('update');
            this.Continuous();
        }
    };
    ValueDelayNode.nodeType = 'pro/ValueDelay';
    return ValueDelayNode;
}(Node));
light.NodeContext.registerNode(ValueDelayNode);

var ValueObserveNode = /** @class */ (function (_super) {
    __extends(ValueObserveNode, _super);
    function ValueObserveNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.valueType = 'number';
        _this.inValue = 2;
        _this.outValue = 2;
        return _this;
    }
    ValueObserveNode.prototype.Process = function () {
        this.outValue = this.inValue;
    };
    ValueObserveNode.nodeType = 'pro/valueObserve';
    return ValueObserveNode;
}(DataProcessNode));
light.NodeContext.registerNode(ValueObserveNode);

var ValueScopeNode = /** @class */ (function (_super) {
    __extends(ValueScopeNode, _super);
    function ValueScopeNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ValueScopeNode.prototype.Process = function () {
        if (this.inputValue < this.min) {
            this.resValue = this.min;
        }
        else if (this.inputValue > this.max) {
            this.resValue = this.max;
        }
        else {
            this.resValue = this.inputValue;
        }
    };
    ValueScopeNode.nodeType = 'pro/valueScope';
    return ValueScopeNode;
}(Node));
light.NodeContext.registerNode(ValueScopeNode);

var AnimationControlNode = /** @class */ (function (_super) {
    __extends(AnimationControlNode, _super);
    function AnimationControlNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.loopCount = 1;
        _this.progress = 0;
        _this.isPlayTogether = false;
        _this.onUpdate = function () {
            if (_this.loopCount <= 0) {
                return;
            }
            var entity = _this.getEntityById(_this.entityId);
            if (!entity) {
                _this.removeListener('update');
                return;
            }
            var controller = entity.getComponent(light.AnimationController);
            var timeCtrls = entity.getComponent(light.TimeControlList);
            if (timeCtrls) {
                controller.clips.forEach(function (clip, index) {
                    if (_this.clipIndex === index && clip.state === 1 /* Playing */) {
                        if (timeCtrls.timeControlArray.get(index).currentTime >= _this.loopCount * clip.duration) {
                            clip.state = 2 /* Paused */;
                            clip.progress = clip.duration;
                            _this.detachEvent();
                            _this.Finish();
                        }
                    }
                });
            }
            else {
                controller.clips.forEach(function (clip, index) {
                    if (_this.clipIndex === index && clip.state === 1 /* Playing */) {
                        if ((light.getCurrentTime() - clip.startTime) >= _this.loopCount * clip.duration) {
                            clip.state = 2 /* Paused */;
                            clip.progress = clip.duration;
                            _this.detachEvent();
                            _this.Finish();
                        }
                    }
                });
            }
        };
        return _this;
    }
    AnimationControlNode.prototype.Play = function () {
        var _this = this;
        var _a, _b;
        var entity = this.getEntityById(this.entityId);
        if (!entity) {
            return;
        }
        var controller = entity.getComponent(light.AnimationController);
        if (!controller) {
            return;
        }
        var timeCtrls = entity.getComponent(light.TimeControlList);
        if (timeCtrls) {
            (_a = controller.clips) === null || _a === void 0 ? void 0 : _a.forEach(function (clip, index) {
                if (_this.clipIndex === index) {
                    clip.state = 1 /* Playing */;
                    timeCtrls.timeControlArray.get(index).pause = false;
                    timeCtrls.timeControlArray.get(index).loopCount = _this.loopCount - 1;
                    timeCtrls.timeControlArray.get(index).currentTime = _this.progress;
                    clip.progress = 1;
                }
                else if (!_this.isPlayTogether) {
                    clip.state = 0 /* Stopped */;
                    timeCtrls.timeControlArray.get(index).pause = true;
                }
            });
        }
        else {
            (_b = controller.clips) === null || _b === void 0 ? void 0 : _b.forEach(function (clip, index) {
                if (_this.clipIndex === index) {
                    clip.state = 1 /* Playing */;
                    clip.progress = _this.progress;
                    clip.startTime = light.getCurrentTime() - clip.progress;
                }
                else if (!_this.isPlayTogether) {
                    clip.state = 0 /* Stopped */;
                }
            });
        }
        this.detachEvent();
        this.addListener('update', this.onUpdate);
        this.Next();
    };
    AnimationControlNode.prototype.Pause = function () {
        var _this = this;
        var entity = this.getEntityById(this.entityId);
        if (!entity) {
            return;
        }
        var controller = entity.getComponent(light.AnimationController);
        if (!controller) {
            return;
        }
        var timeCtrls = entity.getComponent(light.TimeControlList);
        controller.clips.forEach(function (clip, index) {
            if (_this.clipIndex === index) {
                clip.state = 2 /* Paused */;
                if (timeCtrls) {
                    timeCtrls.timeControlArray.get(index).pause = true;
                }
            }
        });
        this.detachEvent();
        this.Next();
    };
    AnimationControlNode.prototype.Continue = function () {
        var _this = this;
        var entity = this.getEntityById(this.entityId);
        if (!entity) {
            return;
        }
        var controller = entity.getComponent(light.AnimationController);
        if (!controller) {
            return;
        }
        var timeCtrls = entity.getComponent(light.TimeControlList);
        controller.clips.forEach(function (clip, index) {
            if (_this.clipIndex === index) {
                clip.state = 1 /* Playing */;
                if (timeCtrls) {
                    timeCtrls.timeControlArray.get(index).pause = false;
                }
            }
        });
        this.detachEvent();
        this.addListener('update', this.onUpdate);
    };
    AnimationControlNode.prototype.Stop = function () {
        var _this = this;
        var entity = this.getEntityById(this.entityId);
        if (!entity) {
            return;
        }
        var controller = entity.getComponent(light.AnimationController);
        if (!controller) {
            return;
        }
        var timeCtrls = entity.getComponent(light.TimeControlList);
        controller.clips.forEach(function (clip, index) {
            if (_this.clipIndex === index) {
                clip.state = 0 /* Stopped */;
                if (timeCtrls) {
                    timeCtrls.timeControlArray.get(index).pause = true;
                    timeCtrls.timeControlArray.get(index).currentTime = 0;
                    // clip.progress != timeCtrl.currentTime 触发刷新
                    clip.progress = 1;
                }
            }
        });
        this.detachEvent();
        this.Next();
    };
    AnimationControlNode.prototype.Next = function () { };
    AnimationControlNode.prototype.Finish = function () { };
    AnimationControlNode.prototype.detachEvent = function () {
        this.removeListener('update');
    };
    AnimationControlNode.nodeType = 'pro/AnimationControl';
    return AnimationControlNode;
}(Node));
light.NodeContext.registerNode(AnimationControlNode);

var ParticleControlNode = /** @class */ (function (_super) {
    __extends(ParticleControlNode, _super);
    function ParticleControlNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ParticleControlNode.prototype.Run = function () {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k, _l, _m, _o, _p, _q, _r;
        if (!this.entityId)
            return;
        var entity = this.getEntityById(this.entityId);
        showEntities([this.entityId], this.entityManager);
        if (entity) {
            var particle = entity.getComponent(light.ParticleEffect);
            if (particle) {
                var vec3 = new light.Vec3((_b = (_a = this.xInput) !== null && _a !== void 0 ? _a : particle.emitterLength.x) !== null && _b !== void 0 ? _b : 0, (_d = (_c = this.yInput) !== null && _c !== void 0 ? _c : particle.emitterLength.y) !== null && _d !== void 0 ? _d : 0, (_f = (_e = this.zInput) !== null && _e !== void 0 ? _e : particle.emitterLength.z) !== null && _f !== void 0 ? _f : 0);
                var dvec3 = new light.Vec3((_h = (_g = this.directionXInput) !== null && _g !== void 0 ? _g : particle.emissionVector.x) !== null && _h !== void 0 ? _h : 0, (_k = (_j = this.directionYInput) !== null && _j !== void 0 ? _j : particle.emissionVector.y) !== null && _k !== void 0 ? _k : 0, (_m = (_l = this.directionZInput) !== null && _l !== void 0 ? _l : particle.emissionVector.z) !== null && _m !== void 0 ? _m : 0);
                particle.emitterLength = vec3;
                particle.emissionVector = dvec3;
                particle.speed = (_p = (_o = this.speedInput) !== null && _o !== void 0 ? _o : particle.speed) !== null && _p !== void 0 ? _p : 0.1;
                particle.maxParticles = (_r = (_q = this.maxInput) !== null && _q !== void 0 ? _q : particle.maxParticles) !== null && _r !== void 0 ? _r : 0;
                particle.speedRandom = false;
                particle.emissionVectorRandom = false;
                particle.updateComponentData = true;
            }
        }
        this.Next();
    };
    ParticleControlNode.prototype.Pause = function () {
    };
    ParticleControlNode.prototype.Continue = function () {
    };
    ParticleControlNode.prototype.StopInput = function () {
        var entity = this.getEntityById(this.entityId);
        if (entity) {
            var particle = entity.getComponent(light.ParticleEffect);
            particle.enabled = false;
            this.AfterStop();
        }
    };
    ParticleControlNode.prototype.AfterStop = function () { };
    ParticleControlNode.prototype.Next = function () { };
    ParticleControlNode.nodeType = 'pro/Particle';
    return ParticleControlNode;
}(Node));
light.NodeContext.registerNode(ParticleControlNode);

var Rotation3DAnimationNode = /** @class */ (function (_super) {
    __extends(Rotation3DAnimationNode, _super);
    function Rotation3DAnimationNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.stopRotation = false;
        // 记录时间信息
        _this.lastTime = -1;
        _this.startTime = -1;
        _this.currentTimes = 0;
        _this.isInit = false;
        _this.isContinue = false;
        _this.isFinish = false;
        _this.lastAngle = 0;
        _this.onLightUpdate = function (time) {
            if (_this.isFinish) {
                return;
            }
            var _time = time / 1000000;
            var onceSpinTime = _this.onceSpinTime;
            if (_this.isContinue) {
                _this.startTime = _time - (_this.lastTime + _this.currentTimes * onceSpinTime);
                _this.isContinue = false;
                return;
            }
            if (_this.lastTime < 0) {
                _this.lastTime = 0;
                _this.startTime = _time;
                _this.isInit = true;
                return;
            }
            var durationTime = _time - _this.startTime;
            if (durationTime <= 0) {
                return;
            }
            _this.currentTimes = Math.floor(durationTime / onceSpinTime);
            var spinTimes = _this.spinTimes;
            if (_this.currentTimes >= spinTimes) {
                _this.isFinish = true;
                _this.UpdateObjectByTime(onceSpinTime);
                _this.Rotating();
                _this.detachEvent();
                _this.Stop();
                _this.Finish();
                return;
            }
            var currentTime = durationTime % onceSpinTime;
            _this.UpdateObjectByTime(currentTime);
            _this.Rotating();
            _this.lastTime = currentTime;
        };
        return _this;
    }
    Rotation3DAnimationNode.prototype.Start = function () {
        this.Cancel();
        this.StartRotate();
        this.addListener('update', this.onLightUpdate);
    };
    Rotation3DAnimationNode.prototype.Stop = function () {
        this.detachEvent();
    };
    Rotation3DAnimationNode.prototype.Continue = function () {
        this.isContinue = true;
        this.addListener('update', this.onLightUpdate);
    };
    Rotation3DAnimationNode.prototype.Cancel = function () {
        this.dataReset();
        this.reset();
    };
    Rotation3DAnimationNode.prototype.StartRotate = function () { };
    Rotation3DAnimationNode.prototype.Rotating = function () { };
    Rotation3DAnimationNode.prototype.Finish = function () { };
    Rotation3DAnimationNode.prototype.UpdateObject = function (currentAngle) {
        var _a;
        if (this.component) {
            var entity = this.getEntityById((_a = this.component) === null || _a === void 0 ? void 0 : _a.entityId);
            if (!entity) {
                return;
            }
            var comp = entity.getComponent(light.BasicTransform);
            if (comp) {
                var transform = new Vector3(comp.position.x, comp.position.y, comp.position.z);
                var scale = new Vector3(comp.scale.x, comp.scale.y, comp.scale.z);
                var rotation = new Quaternion(comp.rotation.x, comp.rotation.y, comp.rotation.z, comp.rotation.w);
                var matrix = new Matrix4();
                matrix.compose(transform, rotation, scale);
                if (this.isInit) {
                    this.initMatrix = new Matrix4().copy(matrix);
                    this.isInit = false;
                }
                var spinPoint = this.spinPoint;
                var spinPointTransform = new Matrix4().setPosition(new Vector3(spinPoint.x, spinPoint.y, spinPoint.z));
                matrix.premultiply(new Matrix4().getInverse(spinPointTransform));
                var spinAxis = this.spinAxis;
                matrix.premultiply(new Matrix4().makeRotationAxis(new Vector3(spinAxis.x, spinAxis.y, spinAxis.z).normalize(), (currentAngle - this.lastAngle) / 180 * Math.PI));
                matrix.premultiply(spinPointTransform);
                matrix.decompose(transform, rotation, scale);
                comp.SetPosition(new light.Vec3(transform.x, transform.y, transform.z));
                this.CurrentPosition = new light.Vec3(transform.x, transform.y, transform.z);
                if (!this.stopRotation) {
                    comp.SetRotation(new light.Quat(rotation.w, rotation.x, rotation.y, rotation.z));
                }
                this.CurrentRotation = comp.rotation;
                comp.SetScale(new light.Vec3(scale.x, scale.y, scale.z));
                this.lastAngle = currentAngle;
            }
        }
    };
    Rotation3DAnimationNode.prototype.dataReset = function () {
        this.detachEvent();
        this.lastTime = -1;
        this.startTime = -1;
        this.currentTimes = 0;
        this.isInit = false;
        this.isContinue = false;
        this.isFinish = false;
        this.lastAngle = 0;
    };
    Rotation3DAnimationNode.prototype.detachEvent = function () {
        this.removeListener('update');
    };
    Rotation3DAnimationNode.prototype.reset = function () {
        var _a;
        if (this.component) {
            var entity = this.getEntityById((_a = this.component) === null || _a === void 0 ? void 0 : _a.entityId);
            if (!entity) {
                return;
            }
            var comp = entity.getComponent(light.BasicTransform);
            if (comp && this.initMatrix) {
                var transform = new Vector3();
                var scale = new Vector3();
                var rotation = new Quaternion();
                this.initMatrix.decompose(transform, rotation, scale);
                comp.SetPosition(new light.Vec3(transform.x, transform.y, transform.z));
                comp.SetRotation(new light.Quat(rotation.w, rotation.x, rotation.y, rotation.z));
                comp.SetScale(new light.Vec3(scale.x, scale.y, scale.z));
            }
        }
    };
    Rotation3DAnimationNode.prototype.UpdateObjectByTime = function (currentTime) {
        var currentAngle = getGradientValue(this._smoothType, currentTime, 0, this.spinAngle, this.onceSpinTime);
        this.UpdateObject(currentAngle);
    };
    Rotation3DAnimationNode.nodeType = 'pro/rotation3D';
    return Rotation3DAnimationNode;
}(Node));
light.NodeContext.registerNode(Rotation3DAnimationNode);

var Rotation2DAnimationNode = /** @class */ (function (_super) {
    __extends(Rotation2DAnimationNode, _super);
    function Rotation2DAnimationNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.notSelfRotation = false;
        _this.CurrentPosition = { x: 0, y: 0 };
        // 记录时间信息
        _this.lastTime = -1;
        _this.startTime = -1;
        _this.currentTimes = 0;
        // 记录状态信息
        _this.isInit = false;
        _this.isContinue = false;
        _this.isFinish = false;
        _this.lastAngle = 0;
        _this.onLightUpdate = function (time) {
            if (_this.isFinish) {
                return;
            }
            var _time = time / 1000000;
            var onceSpinTime = _this.onceSpinTime;
            if (_this.isContinue) {
                _this.startTime = _time - (_this.lastTime + _this.currentTimes * onceSpinTime);
                _this.isContinue = false;
                return;
            }
            if (_this.lastTime < 0) {
                _this.lastTime = 0;
                _this.startTime = _time;
                _this.isInit = true;
                return;
            }
            var durationTime = _time - _this.startTime;
            if (durationTime <= 0) {
                return;
            }
            _this.currentTimes = Math.floor(durationTime / onceSpinTime);
            var spinTimes = _this.spinTimes;
            if (_this.currentTimes >= spinTimes) {
                _this.isFinish = true;
                _this.UpdateObjectByTime(onceSpinTime);
                _this.Rotating();
                _this.detachEvent();
                _this.Stop();
                _this.Finish();
                return;
            }
            var currentTime = durationTime % onceSpinTime;
            _this.UpdateObjectByTime(currentTime);
            _this.Rotating();
            _this.lastTime = currentTime;
        };
        return _this;
    }
    Rotation2DAnimationNode.prototype.Start = function () {
        this.DataReset();
        this.onLightUpdate(light.getCurrentTime());
        this.addListener('update', this.onLightUpdate);
        this.StartRotate();
    };
    Rotation2DAnimationNode.prototype.Stop = function () {
        this.detachEvent();
        this.StopRotate();
    };
    Rotation2DAnimationNode.prototype.Continue = function () {
        this.isContinue = true;
        this.onLightUpdate(light.getCurrentTime());
        this.addListener('update', this.onLightUpdate);
    };
    Rotation2DAnimationNode.prototype.Cancel = function () {
        this.DataReset();
        this.StopRotate();
    };
    Rotation2DAnimationNode.prototype.StartRotate = function () { };
    Rotation2DAnimationNode.prototype.Rotating = function () { };
    Rotation2DAnimationNode.prototype.StopRotate = function () { };
    Rotation2DAnimationNode.prototype.Finish = function () { };
    Rotation2DAnimationNode.prototype.UpdateObject = function (currentAngle) {
        var _a;
        if (this.component) {
            var entity = this.getEntityById((_a = this.component) === null || _a === void 0 ? void 0 : _a.entityId);
            if (!entity) {
                return;
            }
            var comp = entity.getComponent(light.ScreenTransform);
            if (comp) {
                var center = this.spinPoint;
                var pos_x = comp.anchor.left - center.x;
                var pos_y = comp.anchor.bottom - center.y;
                var cos_angle = Math.cos((currentAngle - this.lastAngle) / 180 * Math.PI);
                var sin_angle = Math.sin((currentAngle - this.lastAngle) / 180 * Math.PI);
                var x = pos_x * cos_angle - pos_y * sin_angle;
                var y = pos_x * sin_angle + pos_y * cos_angle;
                comp.anchor.bottom = y + center.y;
                comp.anchor.top = y + center.y;
                comp.anchor.left = x + center.x;
                comp.anchor.right = x + center.x;
                if (!this.notSelfRotation) {
                    var quaternion = eulerToQuaternion(0, 0, currentAngle);
                    comp.rotation.x = quaternion.x;
                    comp.rotation.y = quaternion.y;
                    comp.rotation.z = quaternion.z;
                    comp.rotation.w = quaternion.w;
                }
                this.CurrentPosition = { x: comp.anchor.left, y: comp.anchor.top };
                this.lastAngle = currentAngle;
            }
        }
    };
    Rotation2DAnimationNode.prototype.DataReset = function () {
        this.detachEvent();
        this.lastTime = -1;
        this.startTime = -1;
        this.currentTimes = 0;
        this.CurrentPosition = { x: 0, y: 0 };
        this.lastAngle = 0;
        this.isInit = false;
        this.isContinue = false;
        this.isFinish = false;
    };
    Rotation2DAnimationNode.prototype.detachEvent = function () {
        this.removeListener('update');
    };
    Rotation2DAnimationNode.prototype.UpdateObjectByTime = function (currentTime) {
        var currentAngle = getGradientValue(this._smoothType, currentTime, 0, this.spinAngle, this.onceSpinTime);
        this.UpdateObject(currentAngle);
    };
    Rotation2DAnimationNode.nodeType = 'pro/rotation2D';
    return Rotation2DAnimationNode;
}(Node));
light.NodeContext.registerNode(Rotation2DAnimationNode);

var PlayerState;
(function (PlayerState) {
    PlayerState[PlayerState["Init"] = 0] = "Init";
    PlayerState[PlayerState["Stop"] = 1] = "Stop";
    PlayerState[PlayerState["Playing"] = 2] = "Playing";
    PlayerState[PlayerState["Paused"] = 3] = "Paused";
})(PlayerState || (PlayerState = {}));
var Player = /** @class */ (function () {
    function Player(entityManager, entityId) {
        // 暂停时记录的时间
        this.pauseTime = 0;
        // timeControl的lastTime，用来记录播放状态
        this.timeControlLastTime = 0;
        this.entityManager = entityManager;
        this.entity = entityManager.getEntityById(entityId);
        this.timeOffset = this.entity.getComponent(light.TimeOffset);
        this.timeControl = this.entity.getComponent(light.TimeControl);
        this.duration = Player.getDuration(entityManager, this.entity);
        this.startTime = 0;
        this.endTime = this.duration;
        this.loopCount = 1;
        this.remainingLoopCount = 1;
        this.state = PlayerState.Init;
        // 初始化时暂停播放, 修改播放次数为1次
        this.timeOffset.loopCount = 0;
        this.timeControl.pause = true;
    }
    Player.getDuration = function (entityManager, entity) {
        var _a;
        var inputSourcesString = light.AIDataUtils.GetDataFromDataCenter(entityManager, 'inputSources');
        var inputSources = JSON.parse(inputSourcesString);
        for (var _i = 0, _b = ['Image', 'PAGAsset', 'AudioSource']; _i < _b.length; _i++) {
            var type = _b[_i];
            var component = entity.getComponent(light[type]);
            if (component) {
                var source = inputSources[component.src];
                if ((source === null || source === void 0 ? void 0 : source.duration) > 0) {
                    return source.duration;
                }
                if (((_a = source === null || source === void 0 ? void 0 : source.timeRange) === null || _a === void 0 ? void 0 : _a.duration) > 0) {
                    return source.timeRange.duration;
                }
            }
        }
        return 0;
    };
    Player.prototype.setStartTime = function (time) {
        this.duration = Player.getDuration(this.entityManager, this.entity);
        this.startTime = time % this.duration;
    };
    Player.prototype.setEndTime = function (time) {
        this.duration = Player.getDuration(this.entityManager, this.entity);
        this.endTime = time > this.startTime ? time : this.duration;
    };
    Player.prototype.setLoopCount = function (count) {
        this.loopCount = count;
        this.remainingLoopCount = count;
    };
    Player.prototype.setVisibleWhileOverTime = function (flag) {
        this.timeOffset.visibleWhileOverTime = flag;
        this.timeControl.visibleWhileOverTime = flag;
    };
    Player.prototype.play = function (onProgress, onFinished) {
        this.reset(light.getCurrentTime());
        this.state = PlayerState.Playing;
        this.timeControl.pause = false;
        this.onFinished = onFinished;
        this.onProgress = onProgress;
        light.once('update', this.updatePlayer.bind(this));
    };
    Player.prototype.pause = function () {
        if (this.state === PlayerState.Stop) {
            return;
        }
        this.timeControl.pause = true;
        this.pauseTime = light.getCurrentTime();
        this.state = PlayerState.Paused;
    };
    Player.prototype.continue = function () {
        if (this.state === PlayerState.Stop) {
            return;
        }
        this.timeControl.pause = false;
        if (this.pauseTime > 0) {
            var pauseDuringTime = light.getCurrentTime() - this.pauseTime;
            this.timeOffset.duration += pauseDuringTime;
        }
        this.state = PlayerState.Playing;
        light.once('update', this.updatePlayer.bind(this));
    };
    Player.prototype.stop = function () {
        this.timeControl.pause = true;
        this.state = PlayerState.Stop;
    };
    Player.prototype.updatePlayer = function (currentTime) {
        this.loop(currentTime);
        if (this.timeControl.currentTime < 0) {
            this.onProgress(0);
        }
        else {
            var progress = (this.timeControl.currentTime - this.startTime) / (this.endTime - this.startTime);
            this.onProgress(progress);
        }
        if (this.state === PlayerState.Stop) {
            return;
        }
        light.once('update', this.updatePlayer.bind(this));
    };
    Player.prototype.loop = function (currentTime) {
        if (this.state !== PlayerState.Playing) {
            // 暂停或停止
            return;
        }
        if (this.endTime - this.timeControl.currentTime > 1000
            && this.timeControl.currentTime - this.timeControlLastTime >= 0) {
            this.timeControlLastTime = this.timeControl.currentTime;
            // 播放中，但未播放至末尾，跳出循环继续播放
            return;
        }
        this.timeControl.setCurrentTime(this.endTime);
        // 已经播到末尾了 需要重新置为最开始
        this.onProgress(1);
        this.remainingLoopCount -= 1;
        if (this.remainingLoopCount <= 0) {
            // 循环播放结束
            this.stop();
            this.onFinished();
            return;
        }
        this.pauseTime = 0;
        this.timeControlLastTime = 0;
        this.resetComponent(currentTime);
    };
    Player.prototype.reset = function (currentTime) {
        this.resetComponent(currentTime);
        this.pauseTime = 0;
        this.timeControlLastTime = 0;
        this.remainingLoopCount = this.loopCount;
    };
    Player.prototype.resetComponent = function (currentTime) {
        if (this.timeOffset) {
            this.timeOffset.duration = this.endTime - this.startTime;
            this.timeOffset.startOffset = currentTime;
            this.timeOffset.loopCount = 0;
        }
        if (this.timeControl) {
            this.timeControl.reset();
            this.timeControl.pause = false;
            this.timeControl.setCurrentTime(this.startTime);
        }
    };
    return Player;
}());

var SequenceFrameNode = /** @class */ (function (_super) {
    __extends(SequenceFrameNode, _super);
    function SequenceFrameNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object.defineProperty(SequenceFrameNode.prototype, "player", {
        get: function () {
            if (this._player) {
                return this._player;
            }
            if (!this.component || !this.getEntityById(this.component.entityId)) {
                return null;
            }
            this._player = new Player(this.entityManager, this.component.entityId);
            this._player.setStartTime(this.startTime * 1000000);
            this._player.setEndTime(this.endTime * 1000000);
            this._player.setLoopCount(this.loopCount);
            this._player.setVisibleWhileOverTime(this.stopOnLastFrame);
            return this._player;
        },
        enumerable: false,
        configurable: true
    });
    SequenceFrameNode.prototype.play = function () {
        var _this = this;
        if (!this.player) {
            return;
        }
        this.player.play(function (progress) {
            _this.progress = progress;
        }, function () {
            _this.onEnd();
        });
        this.onStart();
    };
    SequenceFrameNode.prototype.continue = function () {
        if (!this.player) {
            return;
        }
        this.player.continue();
    };
    SequenceFrameNode.prototype.pause = function () {
        if (!this.player) {
            return;
        }
        this.player.pause();
    };
    SequenceFrameNode.prototype.stop = function () {
        if (!this.player) {
            return;
        }
        this.player.stop();
    };
    SequenceFrameNode.prototype.onStart = function () { };
    SequenceFrameNode.prototype.onEnd = function () { };
    SequenceFrameNode.nodeType = 'pro/SequenceFrame';
    return SequenceFrameNode;
}(Node));
light.NodeContext.registerNode(SequenceFrameNode);

var TexturePulseNode = /** @class */ (function (_super) {
    __extends(TexturePulseNode, _super);
    function TexturePulseNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.num = 2;
        _this.isRandom = false;
        _this.current = 0;
        return _this;
    }
    TexturePulseNode.prototype.Run = function () {
        // 首次执行
        if (this.current === 0) {
            if (this.isRandom) {
                // 生成[1,n]内的随机数
                this.current = Math.floor(Math.random() * this.num);
            }
            else {
                this.current = 0;
            }
        }
        this.output = this["fileKey" + ((this.current % this.num) + 1)] || '';
        this.current += 1;
        this.Next();
    };
    TexturePulseNode.prototype.Next = function () { };
    TexturePulseNode.nodeType = 'pro/texturePulse';
    return TexturePulseNode;
}(Node));
light.NodeContext.registerNode(TexturePulseNode);

var TextureRandomNode = /** @class */ (function (_super) {
    __extends(TextureRandomNode, _super);
    function TextureRandomNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.num = 1;
        return _this;
    }
    TextureRandomNode.prototype.Run = function () {
        // 生成[1,n]内的随机数
        var index = Math.floor(Math.random() * this.num) + 1;
        this.output = this["fileKey" + index] || '';
        this.Next();
    };
    TextureRandomNode.prototype.Next = function () { };
    TextureRandomNode.nodeType = 'pro/textureRandom';
    return TextureRandomNode;
}(Node));
light.NodeContext.registerNode(TextureRandomNode);

var TextureLoopNode = /** @class */ (function (_super) {
    __extends(TextureLoopNode, _super);
    function TextureLoopNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.isRandom = false;
        _this.times = 1;
        _this.delay = 1;
        _this.num = 2;
        _this.stopLoop = false;
        _this.current = 0;
        _this.lastExecuteTime = 0;
        _this.executeTimes = 0;
        _this.microsecondToSecond = 1000000;
        _this.isFirstTriggered = true;
        return _this;
    }
    TextureLoopNode.prototype.Run = function () {
        this.DataReset();
        if (this.isRandom === true) {
            // 生成[1,n]内的随机数
            this.current = Math.floor(Math.random() * this.num);
        }
        else {
            this.current = 0;
        }
        this.lastExecuteTime = light.getCurrentTime();
        this.Update(this.lastExecuteTime);
        this.addListener('update', this.Update);
        this.Next();
    };
    TextureLoopNode.prototype.Update = function (time) {
        // 超过执行总次数或停止运行则不继续执行
        if (this.executeTimes >= this.num * this.times || this.stopLoop) {
            this.removeListener('update');
            return;
        }
        if (this.isFirstTriggered || time - this.lastExecuteTime >= this.microsecondToSecond * this.delay) {
            this.output = this["fileKey" + ((this.current % this.num) + 1)] || '';
            this.current += 1;
            this.lastExecuteTime = time;
            this.executeTimes += 1;
            this.isFirstTriggered = false;
            this.Loop();
        }
    };
    TextureLoopNode.prototype.Stop = function () {
        this.stopLoop = true;
    };
    TextureLoopNode.prototype.Next = function () { };
    TextureLoopNode.prototype.Loop = function () { };
    TextureLoopNode.prototype.DataReset = function () {
        this.current = 0;
        this.executeTimes = 0;
        this.stopLoop = false;
        this.isFirstTriggered = true;
        this.removeListener('update');
    };
    TextureLoopNode.nodeType = 'pro/textureLoop';
    return TextureLoopNode;
}(Node));
light.NodeContext.registerNode(TextureLoopNode);

var GuideVideoNode = /** @class */ (function (_super) {
    __extends(GuideVideoNode, _super);
    function GuideVideoNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    GuideVideoNode.prototype.Run = function () {
        // 视频路径
        var inputSourcesString = light.AIDataUtils.GetDataFromDataCenter(this.entityManager, 'inputSources');
        var inputSources = JSON.parse(inputSourcesString);
        var source = inputSources[this.videoAssetKey];
        // 发送事件
        var result = new light.MapStringString();
        result.set('lightasset.behaviour.guide.startvideo', RootPathGetter() + source.path);
        this.eventManager.emit(new light.LightAssetStatusEvent(this.entityManager, 'lightAsset.process.result', result));
        this.Next();
    };
    GuideVideoNode.prototype.Next = function () { };
    GuideVideoNode.nodeType = 'pro/GuideVideo';
    return GuideVideoNode;
}(EventNode));
light.NodeContext.registerNode(GuideVideoNode);

var TipsNode = /** @class */ (function (_super) {
    __extends(TipsNode, _super);
    function TipsNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    TipsNode.prototype.Run = function () {
        // 判断 icon 类型
        var inputSourcesString = light.AIDataUtils.GetDataFromDataCenter(this.entityManager, 'inputSources');
        var inputSources = JSON.parse(inputSourcesString);
        var source = inputSources[this.tipsAssetKey];
        var type = this.tipsAssetKey && source.type === 'PAGFileData' ? 1 : 0;
        this.eventManager.emit(new light.TipsEvent(this.text ? this.text : '', this.tipsAssetKey ? this.tipsAssetKey : '', true, type, this.duration * 1000, this.entityManager));
        this.Next();
    };
    TipsNode.prototype.Next = function () { };
    TipsNode.nodeType = 'pro/Tips';
    return TipsNode;
}(EventNode));
light.NodeContext.registerNode(TipsNode);

var AudioPlayerNode = /** @class */ (function (_super) {
    __extends(AudioPlayerNode, _super);
    function AudioPlayerNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object.defineProperty(AudioPlayerNode.prototype, "player", {
        get: function () {
            if (this._player) {
                return this._player;
            }
            if (!this.component || !this.getEntityById(this.component.entityId)) {
                return;
            }
            this._player = new Player(this.entityManager, this.component.entityId);
            this._player.setStartTime(this.startTime * 1000000);
            this._player.setEndTime(this.endTime * 1000000);
            this._player.setLoopCount(this.loopCount);
            return this._player;
        },
        enumerable: false,
        configurable: true
    });
    AudioPlayerNode.prototype.play = function () {
        var _this = this;
        if (!this.player) {
            return;
        }
        this.player.play(function (progress) {
            _this.progress = progress;
        }, function () {
            _this.onEnd();
        });
        this.onStart();
    };
    AudioPlayerNode.prototype.continue = function () {
        if (!this.player) {
            return;
        }
        this.player.continue();
    };
    AudioPlayerNode.prototype.pause = function () {
        if (!this.player) {
            return;
        }
        this.player.pause();
    };
    AudioPlayerNode.prototype.stop = function () {
        if (!this.player) {
            return;
        }
        this.player.stop();
    };
    AudioPlayerNode.prototype.onStart = function () { };
    AudioPlayerNode.prototype.onEnd = function () { };
    AudioPlayerNode.nodeType = 'pro/AudioPlayer';
    return AudioPlayerNode;
}(Node));
light.NodeContext.registerNode(AudioPlayerNode);

var Weather;
(function (Weather) {
    Weather["WEATHER_NAME"] = "weather.name";
    Weather["WEATHER_TYPE"] = "weather.wType";
    Weather["WEATHER_TEMPERATURE"] = "weather.temperature";
    Weather["WEATHER_WIND"] = "weather.wind";
    Weather["WEATHER_FORCE"] = "weather.wind_force";
    Weather["WEATHER_HUMIDITY"] = "weather.humidity";
})(Weather || (Weather = {}));
var WeatherNode = /** @class */ (function (_super) {
    __extends(WeatherNode, _super);
    function WeatherNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    WeatherNode.prototype.Run = function () {
        this.weatherName = GetWaterMarkConfig(Weather.WEATHER_NAME);
        this.weatherCode = GetWaterMarkConfig(Weather.WEATHER_TYPE);
        this.windForce = GetWaterMarkConfig(Weather.WEATHER_FORCE);
        this.windDirection = GetWaterMarkConfig(Weather.WEATHER_WIND); // 方向
        this.temperature = GetWaterMarkConfig(Weather.WEATHER_TEMPERATURE);
        this.humidity = GetWaterMarkConfig(Weather.WEATHER_HUMIDITY);
        this.Next();
    };
    WeatherNode.prototype.Next = function () { };
    WeatherNode.nodeType = 'pro/Weather';
    return WeatherNode;
}(Node));
light.NodeContext.registerNode(WeatherNode);

var LOCATION;
(function (LOCATION) {
    LOCATION["COUNTRY"] = "location.country";
    LOCATION["PROVINCE"] = "location.province";
    LOCATION["CITY"] = "location.city";
    LOCATION["TOWN"] = "location.town";
    LOCATION["LATITUDE"] = "location.latitude";
    LOCATION["LONGITUDE"] = "location.longitude";
    LOCATION["LOCATION"] = "location";
})(LOCATION || (LOCATION = {}));
var LocationNode = /** @class */ (function (_super) {
    __extends(LocationNode, _super);
    function LocationNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.country = '';
        _this.province = '';
        _this.city = '';
        _this.town = '';
        _this.location = '';
        return _this;
    }
    LocationNode.prototype.Run = function () {
        this.country = GetWaterMarkConfig(LOCATION.COUNTRY);
        this.province = GetWaterMarkConfig(LOCATION.PROVINCE);
        this.city = GetWaterMarkConfig(LOCATION.CITY);
        this.town = GetWaterMarkConfig(LOCATION.TOWN);
        this.long_latitude = {
            x: Number(GetWaterMarkConfig(LOCATION.LONGITUDE)),
            y: Number(GetWaterMarkConfig(LOCATION.LATITUDE)),
        };
        this.location = this.country + "-" + this.province + "-" + this.city;
        this.Next();
    };
    LocationNode.prototype.Next = function () { };
    LocationNode.nodeType = 'pro/Location';
    return LocationNode;
}(Node));
light.NodeContext.registerNode(LocationNode);

var StringCompareNode = /** @class */ (function (_super) {
    __extends(StringCompareNode, _super);
    function StringCompareNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.isCaseSensitive = false;
        return _this;
    }
    StringCompareNode.prototype.Run = function () {
        if (!this.isCaseSensitive) {
            this.A = this.A.toUpperCase();
            this.B = this.B.toUpperCase();
        }
        if (this.A === this.B) {
            this.Same();
        }
        else {
            this.Different();
        }
    };
    StringCompareNode.prototype.Same = function () { };
    StringCompareNode.prototype.Different = function () { };
    StringCompareNode.nodeType = 'pro/stringCompare';
    return StringCompareNode;
}(Node));
light.NodeContext.registerNode(StringCompareNode);

var StringSpliceNode = /** @class */ (function (_super) {
    __extends(StringSpliceNode, _super);
    function StringSpliceNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.string1 = '';
        _this.string2 = '';
        _this.num = 2;
        _this.Res = '';
        return _this;
    }
    StringSpliceNode.prototype.Process = function () {
        var res = '';
        for (var i = 1; i <= this.num; i++) {
            res += this["string" + i] || '';
        }
        this.Res = res;
    };
    StringSpliceNode.nodeType = 'pro/stringSplice';
    return StringSpliceNode;
}(Node));
light.NodeContext.registerNode(StringSpliceNode);

var AssetUtils = /** @class */ (function () {
    function AssetUtils() {
    }
    AssetUtils.setAsset = function (entityManager, entity, component, assetKey) {
        // 替换资源 key
        component.src = assetKey;
        var timeOffsetComponent = entity.getComponent(light.TimeOffset);
        if (!timeOffsetComponent) {
            return;
        }
        // 更新时长
        var inputSourcesString = light.AIDataUtils.GetDataFromDataCenter(entityManager, 'inputSources');
        var inputSources = JSON.parse(inputSourcesString);
        var source = inputSources[assetKey];
        timeOffsetComponent.startOffset = light.getCurrentTime();
        timeOffsetComponent.duration = source.duration ? source.duration : 2000000;
        // 重置播放进度
        var timeControlComponent = entity.getComponent(light.TimeControl);
        timeControlComponent.setCurrentTime(0);
    };
    return AssetUtils;
}());

var SetComponentNode = /** @class */ (function (_super) {
    __extends(SetComponentNode, _super);
    function SetComponentNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    SetComponentNode.prototype.Run = function () {
        var _a, _b;
        var entity = this.getEntityById(this.entityId);
        if (!entity || !this.componentType || !this.propertyDef) {
            return;
        }
        var component = entity.getComponent(light[this.componentType]);
        if (component) {
            if (this.componentType === 'ScreenTransform') {
                this.setScreenTransformProperty(entity.screenTransform, this.propertyDef.name, this.currentValue);
            }
            else if (this.componentType === 'BasicTransform') {
                this.setBasicTransformProperty(entity.transform, this.propertyDef.name, this.currentValue);
            }
            else if ((this.componentType === 'Image' && this.propertyDef.name === 'src')
                || (this.componentType === 'AudioSource'
                    && this.propertyDef.name === 'ePath')) {
                AssetUtils.setAsset(this.entityManager, entity, component, this.currentValue);
            }
            else if (this.componentType === 'PAGAsset') {
                if (this.extraPropDef) {
                    var obj_1 = component;
                    var maxLength_1 = this.extraPropDef.length;
                    (_b = (_a = this.extraPropDef).forEach) === null || _b === void 0 ? void 0 : _b.call(_a, function (prop, index) {
                        var _a;
                        if (index >= maxLength_1 - 1) {
                            return;
                        }
                        if (!obj_1) {
                            return;
                        }
                        if (typeof prop == 'number') {
                            obj_1 = (_a = obj_1['get']) === null || _a === void 0 ? void 0 : _a.call(obj_1, prop);
                        }
                        else {
                            obj_1 = obj_1[prop];
                        }
                    });
                    var p = this.extraPropDef[maxLength_1 - 1];
                    // @ts-ignore
                    obj_1[p] = this.currentValue;
                }
            }
            else {
                component[this.propertyDef.name] = this.currentValue;
            }
            this.Next();
        }
    };
    SetComponentNode.prototype.Next = function () { };
    SetComponentNode.prototype.setScreenTransformProperty = function (transform, propertyName, value) {
        if (propertyName === 'size') {
            transform.offset.left = ((-1 - transform.pivot.x) / 2) * (value === null || value === void 0 ? void 0 : value.x);
            transform.offset.right = ((1 - transform.pivot.x) / 2) * (value === null || value === void 0 ? void 0 : value.x);
            transform.offset.top = ((1 - transform.pivot.y) / 2) * (value === null || value === void 0 ? void 0 : value.y);
            transform.offset.bottom = ((-1 - transform.pivot.y) / 2) * (value === null || value === void 0 ? void 0 : value.y);
        }
        else if (propertyName === 'position') {
            transform.anchor.left = value === null || value === void 0 ? void 0 : value.x;
            transform.anchor.right = value === null || value === void 0 ? void 0 : value.x;
            transform.anchor.top = value === null || value === void 0 ? void 0 : value.y;
            transform.anchor.bottom = value === null || value === void 0 ? void 0 : value.y;
        }
        else if (propertyName === 'rotate') {
            var quat = eulerToQuaternion(0, 0, value);
            transform.rotation.x = quat.x;
            transform.rotation.y = quat.y;
            transform.rotation.z = quat.z;
            transform.rotation.w = quat.w;
        }
        else if (propertyName === 'scale') {
            transform.scale = new light.Vec3(value === null || value === void 0 ? void 0 : value.x, value === null || value === void 0 ? void 0 : value.y, 1);
        }
        else if (propertyName === 'pivot') {
            transform.pivot.x = value === null || value === void 0 ? void 0 : value.x;
            transform.pivot.y = value === null || value === void 0 ? void 0 : value.y;
        }
        else if (propertyName === 'enabled') {
            transform.enabled = value;
            transform.objectEnabled = value;
        }
        else {
            transform[propertyName] = value;
        }
    };
    SetComponentNode.prototype.setBasicTransformProperty = function (transform, propertyName, value) {
        if (propertyName === 'position') {
            transform.SetPosition(new light.Vec3(value === null || value === void 0 ? void 0 : value.x, value === null || value === void 0 ? void 0 : value.y, value === null || value === void 0 ? void 0 : value.z));
        }
        else if (propertyName === 'eEuler') {
            var quat = eulerToQuaternion(-(value === null || value === void 0 ? void 0 : value.z) - 180, value === null || value === void 0 ? void 0 : value.y, -(value === null || value === void 0 ? void 0 : value.x));
            transform.SetRotation(new light.Quat(quat.x, quat.y, quat.z, quat.w));
        }
        else if (propertyName === 'scale') {
            transform.SetScale(new light.Vec3(value === null || value === void 0 ? void 0 : value.x, value === null || value === void 0 ? void 0 : value.y, value === null || value === void 0 ? void 0 : value.z));
        }
        else {
            transform[propertyName] = value;
        }
    };
    SetComponentNode.nodeType = 'pro/setComponent';
    return SetComponentNode;
}(Node));
light.NodeContext.registerNode(SetComponentNode);

var GetComponentNode = /** @class */ (function (_super) {
    __extends(GetComponentNode, _super);
    function GetComponentNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    GetComponentNode.prototype.Process = function () {
        this.addListener('RenderWillStartEvent', this.onUpdate);
    };
    GetComponentNode.prototype.onUpdate = function () {
        var _a, _b;
        var entity = this.getEntityById(this.entityId);
        if (!entity || !this.componentType || !this.propertyDef) {
            return;
        }
        var component = entity.getComponent(light[this.componentType]);
        if (component) {
            if (this.componentType === 'ScreenTransform') {
                this.outValue = this.getScreenTransformProperty(entity.screenTransform, this.propertyDef.name);
            }
            else if (this.componentType === 'BasicTransform'
                && this.propertyDef.name === 'eEuler') {
                this.outValue = quaternionToEuler(entity.transform.rotation.x, entity.transform.rotation.y, entity.transform.rotation.z, entity.transform.rotation.w);
            }
            else if (this.componentType === 'PAGAsset') {
                if (this.extraPropDef) {
                    var obj_1 = component;
                    (_b = (_a = this.extraPropDef).forEach) === null || _b === void 0 ? void 0 : _b.call(_a, function (prop) {
                        var _a;
                        if (!obj_1) {
                            return;
                        }
                        if (typeof prop == 'number') {
                            obj_1 = (_a = obj_1['get']) === null || _a === void 0 ? void 0 : _a.call(obj_1, prop);
                        }
                        else {
                            obj_1 = obj_1[prop];
                        }
                    });
                    this.outValue = obj_1;
                }
            }
            else if (this.outValue !== component[this.propertyDef.name]) {
                this.outValue = component[this.propertyDef.name];
            }
        }
    };
    GetComponentNode.prototype.getScreenTransformProperty = function (transform, propertyName) {
        if (propertyName === 'size') {
            var size = ScreenTransformUtils.getEntitySize(this.entityManager, this.entityId);
            return { x: size.width, y: size.height };
        }
        if (propertyName === 'position') {
            var position = ScreenTransformUtils.getEntityPosition(this.entityManager, this.entityId);
            return position;
        }
        if (propertyName === 'rotate') {
            return ScreenTransformUtils.getEntityRotation(this.entityManager, this.entityId);
        }
        if (propertyName === 'scale') {
            return { x: transform.scale.x, y: transform.scale.y };
        }
        if (propertyName === 'pivot') {
            return transform.pivot;
        }
        if (propertyName === 'enabled') {
            return transform.enabled && transform.objectEnabled;
        }
        return transform[propertyName];
    };
    GetComponentNode.nodeType = 'pro/getComponent';
    return GetComponentNode;
}(DataProcessNode));
light.NodeContext.registerNode(GetComponentNode);

var AssetNode = /** @class */ (function (_super) {
    __extends(AssetNode, _super);
    function AssetNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AssetNode.prototype.Process = function () {
        var _a;
        this.outputKey = this.key;
        var inputSourcesString = light.AIDataUtils.GetDataFromDataCenter(this.entityManager, 'inputSources');
        try {
            var inputSources = JSON.parse(inputSourcesString);
            var inputSourceName = ((_a = inputSources[this.outputKey]) === null || _a === void 0 ? void 0 : _a.label) || '';
            var extension = inputSourceName.lastIndexOf('.');
            if (extension <= -1) {
                extension = inputSourceName.length;
            }
            this.fileName = inputSourceName.substring(0, extension);
        }
        catch (e) {
            console.error('AssetNode json parse error', e);
            this.fileName = this.outputKey;
        }
    };
    AssetNode.nodeType = 'pro/asset';
    return AssetNode;
}(DataProcessNode));
light.NodeContext.registerNode(AssetNode);

var EntityNode = /** @class */ (function (_super) {
    __extends(EntityNode, _super);
    function EntityNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EntityNode.prototype.Process = function () {
        this.entity = this.entityId;
    };
    EntityNode.nodeType = 'pro/entity';
    return EntityNode;
}(DataProcessNode));
light.NodeContext.registerNode(EntityNode);

var ComponentNode = /** @class */ (function (_super) {
    __extends(ComponentNode, _super);
    function ComponentNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ComponentNode.prototype.Process = function () {
        this.component = {
            entityId: this.entityId,
            type: this.componentType,
            enabled: this.enabled,
            componentID: this.componentID,
        };
    };
    ComponentNode.nodeType = 'pro/component';
    return ComponentNode;
}(DataProcessNode));
light.NodeContext.registerNode(ComponentNode);

var EntityRandomHideNode = /** @class */ (function (_super) {
    __extends(EntityRandomHideNode, _super);
    function EntityRandomHideNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.num = 2;
        _this.toShow = [];
        _this.toHide = [];
        _this.indexToHilde = 0;
        return _this;
    }
    EntityRandomHideNode.prototype.Run = function () {
        this.toShow = [];
        this.toHide = [];
        this.indexToHilde = Math.floor(Math.random() * this.num);
        for (var i = 0; i < this.num; i++) {
            var entity = this.getEntityById(this["entity" + (i + 1)]);
            if (entity) {
                if (i === this.indexToHilde % this.num) {
                    this.toHide.push(this["entity" + (i + 1)]);
                }
                else {
                    this.toShow.push(this["entity" + (i + 1)]);
                }
            }
        }
        showEntities(this.toShow, this.entityManager);
        hideEntities(this.toHide, this.entityManager);
        this.Next();
    };
    EntityRandomHideNode.prototype.Next = function () { };
    EntityRandomHideNode.nodeType = 'pro/entityRandomHide';
    return EntityRandomHideNode;
}(Node));
light.NodeContext.registerNode(EntityRandomHideNode);

var EntityLoopHideNode = /** @class */ (function (_super) {
    __extends(EntityLoopHideNode, _super);
    function EntityLoopHideNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.random = false;
        _this.times = 1;
        _this.duration = 1;
        _this.num = 2;
        _this.toShow = [];
        _this.toHide = [];
        _this.lastExecuteTime = 0;
        _this.totalExecuteTimes = 0;
        _this.isStop = false;
        _this.isFirstTriggered = false;
        _this.indexToHide = 0;
        return _this;
    }
    EntityLoopHideNode.prototype.Run = function () {
        this.Reset();
        this.addListener('update', this.Update);
        if (this.random) {
            this.indexToHide = Math.floor(Math.random() * this.num);
        }
        this.lastExecuteTime = light.getCurrentTime();
        this.Update(this.lastExecuteTime);
        this.Next();
    };
    EntityLoopHideNode.prototype.Reset = function () {
        this.isFirstTriggered = true;
        this.isStop = false;
        this.totalExecuteTimes = 0;
        this.removeListener('update');
    };
    EntityLoopHideNode.prototype.Stop = function () {
        this.isStop = true;
    };
    EntityLoopHideNode.prototype.Next = function () { };
    EntityLoopHideNode.prototype.Loop = function () { };
    EntityLoopHideNode.prototype.Update = function (time) {
        // 延时未到，暂不执行
        if (!this.isFirstTriggered && time / 1000 - this.lastExecuteTime / 1000 < this.duration * 1000) {
            return;
        }
        this.isFirstTriggered = false;
        // 达到循环次数或手动停止，不继续执行
        if (this.totalExecuteTimes >= this.times * this.num || this.isStop) {
            this.removeListener('update');
            return;
        }
        this.toShow = [];
        this.toHide = [];
        for (var i = 0; i < this.num; i++) {
            var entity = this.getEntityById(this["entity" + (i + 1)]);
            if (entity) {
                if (i === this.indexToHide % this.num) {
                    this.toHide.push(this["entity" + (i + 1)]);
                }
                else {
                    this.toShow.push(this["entity" + (i + 1)]);
                }
            }
        }
        this.indexToHide = this.indexToHide + 1;
        this.totalExecuteTimes += 1;
        this.lastExecuteTime = light.getCurrentTime();
        showEntities(this.toShow, this.entityManager);
        hideEntities(this.toHide, this.entityManager);
        this.Loop();
    };
    EntityLoopHideNode.nodeType = 'pro/entityLoopHide';
    return EntityLoopHideNode;
}(Node));
light.NodeContext.registerNode(EntityLoopHideNode);

var DoTimeNode = /** @class */ (function (_super) {
    __extends(DoTimeNode, _super);
    function DoTimeNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.time = 1;
        _this.triggerTime = -1; // 上一次触发时间
        return _this;
    }
    DoTimeNode.prototype.Run = function () {
        var currentTime = light.getCurrentTime();
        if (this.triggerTime < 0 // 首次触发及间隔超过
            || (this.triggerTime >= 0 && (this.triggerTime + this.time * 1000000 < currentTime))) {
            this.triggerTime = currentTime;
            this.Next();
        }
    };
    DoTimeNode.prototype.Next = function () { };
    DoTimeNode.nodeType = 'pro/doTime';
    return DoTimeNode;
}(Node));
light.NodeContext.registerNode(DoTimeNode);

var ParallelNode = /** @class */ (function (_super) {
    __extends(ParallelNode, _super);
    function ParallelNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ParallelNode.prototype.EventA = function () {
        this.Next();
    };
    ParallelNode.prototype.EventB = function () {
        this.Next();
    };
    ParallelNode.prototype.EventC = function () {
        this.Next();
    };
    ParallelNode.prototype.EventD = function () {
        this.Next();
    };
    ParallelNode.prototype.EventE = function () {
        this.Next();
    };
    ParallelNode.prototype.Next = function () { };
    ParallelNode.nodeType = 'pro/Parallel';
    return ParallelNode;
}(Node));
light.NodeContext.registerNode(ParallelNode);

var EmitEvent = /** @class */ (function (_super) {
    __extends(EmitEvent, _super);
    function EmitEvent() {
        var _this = _super.call(this) || this;
        submitEvent('EmitEvent', function (eventName) {
            if (_this._methodType === 1 && eventName === _this._eventName) {
                _this.Next();
            }
        });
        return _this;
    }
    EmitEvent.prototype.Run = function () {
        emitEvent('EmitEvent', this._eventName);
    };
    EmitEvent.prototype.Next = function () { };
    EmitEvent.nodeType = 'pro/emitEvent';
    return EmitEvent;
}(Node));
light.NodeContext.registerNode(EmitEvent);

var SetMemberVar = /** @class */ (function (_super) {
    __extends(SetMemberVar, _super);
    function SetMemberVar() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    SetMemberVar.prototype.Run = function () {
        this.outValue = this.inValue;
        emitEvent('globalValue', this.propertyName, this.inValue);
        this.Next();
    };
    SetMemberVar.prototype.Next = function () { };
    SetMemberVar.nodeType = 'pro/setMemberVar';
    return SetMemberVar;
}(Node));
light.NodeContext.registerNode(SetMemberVar);

var GetMemberVar = /** @class */ (function (_super) {
    __extends(GetMemberVar, _super);
    function GetMemberVar() {
        var _this = _super.call(this) || this;
        submitEvent('globalValue', function (propertyName, value) {
            if (_this.propertyName === propertyName) {
                _this.outValue = value;
            }
        });
        return _this;
    }
    GetMemberVar.prototype.Next = function () { };
    GetMemberVar.prototype.Process = function () {
        this.outValue = this.defaultValue;
    };
    GetMemberVar.nodeType = 'pro/getMemberVar';
    return GetMemberVar;
}(DataProcessNode));
light.NodeContext.registerNode(GetMemberVar);

var PointInBoundingBoxNode = /** @class */ (function (_super) {
    __extends(PointInBoundingBoxNode, _super);
    function PointInBoundingBoxNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.point = { x: 0, y: 0 };
        _this.area = { x: 0, y: 0, w: 0, h: 0 };
        return _this;
    }
    PointInBoundingBoxNode.prototype.Run = function () {
        if (!this.point || !this.area) {
            return;
        }
        if (this.isPointInMatrix(this.area, this.point)) {
            this.InBoundingBox();
        }
        else {
            this.NotInBoundingBox();
        }
    };
    PointInBoundingBoxNode.prototype.InBoundingBox = function () { };
    PointInBoundingBoxNode.prototype.NotInBoundingBox = function () { };
    PointInBoundingBoxNode.prototype.getCross = function (p1, p2, p) {
        return (p2.x - p1.x) * (p.y - p1.y) - (p.x - p1.x) * (p2.y - p1.y);
    };
    PointInBoundingBoxNode.prototype.isPointInMatrix = function (area, p) {
        var p1 = { x: area.x, y: area.y };
        var p2 = { x: area.x + area.w, y: area.y };
        var p3 = { x: area.x + area.w, y: area.y + area.h };
        var p4 = { x: area.x, y: area.y + area.h };
        return this.getCross(p1, p2, p) * this.getCross(p3, p4, p) >= 0
            && this.getCross(p2, p3, p) * this.getCross(p4, p1, p) >= 0;
    };
    PointInBoundingBoxNode.nodeType = 'pro/pointInBoundingBox';
    return PointInBoundingBoxNode;
}(Node));
light.NodeContext.registerNode(PointInBoundingBoxNode);

function PixelToNormalizedCoords(node, pixel) {
    var width = light.DeviceUtils.GetSurfaceWidth(node.entityManager);
    var height = light.DeviceUtils.GetSurfaceHeight(node.entityManager);
    return {
        x: (2 * pixel.x) / ((1280 * width) / height) - 1,
        y: 1 - (2 * pixel.y) / 1280,
    };
}
function NormalizedToPixelCoords(node, normalized) {
    var width = light.DeviceUtils.GetSurfaceWidth(node.entityManager);
    var height = light.DeviceUtils.GetSurfaceHeight(node.entityManager);
    return {
        x: (normalized.x + 1) * (1280 * width) / height / 2,
        y: (1 - normalized.y) * 640,
    };
}

var NormalizedToPixelNode = /** @class */ (function (_super) {
    __extends(NormalizedToPixelNode, _super);
    function NormalizedToPixelNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    NormalizedToPixelNode.prototype.Process = function () {
        this.pixel = NormalizedToPixelCoords(this, this.normalizedPoint);
    };
    NormalizedToPixelNode.nodeType = 'pro/normalizedToPixel';
    return NormalizedToPixelNode;
}(Node));
light.NodeContext.registerNode(NormalizedToPixelNode);

var PixelToNormalizedNode = /** @class */ (function (_super) {
    __extends(PixelToNormalizedNode, _super);
    function PixelToNormalizedNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    PixelToNormalizedNode.prototype.Process = function () {
        this.normalizedPoint = PixelToNormalizedCoords(this, this.pixel);
    };
    PixelToNormalizedNode.nodeType = 'pro/pixelToNormalized';
    return PixelToNormalizedNode;
}(Node));
light.NodeContext.registerNode(PixelToNormalizedNode);

var EmotionScoreEventNode = /** @class */ (function (_super) {
    __extends(EmotionScoreEventNode, _super);
    function EmotionScoreEventNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EmotionScoreEventNode.prototype.setup = function () {
        openAIFeature(["Emotion_Score" /* EMOTION_SCORE */], this.entityManager, this.eventManager);
    };
    EmotionScoreEventNode.prototype.getData = function () {
        var _a;
        var data = getAIDataFromAIDataCenter(["Emotion_Score" /* EMOTION_SCORE */], this.entityManager);
        if (data.length === 0 || this.faceIndex > ((_a = data[0]) === null || _a === void 0 ? void 0 : _a.length)) {
            return;
        }
        // 按 trace ID 从小到大排序
        var emotionData = data[0];
        emotionData.sort(function (a, b) { return a.traceID - b.traceID; });
        this.emotionDegreeValue = emotionData[this.faceIndex - 1].detectParams[this.emotionType][0];
        this.next();
    };
    EmotionScoreEventNode.prototype.next = function () { };
    EmotionScoreEventNode.nodeType = 'pro/EmotionScoreEvent';
    return EmotionScoreEventNode;
}(Node));
light.NodeContext.registerNode(EmotionScoreEventNode);

var AssetFragmentStartNode = /** @class */ (function (_super) {
    __extends(AssetFragmentStartNode, _super);
    function AssetFragmentStartNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AssetFragmentStartNode.prototype.Run = function () {
        var _this = this;
        light.on('LightAssetFragmentStartEvent', function (event) {
            if (event.type() !== 'LightAssetFragmentStartEvent') {
                return;
            }
            var entity = event['entity'];
            if (entity
                && entity.getComponent(light.EntityIdentifier).id
                    === _this.fragmentRootEntityId) {
                _this.next();
            }
        });
    };
    AssetFragmentStartNode.prototype.next = function () { };
    AssetFragmentStartNode.nodeType = 'pro/AssetFragmentStart';
    return AssetFragmentStartNode;
}(EventNode));
light.NodeContext.registerNode(AssetFragmentStartNode);

var FragmentFinishTriggerNode = /** @class */ (function (_super) {
    __extends(FragmentFinishTriggerNode, _super);
    function FragmentFinishTriggerNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    FragmentFinishTriggerNode.prototype.Run = function () {
        this.Next();
    };
    FragmentFinishTriggerNode.prototype.Next = function () { };
    FragmentFinishTriggerNode.nodeType = 'pro/FragmentFinishTrigger';
    return FragmentFinishTriggerNode;
}(EventNode));
light.NodeContext.registerNode(FragmentFinishTriggerNode);

var CustomEventDetectNode = /** @class */ (function (_super) {
    __extends(CustomEventDetectNode, _super);
    function CustomEventDetectNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    CustomEventDetectNode.prototype.run = function () {
        var result = new light.MapStringString();
        result.set(this.eventKey, this.eventValue);
        this.eventManager.emit(new light.LightAssetStatusEvent(this.entityManager, this.eventName, result));
        this.next();
    };
    CustomEventDetectNode.prototype.next = function () { };
    CustomEventDetectNode.nodeType = 'pro/customEvent';
    return CustomEventDetectNode;
}(Node));
light.NodeContext.registerNode(CustomEventDetectNode);

if (typeof (globalThis) === 'undefined') {
    this['globalThis'] = this;
}
globalThis.configure = light.configure;
globalThis.update = light.update;

})(light);