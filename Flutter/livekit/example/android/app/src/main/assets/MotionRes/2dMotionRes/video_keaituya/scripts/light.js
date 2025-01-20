
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
        entities.forEach(function (entity, _i) {
            var idComponent = entity.getComponent(light.EntityIdentifier);
            var lightBehaviorComponent = entity.getComponent(light.ScriptBehaviors);
            console.log("JS::lightBehaviorComponent: " + JSON.stringify(lightBehaviorComponent));
            if (!lightBehaviorComponent) {
                return;
            }
            lightBehaviorComponent.behaviorProperties.forEach(function (behaviorPropertiesJson, _i) {
                console.log("JS::behaviorPropertiesJson: " + behaviorPropertiesJson);
                var behaviorProperties = JSON.parse(behaviorPropertiesJson);
                var BehaviorClass = SDKRuntime.BehaviorClasses[behaviorProperties.type];
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
            var it = new Clazz();
            it.entityManager = this.entityManager;
            it.eventManager = this.eventManager;
            it.scriptSystem = this.scriptSystem;
            return it;
        }
        console.log("Cannot find node: " + type);
    };
    NodeContext.prototype.connectData = function (source, property, target, targetProperty) {
        // 重写应该基于 instance
        var descriptor = {
            configurable: true,
            enumerable: true,
            get: function () {
                return source[property];
            },
        };
        Object.defineProperty(target, targetProperty, descriptor);
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
        var task = target[targetProperty];
        tasks.push(task.bind(target));
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

/*! *****************************************************************************
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
        function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
    return extendStatics(d, b);
};

function __extends(d, b) {
    extendStatics(d, b);
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
}

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
    AnimationController.prototype.update = function (_time) {
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
    entitiesWithAnimation.forEach(function (entity, _i) {
        var idComponent = entity.getComponent(light.EntityIdentifier);
        if (!idComponent)
            return;
        var AnimationBehavior = new AnimationController(idComponent.id, entityManager, eventManager, scriptSystem);
        light.runtime.addBehavior(AnimationBehavior);
    });
});

var Node = /** @class */ (function () {
    function Node() {
    }
    return Node;
}());
var EventNode = /** @class */ (function (_super) {
    __extends(EventNode, _super);
    function EventNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return EventNode;
}(Node));

var StartNode = /** @class */ (function (_super) {
    __extends(StartNode, _super);
    function StartNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    StartNode.prototype.Run = function () { };
    StartNode.nodeType = 'code/Start';
    return StartNode;
}(EventNode));
light.NodeContext.registerNode(StartNode);

var LoopType;
(function (LoopType) {
    LoopType["FIXED"] = "\u56FA\u5B9A\u6570\u5B57";
    LoopType["INFINITE"] = "\u65E0\u9650\u5FAA\u73AF";
})(LoopType || (LoopType = {}));
var TimerNode = /** @class */ (function (_super) {
    __extends(TimerNode, _super);
    function TimerNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.duration = 1000000;
        _this.repeatTimes = 1;
        _this.times = 1;
        _this._times = 1;
        _this.startupTrigger = false; // 刚开始就触发一次
        _this.lastTime = 0;
        _this.currentTimes = 0;
        return _this;
    }
    TimerNode.prototype.Update = function () { };
    TimerNode.prototype.Run = function () {
        if (this.startupTrigger && this.repeatTimes !== 0) {
            this.currentTimes = this.currentTimes + 1;
            this.Update();
        }
        light.on('update', this.onLightUpdate.bind(this));
    };
    TimerNode.prototype.onLightUpdate = function (time) {
        if ((time - this.lastTime) > this.duration
            && (this.repeatTimes > this.currentTimes || this.loopType === LoopType.INFINITE)) {
            this.Update();
            this.lastTime = time;
            this.currentTimes = this.currentTimes + 1;
            this.times = this.times + 1;
        }
    };
    TimerNode.nodeType = 'code/Timer';
    return TimerNode;
}(EventNode));
light.NodeContext.registerNode(TimerNode);

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
    light.Particle, ,
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
                    }
                });
            }
        });
    });
};
var hideEntities = function (ids, entityManager) {
    if (ids === void 0) { ids = []; }
    ids.forEach(function (entityId) {
        getRelateEntities(entityId, entityManager).forEach(function (entity, _i) {
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
                    }
                });
            }
        });
    });
};
// 判断entity的component是否enable 如果为跟脸贴纸 还需要判断transform是否visible
var isEntityHide = function (id, entityManager) {
    var cmpEnable = false;
    var entity = entityManager.getEntityById(id);
    if (entity) {
        Queue.forEach(function (queueItem) {
            var cmp = entity.getComponent(queueItem);
            if (cmp) {
                if (!cmp.enabled) {
                    return true;
                }
                cmpEnable = cmp.enabled;
            }
        });
        [light.BasicTransform, light.ScreenTransform].forEach(function (item) {
            var cmp = entity.getComponent(item);
            if (cmp) {
                cmpEnable = cmp.objectEnabled && cmp.visible && cmpEnable;
            }
        });
    }
    return !cmpEnable;
};

var ObjectQueueNode = /** @class */ (function (_super) {
    __extends(ObjectQueueNode, _super);
    function ObjectQueueNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.toShow = [];
        _this.toHide = [];
        _this.init = false;
        _this.random = false;
        _this.indexToDisplay = 0;
        return _this;
    }
    ObjectQueueNode.prototype.Run = function () {
        var _this = this;
        var _a;
        this.toShow = [];
        this.toHide = [];
        if (this.random) {
            this.indexToDisplay = Math.floor(Math.random() * this.entityToDisplay.length);
        }
        (_a = this.entityToDisplay) === null || _a === void 0 ? void 0 : _a.forEach(function (entityId, i) {
            var entity = _this.entityManager.getEntityById(entityId);
            if (entity) {
                if (i === _this.indexToDisplay % _this.entityToDisplay.length) {
                    _this.toShow.push(entityId);
                }
                else {
                    _this.toHide.push(entityId);
                }
            }
        });
        this.indexToDisplay = this.indexToDisplay + 1;
        showEntities(this.toShow, this.entityManager);
        hideEntities(this.toHide, this.entityManager);
        this.Next();
    };
    ObjectQueueNode.prototype.Next = function () { };
    ObjectQueueNode.nodeType = 'code/ObjectQueue';
    return ObjectQueueNode;
}(Node));
light.NodeContext.registerNode(ObjectQueueNode);

var SwitchObjectNode = /** @class */ (function (_super) {
    __extends(SwitchObjectNode, _super);
    function SwitchObjectNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.entityToDisplay = [];
        _this.entityToHide = [];
        _this.init = false;
        return _this;
    }
    SwitchObjectNode.prototype.Run = function () {
        showEntities(this.entityToDisplay, this.entityManager);
        hideEntities(this.entityToHide, this.entityManager);
        this.Next();
    };
    SwitchObjectNode.prototype.Next = function () { };
    SwitchObjectNode.nodeType = 'code/SwitchObject';
    return SwitchObjectNode;
}(Node));
light.NodeContext.registerNode(SwitchObjectNode);

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

var GestureNode = /** @class */ (function (_super) {
    __extends(GestureNode, _super);
    function GestureNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.times = 1;
        _this.currentGesture = GestureAction$1.HEART;
        return _this;
    }
    GestureNode.prototype.Run = function () {
        this.actionEventName = actionNameToEvent(light.GestureAction[this.currentGesture]);
        openAIFeature(["Hand_Gesture" /* HAND_GESTURE */], this.entityManager, this.eventManager);
        light.on('update', this.onUpdate.bind(this));
    };
    GestureNode.prototype.onUpdate = function () {
        var aiData = getAIClassData(["Hand_Gesture" /* HAND_GESTURE */], this.entityManager);
        var expressions = aiData === null || aiData === void 0 ? void 0 : aiData[this.actionEventName];
        var hasGesture = expressions && expressions.length > 0;
        if (this.lastState !== hasGesture) {
            this.lastState = hasGesture;
            if (hasGesture) {
                this.Detected();
                this.times = this.times + 1;
            }
            else {
                this.Lost();
            }
        }
    };
    GestureNode.prototype.Detected = function () { };
    GestureNode.prototype.Lost = function () { };
    GestureNode.nodeType = 'code/Gesture';
    return GestureNode;
}(EventNode));
light.NodeContext.registerNode(GestureNode);

var AIFeature;
(function (AIFeature) {
    AIFeature["onSmile"] = "Smile";
    AIFeature["onKiss"] = "Pout";
})(AIFeature || (AIFeature = {}));
var DefaultAIFeature = "Expression" /* EXPRESSION */;
var FaceEventNode = /** @class */ (function (_super) {
    __extends(FaceEventNode, _super);
    function FaceEventNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.faceAction = light.FaceAction.OpenMouth;
        _this.times = 1;
        return _this;
    }
    FaceEventNode.prototype.Run = function () {
        this.actionEventName = actionNameToEvent(light.FaceAction[this.faceAction]);
        var aifeature = AIFeature[this.actionEventName] ? AIFeature[this.actionEventName] : DefaultAIFeature;
        openAIFeature([aifeature], this.entityManager, this.eventManager);
        light.on('update', this.onUpdate.bind(this));
    };
    FaceEventNode.prototype.onUpdate = function () {
        var aifeature = AIFeature[this.actionEventName] ? AIFeature[this.actionEventName] : DefaultAIFeature;
        var data = getAIClassData([aifeature], this.entityManager);
        var expressions = data === null || data === void 0 ? void 0 : data[this.actionEventName];
        var detected = expressions && expressions.length > 0;
        if (detected) {
            var trackId = expressions[0];
            var faceInfo = {
                trackID: trackId,
                expression: [this.faceAction],
            };
            this.faceOutput = faceInfo;
            if (trackId !== this.lastTrackId || detected !== this.lastState) {
                console.log("FaceEventNode: onDetected: " + trackId);
                this.onDetected();
                this.times = this.times + 1;
            }
            this.lastTrackId = trackId;
        }
        else if (this.lastState) {
            this.lastTrackId = -1;
            console.log('FaceEventNode: Lost');
            this.Lost();
        }
        this.lastState = detected;
    };
    FaceEventNode.prototype.onDetected = function () { };
    FaceEventNode.prototype.Lost = function () { };
    FaceEventNode.nodeType = 'code/FaceEvent';
    return FaceEventNode;
}(EventNode));
light.NodeContext.registerNode(FaceEventNode);

var MusicNode = /** @class */ (function (_super) {
    __extends(MusicNode, _super);
    function MusicNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.highlightOffset = 0;
        _this.slowRhythmOffset = 0;
        _this.drumOffset = 0;
        _this.startValueOffset = 0;
        _this.endValueOffset = 0;
        _this.curMusicTime = -1;
        return _this;
    }
    MusicNode.prototype.Run = function () {
        light.on('RhythmEvent', this.onMusicData.bind(this));
    };
    MusicNode.prototype.onMusicData = function (params) {
        if (params.rhythmInfos.HighlightValueTrail) {
            for (var i = 0, size = params.rhythmInfos.HighlightValueTrail.length; i < size; i++) {
                if (Math.abs(params.rhythmInfos.HighlightValueTrail[i].time + this.highlightOffset - params.elapseTime)
                    <= params.elapseTime - this.curMusicTime) {
                    this.onHighlightValueTrail();
                }
            }
        }
        if (params.rhythmInfos.SlowRhythmTrail) {
            for (var i = 0, size = params.rhythmInfos.SlowRhythmTrail.length; i < size; i++) {
                if (Math.abs(params.rhythmInfos.SlowRhythmTrail[i].time + this.slowRhythmOffset - params.elapseTime)
                    <= params.elapseTime - this.curMusicTime) {
                    this.onSlowRhythmTrail();
                }
            }
        }
        if (params.rhythmInfos.DrumTrail) {
            for (var i = 0, size = params.rhythmInfos.DrumTrail.length; i < size; i++) {
                if (Math.abs(params.rhythmInfos.DrumTrail[i].time + this.drumOffset - params.elapseTime)
                    <= params.elapseTime - this.curMusicTime) {
                    this.onDrumTrail();
                }
            }
        }
        if (params.rhythmInfos.StartValueTrail) {
            for (var i = 0, size = params.rhythmInfos.StartValueTrail.length; i < size; i++) {
                if (params.rhythmInfos.StartValueTrail[i].index === 0) {
                    if (Math.abs(params.rhythmInfos.StartValueTrail[i].time + this.startValueOffset - params.elapseTime)
                        <= params.elapseTime - this.curMusicTime) {
                        this.onStartValueTrail();
                    }
                }
                else {
                    if (Math.abs(params.rhythmInfos.StartValueTrail[i].time + this.endValueOffset - params.elapseTime)
                        <= params.elapseTime - this.curMusicTime) {
                        this.onEndValueTrail();
                    }
                }
            }
        }
        this.curMusicTime = params.elapseTime;
    };
    MusicNode.prototype.onStartValueTrail = function () { };
    MusicNode.prototype.onEndValueTrail = function () { };
    MusicNode.prototype.onHighlightValueTrail = function () { };
    MusicNode.prototype.onSlowRhythmTrail = function () { };
    MusicNode.prototype.onDrumTrail = function () { };
    MusicNode.nodeType = 'code/music';
    return MusicNode;
}(EventNode));
light.NodeContext.registerNode(MusicNode);

var AnimationNode = /** @class */ (function (_super) {
    __extends(AnimationNode, _super);
    function AnimationNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.loopCount = 0;
        _this.setProgress = false;
        _this.progress = 0;
        _this.onUpdate = function (time) {
            console.error('AnimationNode update');
            if (_this.loopCount < 0) {
                return;
            }
            var entity = _this.entityManager.getEntityById(_this.entityId);
            var controller = entity.getComponent(light.AnimationController);
            var timeCtrls = entity.getComponent(light.TimeControlList);
            if (timeCtrls) {
                controller.clips.forEach(function (clip, index) {
                    if (_this.clipIndex === index && clip.state === 1 /* Playing */) {
                        if (timeCtrls.timeControlArray.get(index).currentTime >= (_this.loopCount + 1) * clip.duration) {
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
                        if ((light.getCurrentTime() - clip.startTime) >= (_this.loopCount + 1) * clip.duration) {
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
    AnimationNode.prototype.Play = function () {
        var _this = this;
        var entity = this.entityManager.getEntityById(this.entityId);
        var controller = entity.getComponent(light.AnimationController);
        var timeCtrls = entity.getComponent(light.TimeControlList);
        if (timeCtrls) {
            controller.clips.forEach(function (clip, index) {
                if (_this.clipIndex === index) {
                    clip.state = 1 /* Playing */;
                    timeCtrls.timeControlArray.get(index).pause = false;
                    timeCtrls.timeControlArray.get(index).loopCount = _this.loopCount;
                    if (_this.setProgress) {
                        timeCtrls.timeControlArray.get(index).currentTime = _this.progress;
                    }
                    // clip.progress != timeCtrl.currentTime 触发刷新
                    clip.progress = 1;
                }
                else {
                    clip.state = 0 /* Stopped */;
                    timeCtrls.timeControlArray.get(index).pause = true;
                }
            });
        }
        else {
            controller.clips.forEach(function (clip, index) {
                if (_this.clipIndex === index) {
                    clip.state = 1 /* Playing */;
                    if (_this.setProgress) {
                        clip.progress = _this.progress;
                    }
                    clip.startTime = light.getCurrentTime() - clip.progress;
                }
                else {
                    clip.state = 0 /* Stopped */;
                }
            });
        }
        this.detachEvent();
        light.on('update', this.onUpdate);
        this.Next();
    };
    AnimationNode.prototype.Pause = function () {
        var _this = this;
        var entity = this.entityManager.getEntityById(this.entityId);
        var controller = entity.getComponent(light.AnimationController);
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
    AnimationNode.prototype.Stop = function () {
        var _this = this;
        var entity = this.entityManager.getEntityById(this.entityId);
        var controller = entity.getComponent(light.AnimationController);
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
    AnimationNode.prototype.Next = function () { };
    AnimationNode.prototype.Finish = function () { };
    AnimationNode.prototype.detachEvent = function () {
        light.removeListener('update', this.onUpdate);
    };
    AnimationNode.nodeType = 'code/AnimationNode';
    return AnimationNode;
}(Node));
light.NodeContext.registerNode(AnimationNode);

var LoopType$1;
(function (LoopType) {
    LoopType["FIXED"] = "\u56FA\u5B9A\u6570\u5B57";
    LoopType["INFINITE"] = "\u65E0\u9650\u5FAA\u73AF";
})(LoopType$1 || (LoopType$1 = {}));
var QueueNode = /** @class */ (function (_super) {
    __extends(QueueNode, _super);
    function QueueNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.indexToDisplay = 0;
        _this.indexToHide = 0;
        _this.repeatTimes = 1;
        _this.init = false;
        _this.toShow = [];
        _this.toHide = [];
        return _this;
    }
    QueueNode.prototype.Run = function () {
        this.toShow = [];
        this.toHide = [];
        var _a = this, entityToDisplay = _a.entityToDisplay, entityToHide = _a.entityToHide, repeatTimes = _a.repeatTimes;
        if (entityToDisplay.length) {
            if (this.indexToDisplay < entityToDisplay.length * repeatTimes || this.loopType === LoopType$1.INFINITE) {
                var entityId = entityToDisplay[this.indexToDisplay % entityToDisplay.length];
                var entity = this.entityManager.getEntityById(entityId);
                if (entity) {
                    this.toShow.push(entityId);
                    this.indexToDisplay += 1;
                }
            }
        }
        if (entityToHide.length) {
            if (this.indexToHide < entityToHide.length * repeatTimes || this.loopType === LoopType$1.INFINITE) {
                var entityId = entityToHide[this.indexToHide % entityToHide.length];
                var entity = this.entityManager.getEntityById(entityId);
                if (entity) {
                    this.toHide.push(entityId);
                    this.indexToHide += 1;
                }
            }
        }
        showEntities(this.toShow, this.entityManager);
        hideEntities(this.toHide, this.entityManager);
        this.Next();
    };
    QueueNode.prototype.Next = function () { };
    QueueNode.nodeType = 'code/Queue';
    return QueueNode;
}(Node));
light.NodeContext.registerNode(QueueNode);

var AccumulationNumberNode = /** @class */ (function (_super) {
    __extends(AccumulationNumberNode, _super);
    function AccumulationNumberNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.Value = 0;
        return _this;
    }
    AccumulationNumberNode.prototype.Run = function () {
        if (this.current === undefined) {
            this.current = this.from;
            this.Value = this.current;
            this.Next();
        }
        else {
            if (this.current + this.step > this.to) {
                return;
            }
            this.current += this.step;
            this.Value = this.current;
            this.Next();
        }
    };
    AccumulationNumberNode.prototype.Next = function () { };
    AccumulationNumberNode.nodeType = 'code/accumulationNumber';
    return AccumulationNumberNode;
}(Node));
light.NodeContext.registerNode(AccumulationNumberNode);

var MAX_AGE = 66;
// 年龄判断
var AgeNode = /** @class */ (function (_super) {
    __extends(AgeNode, _super);
    function AgeNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.ageRange = [];
        _this.MAX_AGE = 66;
        _this.lastAgeInfo = {};
        return _this;
    }
    AgeNode.prototype.Run = function () {
        var faceInput = this.faceInput;
        if (!(faceInput === null || faceInput === void 0 ? void 0 : faceInput.age)) {
            return;
        }
        // 上次的人脸而且年龄相同，不重复触发
        if (this.lastAgeInfo[faceInput.trackID] === faceInput.age) {
            return;
        }
        this.ageOutput = faceInput;
        if (this.ageRange.length === 0) {
            this.Next();
        }
        else {
            var funName = '';
            var age = faceInput.age;
            this.ageRange.sort(function (a, b) { return a - b; });
            var start = 0;
            for (var i = 0, size = this.ageRange.length; i < size; i++) {
                if (this.ageRange[i] === start) {
                    continue;
                }
                if (age < this.ageRange[i]) {
                    funName = start + "_" + this.ageRange[i];
                    if (!this[funName]) {
                        this[funName] = function () { };
                    }
                    this[funName]();
                    return;
                }
                start = this.ageRange[i];
            }
            if (start < MAX_AGE && start <= age && age < MAX_AGE) {
                funName = start + "_" + MAX_AGE;
                if (!this[funName]) {
                    this[funName] = function () { };
                }
                this[funName]();
            }
        }
    };
    AgeNode.prototype.Next = function () { };
    AgeNode.nodeType = 'code/Age';
    return AgeNode;
}(Node));
light.NodeContext.registerNode(AgeNode);

var AgeEventNode = /** @class */ (function (_super) {
    __extends(AgeEventNode, _super);
    function AgeEventNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.lastAgeInfo = {};
        _this.customEvents = [];
        _this.MAX_AGE = 66;
        _this.AI_FETURE = [
            "Expression" /* EXPRESSION */,
            "Age" /* AGE */,
            "Gender" /* GENDER */,
            "Smile" /* SMILE */,
        ];
        return _this;
    }
    AgeEventNode.prototype.Run = function () {
        openAIFeature(this.AI_FETURE, this.entityManager, this.eventManager);
        light.on('update', this.onUpdate.bind(this));
    };
    AgeEventNode.prototype.onUpdate = function () {
        var _this = this;
        var data = getAIClassData(this.AI_FETURE, this.entityManager);
        var expressions = data === null || data === void 0 ? void 0 : data.onAge;
        var detected = expressions && expressions.length > 0;
        if (detected) {
            expressions.forEach(function (ageRange, _index) {
                var trackID = ageRange[0];
                var age = ageRange[1];
                if (!_this.lastAgeInfo[trackID] || _this.lastAgeInfo[trackID] !== age) {
                    _this.ageOutput = {
                        trackID: ageRange[0],
                        age: age,
                    };
                    _this.ageNumberOutput = age;
                    _this.trackIndexOutput = ageRange[0];
                    var start_1 = 0;
                    var invoke_1 = false;
                    if (_this.ageRange.length > 0) {
                        _this.ageRange.forEach(function (_age, _i) {
                            if (invoke_1) {
                                return;
                            }
                            if (age < _age) {
                                if (!_this[start_1 + "_" + _age]) {
                                    _this[start_1 + "_" + _age] = function () { };
                                }
                                _this[start_1 + "_" + _age]();
                                invoke_1 = true;
                            }
                            start_1 = _age;
                        });
                        if (!invoke_1 && age <= _this.MAX_AGE) {
                            if (!_this[start_1 + "_" + _this.MAX_AGE]) {
                                _this[start_1 + "_" + _this.MAX_AGE] = function () { };
                            }
                            _this[start_1 + "_" + _this.MAX_AGE]();
                        }
                    }
                    else {
                        if (!_this['Next']) {
                            _this['Next'] = function () { };
                        }
                        _this['Next']();
                    }
                }
            });
        }
    };
    AgeEventNode.nodeType = 'code/AgeEvent';
    return AgeEventNode;
}(EventNode));
light.NodeContext.registerNode(AgeEventNode);

var AndNode = /** @class */ (function (_super) {
    __extends(AndNode, _super);
    function AndNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AndNode.prototype.Run = function () {
        if (this.A && this.B) {
            this.True();
        }
        else {
            this.False();
        }
    };
    AndNode.prototype.True = function () { };
    AndNode.prototype.False = function () { };
    AndNode.nodeType = 'code/And';
    return AndNode;
}(Node));
light.NodeContext.registerNode(AndNode);

var CompareNode = /** @class */ (function (_super) {
    __extends(CompareNode, _super);
    function CompareNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    CompareNode.prototype.Run = function () {
        if (this.A == this.B) {
            this.Equal();
        }
        else if (this.A > this.B) {
            this.Bigger();
        }
        else {
            this.Smaller();
        }
    };
    CompareNode.prototype.Equal = function () { };
    CompareNode.prototype.Bigger = function () { };
    CompareNode.prototype.Smaller = function () { };
    CompareNode.nodeType = 'code/compare';
    return CompareNode;
}(Node));
light.NodeContext.registerNode(CompareNode);

var ConstNumberNode = /** @class */ (function (_super) {
    __extends(ConstNumberNode, _super);
    function ConstNumberNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object.defineProperty(ConstNumberNode.prototype, "Value", {
        get: function () {
            return this._value;
        },
        set: function (v) {
            this._value = v;
        },
        enumerable: false,
        configurable: true
    });
    ConstNumberNode.nodeType = 'code/constNumber';
    return ConstNumberNode;
}(Node));
light.NodeContext.registerNode(ConstNumberNode);

var CorrelationNode = /** @class */ (function (_super) {
    __extends(CorrelationNode, _super);
    function CorrelationNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.faceIndex = -1;
        return _this;
    }
    CorrelationNode.prototype.Run = function () {
        var _this = this;
        this.entityToDisplay.forEach(function (entityId) {
            getRelateEntities(entityId, _this.entityManager).forEach(function (entity) {
                if (entity) {
                    var component = entity.getComponent(light.FaceTracking);
                    if (component) {
                        var index = new light.VectorInt();
                        index.add(_this.faceIndex);
                        component.faceIndex = index;
                    }
                }
            });
        });
        this.Next();
    };
    CorrelationNode.prototype.Next = function () { };
    CorrelationNode.nodeType = 'code/Correlation';
    return CorrelationNode;
}(Node));
light.NodeContext.registerNode(CorrelationNode);

var DelayNode = /** @class */ (function (_super) {
    __extends(DelayNode, _super);
    function DelayNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.duration = 1000000;
        return _this;
    }
    DelayNode.prototype.Run = function () {
        var _this = this;
        var currentTime = light.getCurrentTime();
        var invoked = false;
        light.on('update', function (time) {
            if (time - currentTime >= _this.duration && !invoked) {
                invoked = true;
                _this.Next();
            }
        });
    };
    DelayNode.prototype.Next = function () { };
    DelayNode.nodeType = 'code/Delay';
    return DelayNode;
}(Node));
light.NodeContext.registerNode(DelayNode);

var FaceNode = /** @class */ (function (_super) {
    __extends(FaceNode, _super);
    function FaceNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.times = 1;
        _this.AI_FEATURE = [
            "Expression" /* EXPRESSION */,
            "Age" /* AGE */,
            "Gender" /* GENDER */,
            "Smile" /* SMILE */,
        ];
        return _this;
    }
    FaceNode.prototype.Run = function () {
        this.actionEventName = actionNameToEvent(light.FaceAction[light.FaceAction.FaceDetected]);
        openAIFeature(this.AI_FEATURE, this.entityManager, this.eventManager);
        light.on('update', this.onUpdate.bind(this));
    };
    FaceNode.prototype.onUpdate = function () {
        var _this = this;
        var data = getAIClassData(this.AI_FEATURE, this.entityManager);
        var expressions = data === null || data === void 0 ? void 0 : data[this.actionEventName];
        var detected = expressions && expressions.length > 0;
        if (detected) {
            var faces = this.getAllFaceInfo(data);
            faces.forEach(function (face) {
                _this.faceOutput = face;
                _this.onNext();
            });
            this.times += 1;
            var trackId = expressions[0];
            if (detected !== this.lastState) {
                console.log("FaceNode: onDetected: " + trackId);
                this.onDetected();
            }
        }
        else if (this.lastState) {
            console.log('FaceNode: Lost');
            this.Lost();
        }
        this.lastState = detected;
    };
    FaceNode.prototype.onNext = function () { };
    FaceNode.prototype.Lost = function () { };
    FaceNode.prototype.onDetected = function () { };
    FaceNode.prototype.getAllFaceInfo = function (params) {
        if (params === void 0) { params = {}; }
        var keys = Object.keys(params);
        var allFaceInfo = []; // 人脸数据总集
        var _loop_1 = function (i, size) {
            console.log("expression_Key:::" + keys[i]);
            switch (keys[i]) {
                case 'onAge': {
                    var ageInfo_1 = params.onAge; // [[1, 29], [2, 24]]
                    if (ageInfo_1) {
                        var _loop_2 = function (j, ageCount) {
                            // 先找trackID，找得到就更新age，找不到就新增一个
                            var exist = allFaceInfo.find(function (info) { return info.trackID === ageInfo_1[j][0]; });
                            if (exist) {
                                exist.age = ageInfo_1[j][1];
                            }
                            else {
                                var faceInfo = {
                                    trackID: ageInfo_1[j][0],
                                    age: ageInfo_1[j][1],
                                };
                                allFaceInfo.push(faceInfo);
                            }
                        };
                        for (var j = 0, ageCount = ageInfo_1.length; j < ageCount; j++) {
                            _loop_2(j, ageCount);
                        }
                    }
                    break;
                }
                case 'onMale': {
                    var maleInfo_1 = params.onMale; // [1,2,3]
                    if (maleInfo_1) {
                        var _loop_3 = function (j, maleCount) {
                            var exist = allFaceInfo.find(function (info) { return info.trackID === maleInfo_1[j]; });
                            if (exist) {
                                exist.gender = 0;
                            }
                            else {
                                var faceInfo = {
                                    trackID: maleInfo_1[j],
                                    gender: 0,
                                    faceIndex: allFaceInfo.length,
                                };
                                allFaceInfo.push(faceInfo);
                            }
                        };
                        for (var j = 0, maleCount = maleInfo_1.length; j < maleCount; j++) {
                            _loop_3(j, maleCount);
                        }
                    }
                    break;
                }
                case 'onFemale': {
                    var femaleInfo_1 = params.onFemale; // [4,5,6]
                    if (femaleInfo_1) {
                        var _loop_4 = function (j, femaleCount) {
                            var exist = allFaceInfo.find(function (info) { return info.trackID === femaleInfo_1[j]; });
                            if (exist) {
                                exist.gender = 1;
                            }
                            else {
                                var faceInfo = {
                                    trackID: femaleInfo_1[j],
                                    gender: 1,
                                    faceIndex: allFaceInfo.length,
                                };
                                allFaceInfo.push(faceInfo);
                            }
                        };
                        for (var j = 0, femaleCount = femaleInfo_1.length; j < femaleCount; j++) {
                            _loop_4(j, femaleCount);
                        }
                    }
                    break;
                }
                case 'onFacedetected':
                case 'onOpenmouth':
                case 'onBlinkeyebrow':
                case 'onBlinkeye':
                case 'onShakehead':
                case 'onKiss':
                case 'onBlinklefteye':
                case 'onBlinkrighteye':
                case 'onNod':
                case 'onSmile':
                case 'onMouthoccluded':
                case 'onLefteyeoccluded':
                case 'onRighteyeoccluded':
                case 'onDoubleeyeoccluded': {
                    var expressionInfo_1 = params[keys[i]];
                    if (expressionInfo_1) {
                        var _loop_5 = function (j, count) {
                            var exist = allFaceInfo.find(function (info) { return info.trackID === expressionInfo_1[j]; });
                            if (exist) {
                                if (!exist.expression) {
                                    exist.expression = [];
                                }
                                exist.expression.push(keys[i]);
                            }
                            else {
                                var faceInfo = {
                                    trackID: expressionInfo_1[j],
                                    faceIndex: allFaceInfo.length,
                                    expression: [keys[i]],
                                };
                                allFaceInfo.push(faceInfo);
                            }
                        };
                        for (var j = 0, count = expressionInfo_1.length; j < count; j++) {
                            _loop_5(j, count);
                        }
                    }
                    break;
                }
            }
        };
        for (var i = 0, size = keys.length; i < size; i++) {
            _loop_1(i);
        }
        return allFaceInfo;
    };
    FaceNode.nodeType = 'code/Face';
    return FaceNode;
}(EventNode));
light.NodeContext.registerNode(FaceNode);

var AIFeature$1;
(function (AIFeature) {
    AIFeature["onSmile"] = "Smile";
    AIFeature["onKiss"] = "Pout";
})(AIFeature$1 || (AIFeature$1 = {}));
var DefaultAIFeature$1 = "Expression" /* EXPRESSION */;
var FaceActionNode = /** @class */ (function (_super) {
    __extends(FaceActionNode, _super);
    function FaceActionNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.lastExpressionInfo = {};
        return _this;
    }
    FaceActionNode.prototype.onDetected = function () { };
    FaceActionNode.prototype.Lost = function () { };
    FaceActionNode.prototype.Run = function () {
        var _a, _b;
        this.actionEventName = actionNameToEvent(light.FaceAction[this.faceAction]);
        if (((_a = this.faceInput) === null || _a === void 0 ? void 0 : _a.expression) && ((_b = this.faceInput) === null || _b === void 0 ? void 0 : _b.expression.indexOf(this.actionEventName)) > -1) {
            this.faceOutput = this.faceInput;
            this.onDetected();
        }
        light.on('update', this.onUpdate.bind(this));
    };
    FaceActionNode.prototype.onUpdate = function () {
        var aifeature = AIFeature$1[this.actionEventName] ? AIFeature$1[this.actionEventName] : DefaultAIFeature$1;
        var data = getAIClassData([aifeature], this.entityManager);
        var expressions = data === null || data === void 0 ? void 0 : data[this.actionEventName];
        var detected = expressions && expressions.length > 0;
        if (!detected && this.lastState) {
            this.Lost();
        }
        this.lastState = detected;
    };
    FaceActionNode.nodeType = 'code/FaceAction';
    return FaceActionNode;
}(Node));
light.NodeContext.registerNode(FaceActionNode);

var GenderNode = /** @class */ (function (_super) {
    __extends(GenderNode, _super);
    function GenderNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.lastGenderInfo = {};
        return _this;
    }
    GenderNode.prototype.Run = function () {
        if (!this.faceInput) {
            return;
        }
        this.faceOutput = this.faceInput;
        if (this.faceInput.gender === 0) {
            console.log('this.faceInput.gender:male');
            if (!this.lastGenderInfo[this.faceInput.trackID]) {
                this.onMale();
            }
        }
        else if (this.faceInput.gender === 1) {
            console.log('this.faceInput.gender:Female');
            if (!this.lastGenderInfo[this.faceInput.trackID]) {
                this.onFemale();
            }
        }
        this.lastGenderInfo[this.faceInput.trackID] = this.faceInput.gender;
    };
    GenderNode.prototype.onMale = function () { };
    GenderNode.prototype.onFemale = function () { };
    GenderNode.nodeType = 'code/Gender';
    return GenderNode;
}(Node));
light.NodeContext.registerNode(GenderNode);

var GenderEventNode = /** @class */ (function (_super) {
    __extends(GenderEventNode, _super);
    function GenderEventNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.lastMaleInfo = {};
        _this.lastFemaleInfo = {};
        _this.times = 1;
        return _this;
    }
    GenderEventNode.prototype.Run = function () {
        openAIFeature(["Gender" /* GENDER */], this.entityManager, this.eventManager);
        light.on('update', this.onUpdate.bind(this));
    };
    GenderEventNode.prototype.onUpdate = function () {
        var _this = this;
        var data = getAIClassData(["Gender" /* GENDER */], this.entityManager);
        if (data['onMale']) {
            if (!this.lastMaleInfo[data['onMale'].join(',')]) {
                data['onMale'].forEach(function (id) {
                    _this.genderOutput = {
                        trackID: id,
                        gender: 0,
                    };
                    _this.onMale();
                    _this.times += 1;
                });
                this.lastMaleInfo[data['onMale'].join(',')] = true;
            }
        }
        if (data['onFemale']) {
            if (!this.lastFemaleInfo[['onFemale'].join(',')]) {
                data['onFemale'].forEach(function (id) {
                    _this.genderOutput = {
                        trackID: id,
                        gender: 1,
                    };
                    _this.onFemale();
                    _this.times += 1;
                });
                this.lastFemaleInfo[data['onFemale'].join(',')] = true;
            }
        }
    };
    GenderEventNode.prototype.onMale = function () { };
    GenderEventNode.prototype.onFemale = function () { };
    GenderEventNode.nodeType = 'code/GenderEvent';
    return GenderEventNode;
}(EventNode));
light.NodeContext.registerNode(GenderEventNode);

var GradientType;
(function (GradientType) {
    GradientType[GradientType["LINEAR"] = 1] = "LINEAR";
    GradientType[GradientType["EASE_IN"] = 2] = "EASE_IN";
    GradientType[GradientType["EASE_OUT"] = 3] = "EASE_OUT";
    GradientType[GradientType["EASE_IN_OUT"] = 4] = "EASE_IN_OUT";
    GradientType[GradientType["SIN"] = 5] = "SIN";
    GradientType[GradientType["COS"] = 6] = "COS";
})(GradientType || (GradientType = {}));
var GradientNode = /** @class */ (function (_super) {
    __extends(GradientNode, _super);
    function GradientNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.currentTime = 0;
        _this.timeStart = -1;
        _this.listened = false;
        return _this;
    }
    GradientNode.prototype.onUpdate = function (time) {
        // currentTime为初始值0时，更新timeStart
        if (this.currentTime === 0) {
            this.timeStart = time;
        }
        this.currentTime = time;
        if (this.timeStart === -1) {
            return;
        }
        if (time / 1000 - this.timeStart / 1000 <= this.duration / 1000) {
            var current = (this.currentTime - this.timeStart) / 1000;
            var start = this.from;
            var range = this.to - this.from;
            var duration = this.duration / 1000;
            var handler = {};
            handler[GradientType.LINEAR] = this.linear;
            handler[GradientType.EASE_IN] = this.easeInQuad;
            handler[GradientType.EASE_OUT] = this.easeOutQuad;
            handler[GradientType.EASE_IN_OUT] = this.easeInOutQuad;
            handler[GradientType.SIN] = this.sin;
            handler[GradientType.COS] = this.cos;
            this.CurrentValue = handler[this.gradientType](current, start, range, duration);
            this.Next();
        }
        else {
            this.Finish();
            this.timeStart = -1;
        }
    };
    GradientNode.prototype.Run = function () {
        if (this.to === this.from || this.duration === 0) { // 校验输入值
            return;
        }
        if (this.currentTime === undefined) {
            this.currentTime = this.from;
        }
        // 首次调用Run时，currentTime为初始值0，导致timeStart非准确start时间
        this.timeStart = this.currentTime;
        if (this.listened) {
            return;
        }
        this.listened = true;
        light.on('update', this.onUpdate.bind(this));
    };
    GradientNode.prototype.Next = function () { };
    GradientNode.prototype.easeInOutQuad = function (currentTime, startValue, rangeValue, duration) {
        currentTime /= duration / 2;
        if (currentTime < 1)
            return rangeValue / 2 * currentTime * currentTime + startValue;
        currentTime -= 1;
        return -rangeValue / 2 * (currentTime * (currentTime - 2) - 1) + startValue;
    };
    GradientNode.prototype.easeOutQuad = function (currentTime, startValue, rangeValue, duration) {
        currentTime /= duration;
        return -rangeValue * currentTime * (currentTime - 2) + startValue;
    };
    GradientNode.prototype.easeInQuad = function (currentTime, startValue, rangeValue, duration) {
        currentTime /= duration;
        return rangeValue * currentTime * currentTime + startValue;
    };
    GradientNode.prototype.linear = function (currentTime, startValue, rangeValue, duration) {
        return rangeValue * currentTime / duration + startValue;
    };
    GradientNode.prototype.sin = function (currentTime, startValue, rangeValue, duration) {
        return Math.sin(2 * Math.PI * currentTime / duration) * rangeValue + startValue;
    };
    GradientNode.prototype.cos = function (currentTime, startValue, rangeValue, duration) {
        return Math.cos(2 * Math.PI * currentTime / duration) * rangeValue + startValue;
    };
    GradientNode.prototype.Finish = function () { };
    GradientNode.nodeType = 'code/Gradient';
    return GradientNode;
}(Node));
light.NodeContext.registerNode(GradientNode);

var IfNode = /** @class */ (function (_super) {
    __extends(IfNode, _super);
    function IfNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._dep = false;
        return _this;
    }
    Object.defineProperty(IfNode.prototype, "dep", {
        get: function () {
            return this._dep;
        },
        set: function (v) {
            this._dep = v;
        },
        enumerable: false,
        configurable: true
    });
    IfNode.prototype.Run = function () {
        if (this.dep) {
            this.True();
        }
        else {
            this.False();
        }
    };
    IfNode.prototype.True = function () { };
    IfNode.prototype.False = function () { };
    IfNode.nodeType = 'code/if';
    return IfNode;
}(Node));
light.NodeContext.registerNode(IfNode);

var InvertNode = /** @class */ (function (_super) {
    __extends(InvertNode, _super);
    function InvertNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    InvertNode.prototype.Run = function () {
        if (!this.A) {
            this.True();
        }
        else {
            this.False();
        }
    };
    InvertNode.prototype.True = function () { };
    InvertNode.prototype.False = function () { };
    InvertNode.nodeType = 'code/Invert';
    return InvertNode;
}(Node));
light.NodeContext.registerNode(InvertNode);

var LutNode = /** @class */ (function (_super) {
    __extends(LutNode, _super);
    function LutNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.strengthInput = -1;
        return _this;
    }
    LutNode.prototype.Run = function () {
        showEntities([this.lutEntityId], this.entityManager);
        var entity = this.entityManager.getEntityById(this.lutEntityId);
        var lut = entity.getComponent(light.LUTFilter);
        if (lut) {
            lut.intensity = this.strengthInput >= 0 ? this.strengthInput : this.strength;
        }
        this.Next();
    };
    LutNode.prototype.Next = function () { };
    LutNode.nodeType = 'code/Lut';
    return LutNode;
}(Node));
light.NodeContext.registerNode(LutNode);

var OrNode = /** @class */ (function (_super) {
    __extends(OrNode, _super);
    function OrNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    OrNode.prototype.Run = function () {
        if (this.A || this.B) {
            this.True();
        }
        else {
            this.False();
        }
    };
    OrNode.prototype.True = function () { };
    OrNode.prototype.False = function () { };
    OrNode.nodeType = 'code/Or';
    return OrNode;
}(Node));
light.NodeContext.registerNode(OrNode);

var BranchMode;
(function (BranchMode) {
    BranchMode[BranchMode["Queue"] = 1] = "Queue";
    BranchMode[BranchMode["Random"] = 2] = "Random";
})(BranchMode || (BranchMode = {}));
var QueueBranchNode = /** @class */ (function (_super) {
    __extends(QueueBranchNode, _super);
    function QueueBranchNode() {
        var _this = _super.call(this) || this;
        _this.currentIndex = 0;
        _this.branchMode = BranchMode.Queue;
        _this.randomStart = false;
        if (_this.randomStart && _this.branchMode === BranchMode.Queue) {
            _this.currentIndex = Math.floor(Math.random() * 10);
        }
        return _this;
    }
    QueueBranchNode.prototype.Run = function () {
        var Limit = 5;
        var max = 0;
        for (var i = 0; i < Limit; i++) {
            if (this["Next" + i]) {
                max = max + 1;
            }
            else {
                break;
            }
        }
        if (this.branchMode === BranchMode.Queue) {
            this["Next" + this.currentIndex % max]();
            this.currentIndex = this.currentIndex + 1;
        }
        else {
            this["Next" + Math.floor(Math.random() * max)]();
        }
    };
    QueueBranchNode.nodeType = 'code/queueBranch';
    return QueueBranchNode;
}(Node));
light.NodeContext.registerNode(QueueBranchNode);

var RandomNode = /** @class */ (function (_super) {
    __extends(RandomNode, _super);
    function RandomNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.toShow = [];
        _this.toHide = [];
        _this.init = false;
        return _this;
    }
    RandomNode.prototype.Run = function () {
        this.toShow = [];
        this.toHide = [];
        if (this.entityToDisplay.length) {
            var entityId = this.entityToDisplay[Math.floor(Math.random() * this.entityToDisplay.length)];
            var entity = this.entityManager.getEntityById(entityId);
            if (entity) {
                this.toShow.push(entityId);
            }
        }
        if (this.entityToHide.length) {
            var entityId = this.entityToHide[Math.floor(Math.random() * this.entityToHide.length)];
            this.toHide.push(entityId);
        }
        showEntities(this.toShow, this.entityManager);
        hideEntities(this.toHide, this.entityManager);
        this.Next();
    };
    RandomNode.prototype.Next = function () { };
    RandomNode.nodeType = 'code/Random';
    return RandomNode;
}(Node));
light.NodeContext.registerNode(RandomNode);

var RandomNumberNode = /** @class */ (function (_super) {
    __extends(RandomNumberNode, _super);
    function RandomNumberNode() {
        var _this = _super.call(this) || this;
        _this.isInteger = false;
        _this.setValue();
        return _this;
    }
    RandomNumberNode.prototype.setValue = function () {
        if (this.isInteger) {
            return Math.round(Math.round(this.from) + (Math.random() * (this.to - this.from)));
        }
        return this.from + (Math.random() * (this.to - this.from));
    };
    RandomNumberNode.prototype.Run = function () {
        this.Value = this.setValue();
        this.Next();
    };
    RandomNumberNode.prototype.Next = function () { };
    RandomNumberNode.nodeType = 'code/randomNumber';
    return RandomNumberNode;
}(Node));
light.NodeContext.registerNode(RandomNumberNode);

var SumNode = /** @class */ (function (_super) {
    __extends(SumNode, _super);
    function SumNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.A = 0;
        _this.B = 0;
        return _this;
    }
    SumNode.prototype.Run = function () {
        this.Sum = this.A + this.B;
        this.Diff = this.A - this.B;
        this.Multiply = this.A * this.B;
        this.Divide = this.B === 0 ? 0 : this.A / this.B;
        this.Complement = this.A % this.B;
        this.Next();
    };
    SumNode.prototype.Next = function () {
    };
    SumNode.nodeType = 'code/Sum';
    return SumNode;
}(Node));
light.NodeContext.registerNode(SumNode);

var TransformNode = /** @class */ (function (_super) {
    __extends(TransformNode, _super);
    function TransformNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.xInput = 0;
        _this.yInput = 0;
        _this.wInput = 0;
        _this.hInput = 0;
        _this.rotationInput = 0;
        _this.scalexInput = 1;
        _this.scaleyInput = 1;
        _this.accumulation = true;
        return _this;
    }
    TransformNode.prototype.Run = function () {
        if (!this.entityId) {
            this.Next();
            return;
        }
        var entity = this.entityManager.getEntityById(this.entityId);
        var cmp = entity.getComponent(light.ScreenTransform);
        cmp.offset.bottom = this.accumulation ? (cmp.offset.bottom - this.hInput / 2)
            : (this.hInput || cmp.offset.bottom);
        cmp.offset.top = this.accumulation ? (cmp.offset.top + this.hInput / 2)
            : (this.hInput || cmp.offset.top);
        cmp.offset.left = this.accumulation ? (cmp.offset.left - this.wInput / 2)
            : (this.wInput || cmp.offset.left);
        cmp.offset.right = this.accumulation ? (cmp.offset.right + this.wInput / 2)
            : (this.wInput || cmp.offset.right);
        cmp.anchor.left = this.accumulation ? (cmp.anchor.left + this.xInput)
            : (this.xInput || cmp.anchor.left);
        cmp.anchor.right = this.accumulation ? (cmp.anchor.right + this.xInput)
            : (this.xInput || cmp.anchor.right);
        cmp.anchor.top = this.accumulation ? (cmp.anchor.top + this.yInput)
            : (this.yInput || cmp.anchor.top);
        cmp.anchor.bottom = this.accumulation ? (cmp.anchor.bottom + this.yInput)
            : (this.yInput || cmp.anchor.bottom);
        var _a = cmp.rotation, x = _a.x, y = _a.y, z = _a.z, w = _a.w;
        var euler = new Euler(x, y, z);
        var quater = new Quaternion(x, y, z, w);
        euler.setFromQuaternion(quater);
        euler.z = this.accumulation ? euler.z + this.rotationInput / 180 * Math.PI
            : (this.rotationInput / 180 * Math.PI || euler.z);
        quater.setFromEuler(euler);
        cmp.rotation.x = quater.x;
        cmp.rotation.y = quater.y;
        cmp.rotation.z = quater.z;
        cmp.rotation.w = quater.w;
        var xSymbol = cmp.scale.x / Math.abs(cmp.scale.x);
        var ySymbol = cmp.scale.y / Math.abs(cmp.scale.y);
        cmp.scale.x = xSymbol * this.scalexInput + (this.accumulation ? cmp.scale.x : 0);
        cmp.scale.y = ySymbol * this.scaleyInput + (this.accumulation ? cmp.scale.y : 0);
        this.Next();
    };
    TransformNode.prototype.Next = function () {
    };
    TransformNode.nodeType = 'code/Transform';
    return TransformNode;
}(Node));
light.NodeContext.registerNode(TransformNode);

var TransformNode$1 = /** @class */ (function (_super) {
    __extends(TransformNode, _super);
    function TransformNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.X = 0;
        _this.Y = 0;
        _this.Z = 0;
        _this.xInput = 0;
        _this.yInput = 0;
        _this.zInput = 0;
        _this.rotateX = 0;
        _this.rotateY = 0;
        _this.rotateZ = 0;
        _this.rotateXInput = 0;
        _this.rotateYInput = 0;
        _this.rotateZInput = 0;
        _this.scaleX = 0;
        _this.scaleY = 0;
        _this.scaleZ = 0;
        _this.scaleXInput = 0;
        _this.scaleYInput = 0;
        _this.scaleZInput = 0;
        return _this;
    }
    TransformNode.prototype.Run = function () {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k, _l, _m, _o, _p, _q, _r, _s, _t, _u, _v, _w, _x, _y, _z;
        if (!this.entityId) {
            this.Next();
            return;
        }
        var entity = this.entityManager.getEntityById(this.entityId);
        var cmp = entity.getComponent(light.BasicTransform);
        if (cmp) {
            var vec3 = new light.Vec3((_c = (_b = (_a = this.xInput) !== null && _a !== void 0 ? _a : this.X) !== null && _b !== void 0 ? _b : cmp.position.x) !== null && _c !== void 0 ? _c : 0, (_f = (_e = (_d = this.yInput) !== null && _d !== void 0 ? _d : this.Y) !== null && _e !== void 0 ? _e : cmp.position.y) !== null && _f !== void 0 ? _f : 0, (_j = (_h = (_g = this.zInput) !== null && _g !== void 0 ? _g : this.Z) !== null && _h !== void 0 ? _h : cmp.position.z) !== null && _j !== void 0 ? _j : 0);
            var svec3 = new light.Vec3((_m = (_l = (_k = this.scaleXInput) !== null && _k !== void 0 ? _k : this.scaleX) !== null && _l !== void 0 ? _l : cmp.scale.x) !== null && _m !== void 0 ? _m : 0, (_q = (_p = (_o = this.scaleYInput) !== null && _o !== void 0 ? _o : this.scaleY) !== null && _p !== void 0 ? _p : cmp.scale.y) !== null && _q !== void 0 ? _q : 0, (_t = (_s = (_r = this.scaleZInput) !== null && _r !== void 0 ? _r : this.scaleZ) !== null && _s !== void 0 ? _s : cmp.scale.z) !== null && _t !== void 0 ? _t : 0);
            var _0 = cmp.rotation, x = _0.x, y = _0.y, z = _0.z, w = _0.w;
            var valueX = (_v = (_u = this.rotateXInput) !== null && _u !== void 0 ? _u : this.rotateX) !== null && _v !== void 0 ? _v : x;
            var valueY = (_x = (_w = this.rotateYInput) !== null && _w !== void 0 ? _w : this.rotateY) !== null && _x !== void 0 ? _x : y;
            var valueZ = (_z = (_y = this.rotateZInput) !== null && _y !== void 0 ? _y : this.rotateZ) !== null && _z !== void 0 ? _z : z;
            valueX = valueX / 180 * Math.PI;
            valueY = valueY / 180 * Math.PI;
            valueZ = valueZ / 180 * Math.PI;
            var euler = new Euler(x, y, z);
            var quater = new Quaternion(x, y, z, w);
            euler.setFromQuaternion(quater);
            euler.x = valueX;
            euler.y = valueY;
            euler.z = valueZ;
            quater.setFromEuler(euler);
            cmp.SetRotation(new light.Quat(quater.w, quater.x, quater.y, quater.z));
            cmp.SetScale(svec3);
            cmp.SetPosition(vec3);
            this.Next();
        }
    };
    TransformNode.prototype.Next = function () {
    };
    TransformNode.nodeType = 'code/Transform3D';
    return TransformNode;
}(Node));
light.NodeContext.registerNode(TransformNode$1);

var LoopType$2;
(function (LoopType) {
    LoopType["FIXED"] = "\u56FA\u5B9A\u6570\u5B57";
    LoopType["INFINITE"] = "\u65E0\u9650\u5FAA\u73AF";
})(LoopType$2 || (LoopType$2 = {}));
var UpdateNode = /** @class */ (function (_super) {
    __extends(UpdateNode, _super);
    function UpdateNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.repeatTimes = 1;
        _this.frame = 1;
        _this.currentFrame = 0;
        _this.startupTrigger = false; // 刚开始就触发一次
        _this.currentTimes = 0;
        return _this;
    }
    UpdateNode.prototype.Run = function () {
        if (this.startupTrigger && this.repeatTimes !== 0) {
            this.Update();
            this.currentTimes = this.currentTimes + 1;
        }
        light.on('update', this.onFrameUpdate.bind(this));
    };
    UpdateNode.prototype.onFrameUpdate = function (_val) {
        if (this.currentFrame !== 0) {
            var trigger = (this.currentFrame % this.frame) === 0;
            var infinite = this.loopType === LoopType$2.INFINITE;
            var eligible = this.repeatTimes > this.currentTimes;
            if (infinite || eligible) {
                if (trigger) {
                    this.Update();
                    this.currentTimes = this.currentTimes + 1;
                }
            }
        }
        this.currentFrame += 1;
    };
    UpdateNode.prototype.Update = function () { };
    UpdateNode.nodeType = 'code/Update';
    return UpdateNode;
}(EventNode));
light.NodeContext.registerNode(UpdateNode);

var LoopType$3;
(function (LoopType) {
    LoopType["FIXED"] = "\u56FA\u5B9A\u6570\u5B57";
    LoopType["INFINITE"] = "\u65E0\u9650\u5FAA\u73AF";
})(LoopType$3 || (LoopType$3 = {}));
var TimesNode = /** @class */ (function (_super) {
    __extends(TimesNode, _super);
    function TimesNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.total = 2;
        _this.times = 1;
        _this.repeat = 1;
        return _this;
    }
    TimesNode.prototype.Run = function () {
        var repeat = this.loopType === LoopType$3.INFINITE ? Infinity : this.repeat;
        if (this.times > this.total * repeat) {
            return;
        }
        var index = (this.times - 1) % this.total;
        if (!this["Next" + (index + 1)]) {
            this["Next" + (index + 1)] = function () { };
        }
        this["Next" + (index + 1)]();
        this.times = this.times + 1;
    };
    TimesNode.nodeType = 'code/times';
    return TimesNode;
}(Node));
light.NodeContext.registerNode(TimesNode);

var LoopType$4;
(function (LoopType) {
    LoopType["FIXED"] = "\u56FA\u5B9A\u6570\u5B57";
    LoopType["INFINITE"] = "\u65E0\u9650\u5FAA\u73AF";
})(LoopType$4 || (LoopType$4 = {}));
var play = /** @class */ (function (_super) {
    __extends(play, _super);
    function play() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.init = false;
        _this.keepLastFrame = false;
        _this.playtimes = 1;
        return _this;
    }
    play.prototype.Run = function () {
        var _this = this;
        var entity = this.entityManager.getEntityById(this.entityId);
        if (entity) {
            showEntities([this.entityId], this.entityManager);
            if (this.loopType === LoopType$4.INFINITE) {
                return;
            }
            var timeOffset = entity.getComponent(light.TimeOffset);
            var timeContrl = entity.getComponent(light.TimeControl);
            var timeContrlList = entity.getComponent(light.TimeControlList);
            if (timeOffset) {
                if (timeContrlList === null || timeContrlList === void 0 ? void 0 : timeContrlList.timeControlArray) {
                    for (var i = 0; i < timeContrlList.timeControlArray.size(); i++) {
                        timeContrlList.timeControlArray.get(i).reset();
                        timeContrlList.timeControlArray.get(i).currentTime = 0;
                    }
                }
                else if (timeContrl) {
                    timeContrl.reset();
                    timeContrl.currentTime = 0;
                }
                else {
                    timeOffset.startOffset = light.getCurrentTime();
                }
                timeOffset.loopCount = this.playtimes - 1;
                timeOffset.visibleWhileOverTime = this.keepLastFrame;
                var currentTime = light.getCurrentTime();
                var endTime_1 = currentTime + (timeOffset.duration
                    * (this.loopType === LoopType$4.INFINITE ? Infinity : this.playtimes));
                var invoked_1 = false;
                light.on('update', function (time) {
                    if (time > endTime_1 && !invoked_1) {
                        invoked_1 = true;
                        _this.Finish();
                    }
                });
            }
        }
        this.Next();
    };
    play.prototype.Pause = function () {
        var entity = this.entityManager.getEntityById(this.entityId);
        if (entity) {
            var timeOffset = entity.getComponent(light.TimeOffset);
            var timeContrl = entity.getComponent(light.TimeControl);
            if (timeContrl) {
                timeContrl.pause = true;
                timeContrl.loopCount = 0;
                timeContrl.visibleWhileOverTime = true;
            }
            if (timeOffset) {
                timeOffset.loopCount = 0;
                timeOffset.visibleWhileOverTime = true;
            }
        }
    };
    play.prototype.Next = function () { };
    play.prototype.Finish = function () { };
    play.nodeType = 'code/play';
    return play;
}(Node));
light.NodeContext.registerNode(play);

var SnapshotNode = /** @class */ (function (_super) {
    __extends(SnapshotNode, _super);
    function SnapshotNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    SnapshotNode.prototype.Run = function () {
        var _this = this;
        showEntities([this.entityId], this.entityManager);
        light.once('update', function () {
            hideEntities([_this.entityId], _this.entityManager);
        });
        this.Next();
    };
    SnapshotNode.prototype.Next = function () { };
    SnapshotNode.nodeType = 'code/Snapshot';
    return SnapshotNode;
}(Node));
light.NodeContext.registerNode(SnapshotNode);

var TransformNode$2 = /** @class */ (function (_super) {
    __extends(TransformNode, _super);
    function TransformNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.entities = [];
        _this.duration = 1000000;
        return _this;
    }
    TransformNode.prototype.Run = function () {
        var _this = this;
        if (this.entities.length < 2) {
            return;
        }
        var originCoor = {};
        this.entities.forEach(function (entityId) {
            var entity = _this.entityManager.getEntityById(entityId);
            if (entity) {
                var cmp = entity.getComponent(light.ScreenTransform);
                if (cmp) {
                    originCoor[entityId] = {
                        left: cmp.anchor.left,
                        right: cmp.anchor.right,
                        top: cmp.anchor.top,
                        bottom: cmp.anchor.bottom,
                    };
                }
            }
        });
        if (Object.keys(originCoor).length < 2) {
            return;
        }
        this.entities.forEach(function (entityId, i) {
            var from = _this.entityManager.getEntityById(entityId);
            var targetId = _this.entities[i + 1] || _this.entities[0];
            var pos = ['left', 'right', 'top', 'bottom'];
            var invoke = (new Array(pos.length)).fill(false);
            var timeStart = (new Array(pos.length)).fill(light.getCurrentTime());
            light.on('update', function (time) {
                if (invoke.every(function (i) { return i == true; })) {
                    return;
                }
                pos.forEach(function (pos, idx) {
                    var currentTime = light.getCurrentTime();
                    var transform = from.getComponent(light.ScreenTransform);
                    if (timeStart[idx] === -1 || invoke[idx]) {
                        return;
                    }
                    if (time - timeStart[idx] <= _this.duration) {
                        if (transform) {
                            var value = _this.linear((currentTime - timeStart[idx]) / 1000, originCoor[entityId][pos], originCoor[targetId][pos] - originCoor[entityId][pos], _this.duration / 1000);
                            transform.anchor[pos] = value;
                        }
                        currentTime = time;
                    }
                    else {
                        if (transform) {
                            transform.anchor[pos] = originCoor[targetId][pos];
                        }
                        timeStart[idx] = -1;
                        invoke[idx] = true;
                    }
                });
            });
        });
        this.Next();
    };
    TransformNode.prototype.linear = function (currentTime, startValue, changeValue, duration) {
        return changeValue * currentTime / duration + startValue;
    };
    TransformNode.prototype.Next = function () {
    };
    TransformNode.nodeType = 'code/Translate';
    return TransformNode;
}(Node));
light.NodeContext.registerNode(TransformNode$2);

var LotteryNode = /** @class */ (function (_super) {
    __extends(LotteryNode, _super);
    function LotteryNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.from = 0;
        _this.to = 1;
        _this.repeat = false;
        _this.n = 2;
        _this.isInteger = false;
        return _this;
    }
    LotteryNode.prototype.Run = function () {
        var _a = this, n = _a.n, repeat = _a.repeat, from = _a.from, to = _a.to;
        var result = [];
        if (to - from + 1 >= n) { // 判断区间内是否可以产生n个不重复的值
            if (repeat) {
                while (result.length !== n) {
                    result.push(this.generateRandom(from, to));
                }
            }
            else {
                while ((new Set(result)).size !== n) {
                    result.push(this.generateRandom(from, to));
                }
                result = Array.from((new Set(result)));
            }
        }
        else {
            while (result.length !== n) {
                result.push(this.generateRandom(from, to));
            }
        }
        for (var i = 0; i < n; i++) {
            this["number" + (i + 1)] = result[i];
        }
        this.Next();
    };
    LotteryNode.prototype.generateRandom = function (from, to) {
        if (this.isInteger) {
            return Math.round(Math.round(from) + (Math.random() * (to - from)));
        }
        return from + (Math.random() * (to - from));
    };
    LotteryNode.prototype.Next = function () { };
    LotteryNode.nodeType = 'code/Lottery';
    return LotteryNode;
}(Node));
light.NodeContext.registerNode(LotteryNode);

var IfNode$1 = /** @class */ (function (_super) {
    __extends(IfNode, _super);
    function IfNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.time1 = -1;
        _this.time2 = -1;
        _this.gap = 1000000;
        return _this;
    }
    IfNode.prototype.Event1 = function () {
        this.time1 = light.getCurrentTime();
        if (this.time2 !== -1 && Math.abs(this.time1 - this.time2) <= this.gap) {
            this.Next();
        }
    };
    IfNode.prototype.Event2 = function () {
        this.time2 = light.getCurrentTime();
        if (this.time1 !== -1 && Math.abs(this.time1 - this.time2) <= this.gap) {
            this.Next();
        }
    };
    IfNode.prototype.Next = function () { };
    IfNode.nodeType = 'code/Merge';
    return IfNode;
}(Node));
light.NodeContext.registerNode(IfNode$1);

var LotteryNode$1 = /** @class */ (function (_super) {
    __extends(LotteryNode, _super);
    function LotteryNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.from = 0;
        _this.to = 1;
        _this.duration = 1000000; // 编辑器里以微秒处理
        _this.progress = -1;
        return _this;
    }
    LotteryNode.prototype.Run = function () {
        var _this = this;
        var entity = this.entityManager.getEntityById(this.entityId);
        if (!entity)
            return;
        var face = entity.getComponent(light.CrazyFace);
        if (!face)
            return;
        if (this.progress !== -1) {
            face.progress = this.progress;
            showEntities([this.entityId], this.entityManager);
            this.Next();
        }
        else {
            var timeStart_1 = light.getCurrentTime();
            var invoked_1 = false;
            var resetInitialValue_1 = false;
            light.on('update', function (time) {
                var currentTime = light.getCurrentTime();
                if (timeStart_1 === -1 || invoked_1) {
                    return;
                }
                if (!resetInitialValue_1) {
                    resetInitialValue_1 = true;
                    face.progress = _this.from;
                    showEntities([_this.entityId], _this.entityManager);
                }
                if (time - timeStart_1 <= _this.duration) {
                    face.progress = _this.linear((currentTime - timeStart_1) / 1000, _this.from, _this.to - _this.from, _this.duration / 1000);
                }
                else {
                    _this.Next();
                    invoked_1 = true;
                    timeStart_1 = -1;
                }
            });
        }
    };
    LotteryNode.prototype.linear = function (currentTime, startValue, changeValue, duration) {
        return changeValue * currentTime / duration + startValue;
    };
    LotteryNode.prototype.Next = function () { };
    LotteryNode.nodeType = 'code/Fusion';
    return LotteryNode;
}(Node));
light.NodeContext.registerNode(LotteryNode$1);

var Status;
(function (Status) {
    Status[Status["Detected"] = 1] = "Detected";
    Status[Status["Lost"] = 2] = "Lost";
    Status[Status["UNINITIALIZED"] = 3] = "UNINITIALIZED";
})(Status || (Status = {}));
var PointNode = /** @class */ (function (_super) {
    __extends(PointNode, _super);
    function PointNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.pitchMin = 0;
        _this.pitchMax = 1;
        _this.yawMin = 0;
        _this.yawMax = 1;
        _this.rollMin = 0;
        _this.rollMax = 1;
        _this.x1 = 0;
        _this.x2 = 1;
        _this.y1 = 0;
        _this.y2 = 1;
        _this.ratio = 1.2; // 丢失的比例
        _this.status = {};
        _this.init = false;
        _this.minWidth = 0.8;
        _this.minHeight = 0.8;
        return _this;
    }
    PointNode.prototype.Run = function () {
        if (this.init) {
            for (var id in this.status) {
                this.status[id] = Status.UNINITIALIZED;
            }
            return;
        }
        this.init = true;
        openAIFeature([
            "Face_Point" /* FACE_POINT */,
        ], this.entityManager, this.eventManager);
        light.on('update', this.update.bind(this));
    };
    PointNode.prototype.getMinValue = function (value) {
        if (value > 0) {
            return value / this.ratio;
        }
        return value * this.ratio;
    };
    PointNode.prototype.getMaxValue = function (value) {
        if (value > 0) {
            return value * this.ratio;
        }
        return value / this.ratio;
    };
    PointNode.prototype.update = function () {
        var _this = this;
        var datas = getAIPointData("Face_Point" /* FACE_POINT */, this.entityManager);
        var _loop_1 = function (i) {
            var data = datas[i];
            var id_ = data.id_, roll_ = data.roll_, pitch_ = data.pitch_, canvas_width_ = data.canvas_width_, canvas_height_ = data.canvas_height_, yaw_ = data.yaw_, point_array_ = data.point_array_;
            var minX;
            var maxX;
            var minY;
            var maxY;
            if (point_array_ === null || point_array_ === void 0 ? void 0 : point_array_.length) {
                var xs = [];
                var ys = [];
                for (var i_1 = 0; i_1 < point_array_.length; i_1++) {
                    if (i_1 % 2 === 0) {
                        xs.push(point_array_[i_1]);
                    }
                    else {
                        ys.push((point_array_[i_1]));
                    }
                }
                minX = Math.min.apply(Math, xs);
                maxX = Math.max.apply(Math, xs);
                minY = Math.min.apply(Math, ys);
                maxY = Math.max.apply(Math, ys);
            }
            if (this_1.status[id_] === undefined) {
                this_1.status[id_] = Status.UNINITIALIZED;
            }
            var isDetect = function () { return _this.isBetween(roll_, _this.rollMin, _this.rollMax)
                && _this.isBetween(yaw_, _this.yawMin, _this.yawMax)
                && _this.isBetween(pitch_, _this.pitchMin, _this.pitchMax)
                && _this.isBetween(minX, _this.x1 * canvas_width_, _this.x2 * canvas_width_)
                && _this.isBetween(maxX, _this.x1 * canvas_width_, _this.x2 * canvas_width_)
                && _this.isBetween(minY, _this.y1 * canvas_height_, _this.y2 * canvas_height_)
                && _this.isBetween(maxY, _this.y1 * canvas_height_, _this.y2 * canvas_height_)
                && isVaildFace(); };
            var isLost = function () { return !_this.isBetween(roll_, _this.getMinValue(_this.rollMin), _this.getMaxValue(_this.rollMax))
                || !_this.isBetween(yaw_, _this.getMinValue(_this.yawMin), _this.getMaxValue(_this.yawMax))
                || !_this.isBetween(pitch_, _this.getMinValue(_this.pitchMin), _this.getMaxValue(_this.pitchMax))
                || !_this.isBetween(minX, _this.getMinValue(_this.x1 * canvas_width_), _this.getMaxValue(_this.x2 * canvas_width_))
                || !_this.isBetween(maxX, _this.getMinValue(_this.x1 * canvas_width_), _this.getMaxValue(_this.x2 * canvas_width_))
                || !_this.isBetween(minY, _this.getMinValue(_this.y1 * canvas_height_), _this.getMaxValue(_this.y2 * canvas_height_))
                || !_this.isBetween(maxY, _this.getMinValue(_this.y1 * canvas_height_), _this.getMaxValue(_this.y2 * canvas_height_))
                || !isVaildFace(); };
            var isVaildFace = function () { return (maxX - minX) > (_this.minWidth * canvas_width_)
                && (maxY - minY) > (_this.minHeight * canvas_height_); };
            if (this_1.status[id_] === Status.UNINITIALIZED) {
                if (isDetect()) {
                    this_1.faceId = id_;
                    this_1.faceIndex = i;
                    this_1.status[id_] = Status.Detected;
                    this_1.Detected();
                }
                else if (isLost()) {
                    this_1.faceId = id_;
                    this_1.faceIndex = i;
                    this_1.status[id_] = Status.Lost;
                    this_1.Lost();
                }
            }
            else if (this_1.status[id_] === Status.Lost) { // 初始化或丢失之后处理
                if (isDetect()) {
                    this_1.faceId = id_;
                    this_1.faceIndex = i;
                    this_1.status[id_] = Status.Detected;
                    this_1.Detected();
                }
            }
            else if (this_1.status[id_] === Status.Detected) { // 检测到时处理
                if (isLost()) {
                    this_1.faceId = id_;
                    this_1.faceIndex = i;
                    this_1.status[id_] = Status.Lost;
                    this_1.Lost();
                }
            }
        };
        var this_1 = this;
        for (var i = 0; i < datas.length; i++) {
            _loop_1(i);
        }
    };
    PointNode.prototype.isBetween = function (v, min, max) {
        if (min <= v && v <= max) {
            return true;
        }
        return false;
    };
    PointNode.prototype.Detected = function () { };
    PointNode.prototype.Lost = function () { };
    PointNode.nodeType = 'code/Point';
    return PointNode;
}(Node));
light.NodeContext.registerNode(PointNode);

var PostEffectNode = /** @class */ (function (_super) {
    __extends(PostEffectNode, _super);
    function PostEffectNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.strengthInput = 0;
        _this.starMaxScaleInput = 0;
        _this.starMinScaleInput = 0;
        _this.starScalePeriodInput = 0;
        _this.maxCornersInput = 0;
        _this.minDistanceInput = 0;
        _this.brightnessInput = 0;
        _this.contrastInput = 0;
        _this.hueInput = 0;
        _this.highlightsInput = 0;
        _this.sharpnessInput = 0;
        _this.shadowsInput = 0;
        _this.saturationInput = 0;
        _this.temperatureInput = 0;
        return _this;
    }
    PostEffectNode.prototype.Run = function () {
        showEntities([this.entityId], this.entityManager);
        var entity = this.entityManager.getEntityById(this.entityId);
        var postEffect = entity.getComponent(light.PostEffect);
        if (postEffect) {
            if (postEffect.getEffectJson()) {
                var json = JSON.parse(postEffect.getEffectJson());
                json.effectStrength = this.strengthInput > 0 ? this.strengthInput : json.effectStrength;
                json.starMaxScale = this.starMaxScaleInput > 0 ? this.starMaxScaleInput : json.starMaxScale;
                json.starMinScale = this.starMinScaleInput > 0 ? this.starMinScaleInput : json.starMinScale;
                json.starScalePeriod = this.starScalePeriodInput > 0 ? this.starScalePeriodInput : json.starScalePeriod;
                json.minDistance = this.minDistanceInput > 0 ? this.minDistanceInput : json.minDistance;
                json.maxCorners = this.maxCornersInput > 0 ? this.maxCornersInput : json.maxCorners;
                json.brightness = this.brightnessInput > 0 ? this.brightnessInput : json.brightness;
                json.contrast = this.contrastInput > 0 ? this.contrastInput : json.contrast;
                json.hue = this.hueInput > 0 ? this.hueInput : json.hue;
                json.highlights = this.highlightsInput > 0 ? this.highlightsInput : json.highlights;
                json.sharpness = this.sharpnessInput > 0 ? this.sharpnessInput : json.sharpness;
                json.shadows = this.shadowsInput > 0 ? this.shadowsInput : json.shadows;
                json.saturation = this.saturationInput > 0 ? this.saturationInput : json.saturation;
                json.temperature = this.temperatureInput > 0 ? this.temperatureInput : json.temperature;
                postEffect.setEffectJson(JSON.stringify(json));
            }
        }
        this.Next();
    };
    PostEffectNode.prototype.Next = function () { };
    PostEffectNode.nodeType = 'code/PostEffect';
    return PostEffectNode;
}(Node));
light.NodeContext.registerNode(PostEffectNode);

var ParticleNode = /** @class */ (function (_super) {
    __extends(ParticleNode, _super);
    function ParticleNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ParticleNode.prototype.Run = function () {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k, _l, _m, _o, _p, _q, _r, _s, _t, _u, _v, _w, _x, _y, _z;
        if (!this.entityId)
            return;
        var entity = this.entityManager.getEntityById(this.entityId);
        showEntities([this.entityId], this.entityManager);
        if (entity) {
            var particle = entity.getComponent(light.ParticleEffect);
            if (particle) {
                var vec3 = new light.Vec3((_c = (_b = (_a = this.xInput) !== null && _a !== void 0 ? _a : this.x) !== null && _b !== void 0 ? _b : particle.emitterLength.x) !== null && _c !== void 0 ? _c : 0, (_f = (_e = (_d = this.yInput) !== null && _d !== void 0 ? _d : this.y) !== null && _e !== void 0 ? _e : particle.emitterLength.y) !== null && _f !== void 0 ? _f : 0, (_j = (_h = (_g = this.zInput) !== null && _g !== void 0 ? _g : this.z) !== null && _h !== void 0 ? _h : particle.emitterLength.z) !== null && _j !== void 0 ? _j : 0);
                var dvec3 = new light.Vec3((_m = (_l = (_k = this.directionXInput) !== null && _k !== void 0 ? _k : this.directionX) !== null && _l !== void 0 ? _l : particle.emissionVector.x) !== null && _m !== void 0 ? _m : 0, (_q = (_p = (_o = this.directionYInput) !== null && _o !== void 0 ? _o : this.directionY) !== null && _p !== void 0 ? _p : particle.emissionVector.y) !== null && _q !== void 0 ? _q : 0, (_t = (_s = (_r = this.directionZInput) !== null && _r !== void 0 ? _r : this.directionZ) !== null && _s !== void 0 ? _s : particle.emissionVector.z) !== null && _t !== void 0 ? _t : 0);
                particle.emitterLength = vec3;
                particle.emissionVector = dvec3;
                particle.speed = (_w = (_v = (_u = this.speedInput) !== null && _u !== void 0 ? _u : this.speed) !== null && _v !== void 0 ? _v : particle.speed) !== null && _w !== void 0 ? _w : 0.1;
                particle.maxParticles = (_z = (_y = (_x = this.maxInput) !== null && _x !== void 0 ? _x : this.max) !== null && _y !== void 0 ? _y : particle.maxParticles) !== null && _z !== void 0 ? _z : 0;
                particle.speedRandom = false;
                particle.emissionVectorRandom = false;
                particle.updateComponentData = true;
            }
        }
        this.Next();
    };
    ParticleNode.prototype.Next = function () { };
    ParticleNode.nodeType = 'code/Particle';
    return ParticleNode;
}(Node));
light.NodeContext.registerNode(ParticleNode);

var ThrottleNode = /** @class */ (function (_super) {
    __extends(ThrottleNode, _super);
    function ThrottleNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.value = 1000000;
        _this.triggerTime = -1; // 上一次触发时间
        return _this;
    }
    ThrottleNode.prototype.Next = function () { };
    ThrottleNode.prototype.Run = function () {
        if (this.triggerTime < 0 // 首次触发及间隔超过
            || (this.triggerTime >= 0 && (this.triggerTime + this.value < light.getCurrentTime()))) {
            this.triggerTime = light.getCurrentTime();
            this.Next();
        }
    };
    ThrottleNode.nodeType = 'code/throttle';
    return ThrottleNode;
}(Node));
light.NodeContext.registerNode(ThrottleNode);

var UserMaterialNode = /** @class */ (function (_super) {
    __extends(UserMaterialNode, _super);
    function UserMaterialNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.lastpaths = [];
        return _this;
    }
    UserMaterialNode.prototype.Run = function () {
        var _this = this;
        light.on('update', function () {
            var entity = _this.entityManager.getEntityById(_this.entityId);
            if (entity) {
                var cmp = entity.getComponent(light.UserMaterial);
                if (cmp) {
                    var clipData = cmp.getClipDataPaths();
                    if ((clipData === null || clipData === void 0 ? void 0 : clipData.size) && clipData.size()) {
                        var paths = [];
                        for (var i = 0; i < clipData.size(); i++) {
                            paths.push(clipData.get(i));
                        }
                        if (paths.length !== _this.lastpaths.length || !paths.every(function (path, i) { return path === _this.lastpaths[i]; })) {
                            if (_this.lastpaths.length === 0) {
                                _this.Next();
                            }
                            else {
                                _this.Change();
                            }
                            _this.lastpaths = paths;
                        }
                    }
                }
            }
        });
    };
    UserMaterialNode.prototype.Next = function () { };
    UserMaterialNode.prototype.Change = function () { };
    UserMaterialNode.nodeType = 'code/userMaterial';
    return UserMaterialNode;
}(EventNode));
light.NodeContext.registerNode(UserMaterialNode);

var TouchEventNode = /** @class */ (function (_super) {
    __extends(TouchEventNode, _super);
    function TouchEventNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.init = false;
        return _this;
    }
    TouchEventNode.prototype.Run = function () {
        var _this = this;
        if (!this.init) {
            this.init = true;
            light.on('TouchEvent', function () {
                _this.Next();
            });
        }
    };
    TouchEventNode.prototype.Next = function () { };
    TouchEventNode.nodeType = 'code/touchEvent';
    return TouchEventNode;
}(EventNode));
light.NodeContext.registerNode(TouchEventNode);

var Status$1;
(function (Status) {
    Status[Status["Detected"] = 1] = "Detected";
    Status[Status["Lost"] = 2] = "Lost";
    Status[Status["UNINITIALIZED"] = 3] = "UNINITIALIZED";
})(Status$1 || (Status$1 = {}));
var PointNode$1 = /** @class */ (function (_super) {
    __extends(PointNode, _super);
    function PointNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.detected = false;
        return _this;
    }
    PointNode.prototype.transform = function (value, max) {
        var rangeMin = -1;
        var rangeMax = 1;
        var range = rangeMax - rangeMin;
        return ((range * value / max) + rangeMin);
    };
    PointNode.prototype.Run = function () {
        var _this = this;
        openAIFeature(["Hand_Gesture" /* HAND_GESTURE */], this.entityManager, this.eventManager);
        light.on('update', function (_time) {
            var handPointdatas = getAIPointData("Hand_Point" /* HAND_POINT */, _this.entityManager);
            var firstHand = handPointdatas === null || handPointdatas === void 0 ? void 0 : handPointdatas[0];
            if (firstHand) {
                _this.detected = true;
                var canvas_width_ = firstHand.canvas_width_;
                var canvas_height_ = firstHand.canvas_height_;
                var points = firstHand.point_array_;
                var xs = points.filter(function (point, index) { return (index % 2 === 0); });
                var ys = points.filter(function (point, index) { return (index % 2 === 1); });
                var minX = Math.min.apply(Math, xs);
                var maxX = Math.max.apply(Math, xs);
                var minY = Math.min.apply(Math, ys);
                var maxY = Math.max.apply(Math, ys);
                _this.horizonX1 = _this.transform(minX, canvas_width_);
                _this.horizonX2 = _this.transform(maxX, canvas_width_);
                _this.horizonMiddle = _this.transform((minX + maxX) / 2, canvas_width_);
                _this.verticalY1 = _this.transform(minY, canvas_height_);
                _this.verticalY2 = _this.transform(maxY, canvas_height_);
                _this.verticalMiddle = _this.transform((minY + maxY) / 2, canvas_height_);
                _this.screenWidth = canvas_width_;
                _this.screenHeight = canvas_height_;
                _this.Detect();
            }
            else {
                _this.detected = false;
                _this.Lost();
            }
        });
    };
    PointNode.prototype.Detect = function () { };
    PointNode.prototype.Lost = function () { };
    PointNode.nodeType = 'code/handpoint';
    return PointNode;
}(Node));
light.NodeContext.registerNode(PointNode$1);

var OpenMouthRatioNode = /** @class */ (function (_super) {
    __extends(OpenMouthRatioNode, _super);
    function OpenMouthRatioNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.Value = 1.0;
        _this.init = false;
        _this.ratio = 1.0;
        _this.nose_x = -1;
        _this.nose_y = -1;
        _this.mouse_up_x = -1;
        _this.mouse_up_y = -1;
        _this.mouse_down_x = -1;
        _this.mouse_down_y = -1;
        _this.originDis = -1;
        _this.currentDis = -1;
        return _this;
    }
    OpenMouthRatioNode.prototype.Run = function () {
        if (!this.init) {
            this.init = true;
            light.on('update', this.update.bind(this));
        }
    };
    OpenMouthRatioNode.prototype.update = function () {
        var datas = getAIPointData("Face_Point" /* FACE_POINT */, this.entityManager);
        if (datas.length > 0) {
            var person = datas[0];
            var id_ = person.id_, roll_ = person.roll_, pitch_ = person.pitch_, canvas_width_ = person.canvas_width_, canvas_height_ = person.canvas_height_, yaw_ = person.yaw_, point_array_ = person.point_array_;
            this.nose_x = person.point_array_[128] / person.canvas_width_ * 2 - 1;
            this.nose_y = -(person.point_array_[129] / person.canvas_height_ * 2 - 1);
            this.mouse_up_x = person.point_array_[170] / person.canvas_width_ * 2 - 1;
            this.mouse_up_y = -(person.point_array_[171] / person.canvas_height_ * 2 - 1);
            this.mouse_down_x = person.point_array_[138] / person.canvas_width_ * 2 - 1;
            this.mouse_down_y = -(person.point_array_[139] / person.canvas_height_ * 2 - 1);
            this.currentDis = this.calcDis(this.mouse_up_x, this.mouse_up_y, this.mouse_down_x, this.mouse_down_y);
            if (this.originDis === -1) {
                this.originDis = this.currentDis;
            }
            this.ratio = this.currentDis / this.originDis;
            this.ratio = Math.max(this.ratio, 1) * this.inputRatio;
        }
        else {
            this.ratio = 1.0;
        }
        this.Value = this.ratio;
        this.Next();
    };
    OpenMouthRatioNode.prototype.calcDis = function (sx, sy, fx, fy) {
        return Math.sqrt((sx - fx) * (sx - fx) + (sy - fy) * (sy - fy));
    };
    OpenMouthRatioNode.prototype.Next = function () {
    };
    OpenMouthRatioNode.nodeType = 'code/OpenMouthRatio';
    return OpenMouthRatioNode;
}(Node));
light.NodeContext.registerNode(OpenMouthRatioNode);

var StateEventNode = /** @class */ (function (_super) {
    __extends(StateEventNode, _super);
    function StateEventNode() {
        var _this = _super.call(this) || this;
        _this.total = 3;
        _this.repeat = 1;
        _this.currentIndex = 0;
        return _this;
    }
    StateEventNode.prototype.Run1 = function () {
        this.triggerEvent(0);
    };
    StateEventNode.prototype.Run2 = function () {
        this.triggerEvent(1);
    };
    StateEventNode.prototype.Run3 = function () {
        this.triggerEvent(2);
    };
    StateEventNode.prototype.Run4 = function () {
        this.triggerEvent(3);
    };
    StateEventNode.prototype.Run5 = function () {
        this.triggerEvent(4);
    };
    StateEventNode.prototype.Run6 = function () {
        this.triggerEvent(5);
    };
    StateEventNode.prototype.Run7 = function () {
        this.triggerEvent(6);
    };
    StateEventNode.prototype.Run8 = function () {
        this.triggerEvent(7);
    };
    StateEventNode.prototype.Run9 = function () {
        this.triggerEvent(8);
    };
    StateEventNode.prototype.Run10 = function () {
        this.triggerEvent(9);
    };
    StateEventNode.prototype.triggerEvent = function (index) {
        var currentIndex = 0;
        if (this.repeat < 0) {
            currentIndex = this.currentIndex % this.total;
        }
        else if (this.repeat > 0) {
            if (this.currentIndex < this.total * this.repeat) {
                currentIndex = this.currentIndex % this.total;
            }
            else {
                currentIndex = this.currentIndex;
            }
        }
        else {
            currentIndex = -1;
        }
        if (currentIndex === index) {
            if (this["Next" + (currentIndex + 1)]) {
                this["Next" + (currentIndex + 1)]();
            }
            this.currentIndex = this.currentIndex + 1;
        }
    };
    StateEventNode.nodeType = 'code/stateEvent';
    return StateEventNode;
}(Node));
light.NodeContext.registerNode(StateEventNode);

var ParallelNode = /** @class */ (function (_super) {
    __extends(ParallelNode, _super);
    function ParallelNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ParallelNode.prototype.Event1 = function () {
        this.Next();
    };
    ParallelNode.prototype.Event2 = function () {
        this.Next();
    };
    ParallelNode.prototype.Event3 = function () {
        this.Next();
    };
    ParallelNode.prototype.Event4 = function () {
        this.Next();
    };
    ParallelNode.prototype.Event5 = function () {
        this.Next();
    };
    ParallelNode.prototype.Next = function () { };
    ParallelNode.nodeType = 'code/Parallel';
    return ParallelNode;
}(Node));
light.NodeContext.registerNode(ParallelNode);

var Interaction;
(function (Interaction) {
    Interaction["CLICK"] = "\u70B9\u51FB";
    Interaction["DCLICK"] = "\u53CC\u51FB";
    Interaction["LCLICK"] = "\u957F\u6309";
    Interaction["HAND"] = "\u624B\u51FA\u73B0";
    Interaction["FINGER"] = "\u98DF\u6307\u6307\u5C16\u51FA\u73B0";
    Interaction["FACE"] = "\u5168\u8138\u51FA\u73B0";
    Interaction["NOSE"] = "\u9F3B\u5B50\u51FA\u73B0";
    Interaction["MOUSE"] = "\u5634\u5DF4\u51FA\u73B0";
    Interaction["LEFTEYE"] = "\u5DE6\u773C\u51FA\u73B0";
    Interaction["RIGHTEYE"] = "\u53F3\u773C\u51FA\u73B0";
})(Interaction || (Interaction = {}));
var LongClickTime = 500; // 长按阈值时间
var DoubleClickTime = 300; // 双击阈值时间
var HEIGHT = 1280; // 画布标准高
var sampleNumber = 20; // 采样数量
var ScreenEventNode = /** @class */ (function (_super) {
    __extends(ScreenEventNode, _super);
    function ScreenEventNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.isDetected = false;
        _this.count = 0;
        _this.times = 0;
        _this.longPressedState = {
            longPressedDownTime: undefined,
            longPressedTriggered: false,
        };
        _this.lastTwoTimePressedTime = []; // 记录最近两次按下的时间 用来判读是否为双击
        // AI点位
        _this.facePoint = Array.from({ length: 90 }, function (v, k) { return k; });
        _this.nosePoint = [56, 57, 58, 59, 60, 61, 62, 64];
        _this.mousePoint = [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82];
        _this.leftEyePoint = [35, 36, 37, 38, 39, 40, 41, 42, 43, 44];
        _this.rightEyePoint = [45, 46, 47, 48, 49, 50, 51, 52, 53, 54];
        _this.handPoint = Array.from({ length: 32 }, function (v, k) { return k; });
        _this.onePoint = [15, 16, 17];
        return _this;
    }
    ScreenEventNode.prototype.onDetected = function () { };
    ScreenEventNode.prototype.Lost = function () { };
    ScreenEventNode.prototype.Run = function () {
        var _this = this;
        switch (this.interactionType) {
            case Interaction.CLICK:
            case Interaction.DCLICK:
            case Interaction.LCLICK:
                light.on('TouchEvent', function (event) {
                    _this.onReceive(event);
                });
                break;
            case Interaction.FACE:
            case Interaction.MOUSE:
            case Interaction.NOSE:
            case Interaction.LEFTEYE:
            case Interaction.RIGHTEYE:
                openAIFeature(["Expression" /* EXPRESSION */], // , light.AIClassFeature.HAND_GESTURE],
                this.entityManager, this.eventManager);
                break;
            case Interaction.HAND:
            case Interaction.FINGER:
                openAIFeature(["Hand_Gesture" /* HAND_GESTURE */, "Hand_Point" /* HAND_POINT */], this.entityManager, this.eventManager);
                break;
        }
        light.on('RenderWillStartEvent', function (event) {
            _this.onReceive(event);
        });
        light.on('update', this.onUpdate.bind(this));
    };
    ScreenEventNode.prototype.onUpdate = function (currentTime) {
        // 在onUpdate中 处理长按事件
        if (this.interactionType === Interaction.LCLICK) {
            this.processLongClickEvent(currentTime);
            return;
        }
        var aiData = [];
        switch (this.interactionType) {
            case Interaction.HAND:
            case Interaction.FINGER:
                aiData = getAIPointData("Hand_Point" /* HAND_POINT */, this.entityManager);
                break;
            case Interaction.FACE:
            case Interaction.MOUSE:
            case Interaction.NOSE:
            case Interaction.LEFTEYE:
            case Interaction.RIGHTEYE:
                aiData = getAIPointData("Face_Point" /* FACE_POINT */, this.entityManager);
                break;
        }
        var rects = [];
        if (!aiData && aiData.length === 0) {
            this.onLost();
            return;
        }
        for (var i = 0; i < aiData.length; i++) {
            var data = aiData[i];
            var AIDataRect = this.getAIDataRect(this.interactionType, data);
            rects.push(AIDataRect);
        }
        if (rects.length > 0) {
            var triggered = this.isInConstrainRects(rects, this.entityRect);
            this.onTriggered(triggered);
        }
    };
    ScreenEventNode.prototype.getAIDataRect = function (actionEventName, data) {
        var id_ = data.id_, canvas_width_ = data.canvas_width_, canvas_height_ = data.canvas_height_, point_array_ = data.point_array_;
        var minX;
        var maxX;
        var minY;
        var maxY;
        if (point_array_ === null || point_array_ === void 0 ? void 0 : point_array_.length) {
            var xs = [];
            var ys = [];
            var points = [];
            switch (actionEventName) {
                case Interaction.FACE:
                    points = this.facePoint;
                    break;
                case Interaction.MOUSE:
                    points = this.mousePoint;
                    break;
                case Interaction.NOSE:
                    points = this.nosePoint;
                    break;
                case Interaction.LEFTEYE:
                    points = this.leftEyePoint;
                    break;
                case Interaction.RIGHTEYE:
                    points = this.rightEyePoint;
                    break;
                case Interaction.HAND:
                    points = this.handPoint;
                    break;
                case Interaction.FINGER:
                    points = this.onePoint;
                    break;
            }
            var beganIndex = points[0] * 2;
            var length = (points[0] + points.length) * 2;
            for (var i = beganIndex; i < length; i++) {
                if (i % 2 === 0) {
                    xs.push(point_array_[i]);
                }
                else {
                    ys.push((point_array_[i]));
                }
            }
            minX = Math.min.apply(Math, xs) / canvas_width_;
            maxX = Math.max.apply(Math, xs) / canvas_width_;
            minY = Math.min.apply(Math, ys) / canvas_height_;
            maxY = Math.max.apply(Math, ys) / canvas_height_;
            // 对于一个点 看做面积为1的矩形
            if (minX === maxX) {
                maxX = minX + 1;
            }
            if (minY === maxY) {
                maxY = minY + 1;
            }
        }
        return { left: minX, top: minY, right: maxX, bottom: maxY };
    };
    ScreenEventNode.prototype.onReceive = function (event) {
        switch (event.type()) {
            case 'RenderWillStartEvent':
                this.entityRect = this.getEntityRect(this.entity);
                break;
            case 'TouchEvent': {
                var clickState = this.getClickEventType(event);
                this.touchEventInfo = {
                    point: {
                        x: event.getX(),
                        y: event.getY(),
                    },
                    downTime: event.getDownTime(),
                    eventTime: event.getEventTime(),
                };
                this.onTouchEventProcess(clickState, this.touchEventInfo);
                break;
            }
        }
    };
    ScreenEventNode.prototype.onTriggered = function (isTriggered, time) {
        if (isTriggered) {
            if (!this.isDetected) {
                this.times = 0;
            }
            this.times = time || 0;
            this.isDetected = true;
            this.count += 1;
            this.onDetected();
        }
        else {
            this.onLost();
        }
    };
    ScreenEventNode.prototype.onLost = function () {
        if (this.isDetected) {
            this.Lost();
        }
        this.isDetected = false;
    };
    ScreenEventNode.prototype.onTouchEventProcess = function (clickState, event) {
        if ((clickState === null || clickState === void 0 ? void 0 : clickState.clickType) && clickState.clickType === this.interactionType
            && clickState.triggered === true && event) {
            var ratio = this.getRatio();
            if (this.isInConstrainRotationRect({ x: event.point.x / ratio / HEIGHT, y: event.point.y / HEIGHT }, this.entityRect)) {
                this.onTriggered(true, event.eventTime - event.downTime);
            }
            else {
                this.onLost();
            }
        }
        else {
            this.times = 0;
            this.onLost();
        }
    };
    ScreenEventNode.prototype.getClickEventType = function (event) {
        var clickState = {
            clickType: undefined,
            triggered: false,
        };
        if (event && event.type() === 'TouchEvent') {
            var downTime = event.getDownTime();
            var eventTime = event.getEventTime();
            if (event.getAction() === 0) { // press
                this.currentPressedTime = downTime;
                this.pushPressedTimeStack(this.currentPressedTime);
                if (this.lastTwoTimePressedTime.length === 2
                    && downTime - this.lastTwoTimePressedTime[0] < DoubleClickTime) {
                    clickState.clickType = Interaction.DCLICK;
                }
                else {
                    clickState.clickType = Interaction.CLICK;
                }
                clickState.triggered = true;
            }
            else if (event.getAction() === 1) { // release
                if (this.currentPressedTime && eventTime - downTime > LongClickTime) {
                    this.times = eventTime - downTime;
                }
                this.currentPressedTime = undefined;
                this.longPressedState = {
                    longPressedDownTime: undefined,
                    longPressedTriggered: false,
                };
                clickState.triggered = false;
            }
        }
        return clickState;
    };
    // 根据 update 的 currentTime 判断是否为长按
    ScreenEventNode.prototype.processLongClickEvent = function (currentTime) {
        if (this.interactionType === Interaction.LCLICK && this.currentPressedTime
            && !this.longPressedState.longPressedTriggered) {
            if (!this.longPressedState.longPressedDownTime) {
                this.longPressedState.longPressedDownTime = currentTime;
            }
            if ((currentTime - this.longPressedState.longPressedDownTime) / 1000 >= LongClickTime) {
                this.longPressedState.longPressedTriggered = true;
                var clickState = {
                    clickType: Interaction.LCLICK,
                    triggered: true,
                };
                this.onTouchEventProcess(clickState, this.touchEventInfo);
            }
        }
    };
    ScreenEventNode.prototype.pushPressedTimeStack = function (time) {
        if (this.lastTwoTimePressedTime.length === 2) {
            this.lastTwoTimePressedTime.splice(0, 1);
        }
        this.lastTwoTimePressedTime.push(time);
    };
    ScreenEventNode.prototype.getRatio = function () {
        var devWidth = light.DeviceUtils.GetSurfaceWidth(this.entityManager);
        var devHeight = light.DeviceUtils.GetSurfaceHeight(this.entityManager);
        return devWidth / devHeight;
    };
    ScreenEventNode.prototype.getEntityRect = function (entityId) {
        var entity = this.entityManager.getEntityById(entityId);
        var transform = entity.getComponent(light.ScreenTransform);
        if (isEntityHide(entityId, this.entityManager)) {
            return undefined;
        }
        var transformArray = this.getParentTransforms(transform);
        transformArray.splice(0, 1); // 删除自己
        var pSize;
        if (transformArray.length > 0) {
            pSize = this.getParentSize(transform, transformArray);
        }
        if (!pSize) {
            return undefined;
        }
        var size = this.getSizeFromTransform(transform, pSize);
        var position = this.getPositionFromTransform(transform, pSize);
        var angle = this.get2DRotationFromTransform(transform);
        var local = { x: position.x + pSize.width / 2, y: -position.y + pSize.height / 2 };
        var originPoint = { x: local.x - size.width / 2, y: local.y - size.height / 2 };
        var transformOrigin = {
            x: ((transform.pivot.x * size.width) / 2 + local.x) / pSize.width,
            y: ((-transform.pivot.y * size.height) / 2 + local.y) / pSize.height,
        };
        var rect = {
            left: originPoint.x / pSize.width,
            top: originPoint.y / pSize.height,
            right: (originPoint.x + size.width) / pSize.width,
            bottom: (originPoint.y + size.height) / pSize.height,
        };
        return { rect: rect, transformOrigin: transformOrigin, angle: angle };
    };
    ScreenEventNode.prototype.getParentSize = function (transform, transformArray) {
        var ratio = this.getRatio();
        var parentSize = { width: ratio * HEIGHT, height: HEIGHT };
        for (var i = transformArray.length - 1; i >= 0; i--) {
            parentSize = this.getSizeFromTransform(transformArray[i], parentSize);
        }
        return parentSize;
    };
    ScreenEventNode.prototype.getParentTransforms = function (transform) {
        var result = [transform];
        if (!this.entityManager.valid(transform.parent)) {
            return [transform];
        }
        var parent = this.entityManager.get(transform.parent);
        var parentTransform = parent.getComponent(light.ScreenTransform);
        if (parentTransform != null) {
            result = result.concat(this.getParentTransforms(parentTransform));
        }
        else {
            return [transform];
        }
        return result;
    };
    ScreenEventNode.prototype.getSizeFromTransform = function (transform, parentSize) {
        var width = (transform.anchor.right - transform.anchor.left) / 2 * parentSize.width
            + (transform.offset.right - transform.offset.left);
        var height = (transform.anchor.top - transform.anchor.bottom) / 2 * parentSize.height
            + (transform.offset.top - transform.offset.bottom);
        return { width: width, height: height };
    };
    ScreenEventNode.prototype.getPositionFromTransform = function (transform, pSize) {
        var left = transform.anchor.left * pSize.width / 2 + transform.offset.left;
        var right = transform.anchor.right * pSize.width / 2 + transform.offset.right;
        var top = transform.anchor.top * pSize.height / 2 + transform.offset.top;
        var bottom = transform.anchor.bottom * pSize.height / 2 + transform.offset.bottom;
        var position = { x: (right + left) / 2, y: (top + bottom) / 2 };
        position.x += (right - left) / 2 * transform.pivot.x;
        position.y += (top - bottom) / 2 * transform.pivot.y;
        return position;
    };
    ScreenEventNode.prototype.get2DRotationFromTransform = function (transform) {
        var _a = transform.rotation, x = _a.x, y = _a.y, z = _a.z, w = _a.w;
        var euler = new Euler();
        var quat = new Quaternion(x, y, z, w);
        euler.setFromQuaternion(quat);
        return euler.z;
    };
    ScreenEventNode.prototype.isInConstrainRect = function (point, rect) {
        if (!rect) {
            return false;
        }
        var x = point.x;
        var y = point.y;
        return x > rect.left && x < rect.right
            && y > rect.top && y < rect.bottom;
    };
    // 点击触发判定
    ScreenEventNode.prototype.isInConstrainRotationRect = function (point, rRect) {
        if (!rRect) {
            return false;
        }
        var transformOrigin = rRect.transformOrigin, angle = rRect.angle, rect = rRect.rect;
        var revertPoint = this.rotateByPoint(point, transformOrigin, angle);
        var x = revertPoint.x;
        var y = revertPoint.y;
        return x > rect.left && x < rect.right
            && y > rect.top && y < rect.bottom;
    };
    // AI触发判定
    ScreenEventNode.prototype.isInConstrainRects = function (originRect, rRect) {
        var _this = this;
        if (!rRect) {
            return false;
        }
        var transformOrigin = rRect.transformOrigin, angle = rRect.angle, rect = rRect.rect;
        // 性能提升(1):旋转角度小于10度 按照没有旋转处理
        var isSmallAngle = Math.abs(angle) < Math.PI / 18;
        return originRect.some(function (oRect) {
            var overlapRatio = 0;
            if (isSmallAngle) {
                if (_this.isRectIntersecting(oRect, rect)) {
                    overlapRatio = _this.getIntersectingAreaMaxPercent(oRect, rect);
                }
            }
            else {
                overlapRatio = _this.getRectArea(oRect) < _this.getRectArea(rect)
                    ? _this.getSampleOfRect(oRect, rect, transformOrigin, angle)
                    : _this.getSampleOfRect(rect, oRect, transformOrigin, -angle);
            }
            if (overlapRatio >= 0.2) {
                return true;
            }
        });
    };
    // 性能提升(2):使用采样算法模拟碰撞面积 准确度降低 效率提高
    ScreenEventNode.prototype.getSampleOfRect = function (smallerRect, biggerRect, transformOrigin, angle) {
        var _this = this;
        var rectPoint = this.calculateAreaBySampleAlgorithm(smallerRect);
        var rotateRectPoint = rectPoint.map(function (point) { return _this.rotateByPoint(point, transformOrigin, angle); });
        var number = 0;
        rotateRectPoint.forEach(function (point) {
            if (_this.isInConstrainRect(point, biggerRect)) {
                number += 1;
            }
        });
        return number / rotateRectPoint.length;
    };
    ScreenEventNode.prototype.calculateAreaBySampleAlgorithm = function (rect) {
        var index = sampleNumber;
        var points = [];
        var widthStep = (rect.right - rect.left) / index;
        var heightStep = (rect.bottom - rect.top) / index;
        var startX = rect.left;
        var startY = rect.top;
        for (var i = 0; i < index; i++) {
            for (var j = 0; j < index; j++) {
                points.push({ x: startX + widthStep * i, y: startY + heightStep * j });
            }
        }
        return points;
    };
    // 逆时针旋转角度为正
    ScreenEventNode.prototype.rotateByPoint = function (originPoint, pivot, rotation) {
        var ratio = this.getRatio();
        var newOriginPointX = (originPoint.x - pivot.x) * Math.cos(rotation)
            - (originPoint.y - pivot.y) / ratio * Math.sin(rotation) + pivot.x;
        var newOriginPointY = (originPoint.x - pivot.x) * ratio * Math.sin(rotation)
            + (originPoint.y - pivot.y) * Math.cos(rotation) + pivot.y;
        return { x: newOriginPointX, y: newOriginPointY };
    };
    ScreenEventNode.prototype.isRectIntersecting = function (originRect, targetRect) {
        return (Math.abs(targetRect.right + targetRect.left - originRect.right - originRect.left)
            <= originRect.right - originRect.left + targetRect.right - targetRect.left)
            && (Math.abs(targetRect.bottom + targetRect.top - originRect.bottom - originRect.top)
                <= originRect.bottom - originRect.top + targetRect.bottom - targetRect.top);
    };
    ScreenEventNode.prototype.getIntersectingAreaMaxPercent = function (originRect, targetRect) {
        var IntersectingArea = this.getIntersectingArea(originRect, targetRect);
        var originRectPercent = IntersectingArea / this.getRectArea(originRect);
        var targetRectPercent = IntersectingArea / this.getRectArea(targetRect);
        return Math.max(originRectPercent, targetRectPercent);
    };
    ScreenEventNode.prototype.getIntersectingArea = function (originRect, targetRect) {
        var left = Math.max(originRect.left, targetRect.left);
        var top = Math.max(originRect.top, targetRect.top);
        var right = Math.min(originRect.right, targetRect.right);
        var bottom = Math.min(originRect.bottom, targetRect.bottom);
        return this.getRectArea({ left: left, right: right, top: top, bottom: bottom });
    };
    ScreenEventNode.prototype.getRectArea = function (rect) {
        return (rect.right - rect.left) * (rect.bottom - rect.top);
    };
    ScreenEventNode.nodeType = 'code/ScreenEvent';
    return ScreenEventNode;
}(Node));
light.NodeContext.registerNode(ScreenEventNode);

var ViewPointEventNode = /** @class */ (function (_super) {
    __extends(ViewPointEventNode, _super);
    function ViewPointEventNode() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ViewPointEventNode.prototype.Run = function () {
        openAIFeature(["View_Point" /* VIEW_POINT */], this.entityManager, this.eventManager);
        light.on('update', this.onUpdate.bind(this));
    };
    ViewPointEventNode.prototype.onUpdate = function () {
        // 获取视动点位
        var points = light.AIDataUtils.GetViewPoint(this.entityManager);
        var screenX = points.get(0);
        var screenY = points.get(1);
        var screenWidth = light.DeviceUtils.GetSurfaceWidth(this.entityManager);
        var screenHeight = light.DeviceUtils.GetSurfaceHeight(this.entityManager);
        this.viewPointX = screenX / screenWidth;
        this.viewPointY = screenY / screenHeight;
        this.Next();
    };
    ViewPointEventNode.prototype.Next = function () {
    };
    ViewPointEventNode.nodeType = 'code/ViewPointEvent';
    return ViewPointEventNode;
}(Node));
light.NodeContext.registerNode(ViewPointEventNode);

var _a;
var RegionType;
(function (RegionType) {
    RegionType["RECT"] = "\u77E9\u5F62";
    RegionType["CIRCLE"] = "\u5706\u5F62";
})(RegionType || (RegionType = {}));
function square(x) {
    return x * x;
}
var checkInRegionFunctions = (_a = {},
    _a[RegionType.RECT] = function (x, y, x1, y1, x2, y2) { return x >= x1 && y >= y1 && x <= x2 && y <= y2; },
    _a[RegionType.CIRCLE] = function (x, y, x1, y1, x2, y2) {
        var xRadius = Math.abs(x1 - x2) / 2;
        var yRadius = Math.abs(y1 - y2) / 2;
        var xCenter = (x1 + x2) / 2;
        var yCenter = (y1 + y2) / 2;
        return square(x - xCenter) / square(xRadius) + square(y - yCenter) / square(yRadius) <= 1;
    },
    _a);
var RegionNode = /** @class */ (function (_super) {
    __extends(RegionNode, _super);
    function RegionNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.X = 0;
        _this.Y = 0;
        _this.x1 = 0;
        _this.y1 = 0;
        _this.x2 = 0;
        _this.y2 = 0;
        _this.regionType = RegionType.RECT;
        return _this;
    }
    RegionNode.prototype.Run = function () {
        var checkInRegion = checkInRegionFunctions[this.regionType];
        if (checkInRegion(this.X, this.Y, this.x1, this.y1, this.x2, this.y2)) {
            this.InSide();
        }
        else {
            this.OutSide();
        }
    };
    RegionNode.prototype.InSide = function () {
    };
    RegionNode.prototype.OutSide = function () {
    };
    RegionNode.nodeType = 'code/region';
    return RegionNode;
}(Node));
light.NodeContext.registerNode(RegionNode);

var ValueNode = /** @class */ (function (_super) {
    __extends(ValueNode, _super);
    function ValueNode() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.valueType = 'number';
        _this._numberValue = 2;
        _this._stringValue = '';
        _this._colorValue = '#00000000';
        _this._boolValue = true;
        _this._fileValue = '';
        _this._vector2Value = { x: 0, y: 0 };
        _this._vector3Value = { x: 1, y: 1, z: 0 };
        _this._vector4Value = { top: 0, right: 0, bottom: 0, left: 0 };
        return _this;
    }
    Object.defineProperty(ValueNode.prototype, "numberValue", {
        get: function () {
            return this._numberValue;
        },
        set: function (v) {
            this._numberValue = v;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(ValueNode.prototype, "stringValue", {
        get: function () {
            return this._stringValue;
        },
        set: function (v) {
            this._stringValue = v;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(ValueNode.prototype, "fileValue", {
        get: function () {
            return this._fileValue;
        },
        set: function (v) {
            this._fileValue = v;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(ValueNode.prototype, "colorValue", {
        get: function () {
            return this._colorValue;
        },
        set: function (v) {
            this._colorValue = v;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(ValueNode.prototype, "boolValue", {
        get: function () {
            return this._boolValue;
        },
        set: function (v) {
            this._boolValue = v;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(ValueNode.prototype, "vector2Value", {
        get: function () {
            return this._vector2Value;
        },
        set: function (v) {
            this._vector2Value = v;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(ValueNode.prototype, "vector3Value", {
        get: function () {
            return this._vector3Value;
        },
        set: function (v) {
            this._vector3Value = v;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(ValueNode.prototype, "vector4Value", {
        get: function () {
            return this._vector4Value;
        },
        set: function (v) {
            this._vector4Value = v;
        },
        enumerable: false,
        configurable: true
    });
    ValueNode.nodeType = 'code/value';
    return ValueNode;
}(Node));
light.NodeContext.registerNode(ValueNode);

var CommonComponent = /** @class */ (function (_super) {
    __extends(CommonComponent, _super);
    function CommonComponent() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.entityId = null;
        _this.componentProps = []; // 当前对象的组件的全量属性
        _this.componentType = null;
        return _this;
    }
    CommonComponent.prototype.Run = function () {
        var _this = this;
        var _a;
        if (this.entityId) {
            var entity = this.entityManager.getEntityById(this.entityId);
            var comp_1 = entity.getComponent(light[this.componentType]);
            if (comp_1) {
                if ((_a = this.componentProps) === null || _a === void 0 ? void 0 : _a.length) {
                    this.componentProps.forEach(function (prop) {
                        if (_this[prop] !== null && _this[prop] !== undefined) {
                            _this.assignProp(comp_1, prop);
                        }
                    });
                }
            }
            this.Next();
        }
    };
    CommonComponent.prototype.Next = function () { };
    CommonComponent.prototype.assignProp = function (target, prop) {
        if (!target[prop]) {
            // todo studio上的属性与SDK不是完全一一对应的 有些属性会进行转化 比如ScreenTransform在ls上是eEuler position 但SDK是rotation和anchor/offset
            return;
        }
        var isRectProps = function (props) { return typeof props.top !== 'undefined'
            && typeof props.left !== 'undefined'
            && typeof props.right !== 'undefined'
            && typeof props.bottom !== 'undefined'; };
        var isV3Props = function (props) { return typeof props.x !== 'undefined'
            && typeof props.y !== 'undefined'
            && typeof props.z !== 'undefined'; };
        var isV2Props = function (props) { return typeof props.x !== 'undefined'
            && typeof props.y !== 'undefined'; };
        if (isV3Props(this[prop])) {
            target[prop].x = this[prop].x;
            target[prop].y = this[prop].y;
            target[prop].z = this[prop].z;
        }
        else if (isV2Props(this[prop])) {
            target[prop].x = this[prop].x;
            target[prop].y = this[prop].y;
        }
        else if (isRectProps(this[prop])) {
            target[prop].right = this[prop].right;
            target[prop].left = this[prop].left;
            target[prop].top = this[prop].top;
            target[prop].bottom = this[prop].bottom;
        }
        else {
            target[prop] = this[prop];
        }
    };
    CommonComponent.nodeType = 'code/commonComponent';
    return CommonComponent;
}(Node));
light.NodeContext.registerNode(CommonComponent);

if (typeof (globalThis) === 'undefined') {
    this['globalThis'] = this;
}
globalThis.configure = light.configure;
globalThis.update = light.update;

})(light);