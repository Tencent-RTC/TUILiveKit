light.execute("light://js/AEJSBridge.js");
(function () {
    // 获取global对象
    var global = global || (function () {
        return this;
    }());
    var template = {};
    global.template = template;
    
    template.onTemplateInit = function () {
        light._disableDefaultBeauty([BASIC_STRETCH, BASIC_LIQUIFY]);
    }
}())
