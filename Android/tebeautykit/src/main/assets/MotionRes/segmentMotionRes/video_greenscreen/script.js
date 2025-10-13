/*** light-js-config
***/
// 加载 AEJSBridge.js
light.execute("light://js/AEJSBridge.js");

// 素材逻辑函数体
(function () {
    // 定义global对象
    var global = global || (function () {return this;}());
    // 定义素材对象
    var template = global.template || (function () {return {};}());
    // 并挂在global对象下
    global.template = template;
    // 定义需要用到的resource
    var resourcePool = {
    }
     //初始化一个jsonObject,
    template.uniformJson = {
        "uniformMap": {
            "chromakey" : {
                "key_color" : [0.0, 1.0, 0.0, 0.0],
                "tex_rect" : [0.0, 0.0, 720.0, 1280.0]
            }
        }
    }
    // 也挂在global对象下
    global.resourcePool = resourcePool;


//    // 订阅InputEvent事件
    template.onInputEvent = function(params) {
        var jsonData = params["event.script.lightsdk.GreenScreenSetKeyColor"];
        if (jsonData) {
            // 目前只会改这三个
            template.uniformJson.uniformMap.chromakey.key_color[0] = jsonData["key_color"][0];
            template.uniformJson.uniformMap.chromakey.key_color[1] = jsonData["key_color"][1];
            template.uniformJson.uniformMap.chromakey.key_color[2] = jsonData["key_color"][2];
        }
    }

    // 素材初始化, 对应c++的configure
    template.onTemplateInit = function (entityManager, eventManager) {
        template.customGraph = light.getComponent(
          entityManager.getEntityByName("CustomGraph"),
          "CustomGraph"
        );
    }
    // 对应c++的update
    template.onFrameUpdate = function (currentTime, entityManager, eventManager) {
        //将得到的uniformJson传递到自定义滤镜链中
        var surfaceWidth = light.DeviceUtils.GetSurfaceWidth(entityManager);
        var surfaceHeight = light.DeviceUtils.GetSurfaceHeight(entityManager);
        
        template.uniformJson.uniformMap.chromakey.tex_rect[2] = surfaceWidth;
        template.uniformJson.uniformMap.chromakey.tex_rect[3] = surfaceHeight;

        template.customGraph.uniformJson = JSON.stringify(template.uniformJson);
    }
}());
