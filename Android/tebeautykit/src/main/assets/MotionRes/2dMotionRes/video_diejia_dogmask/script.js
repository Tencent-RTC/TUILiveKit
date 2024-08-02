
// 加载 AEJSBridge.js
light.execute("light://js/AEJSBridge.js");

(function () {
    //定义素材对象
    var global = global || (function () {
        return this;
    }());
    var template = {};
    global.template = template;
    var resourcePool = {
        "sweat_1": new Resource("sweat_1.png"),
        "peach_1": new Resource("peach_1.png"),
        "icecream_1": new Resource("icecream_1.png")
    };
    global.resourcePool = resourcePool;

    template.genderType = 0;

    template.actionList = [];
    template.oneLoopTime = 999000;
    template.lastTime = 0;
    template.needAction = function (action) {
        var needAction = template.actionList.indexOf(action) === -1;
        if (needAction) {
            template.actionList.push(action);
        }
        return needAction;
    }
    template.resetAction = function () {
        template.actionList = [];
    }

    template.currentTouchState = false;
    template.previewTouchState = false;

    template.onTemplateInit = function (entityManager, eventManager) {
        //指定JS需要从AEDataCenter里获取哪些值
        template.script_component_1 = light.getComponent(entityManager.getEntity(19), "Script");

        // 场景一 桃子
        template.peach_1_Image = light.getComponent(entityManager.getEntity(104), "Image"); 
        template.peach_2_Image = light.getComponent(entityManager.getEntity(147), "Image");    
        template.peach_3_Image = light.getComponent(entityManager.getEntity(196), "Image"); 
        // 场景二 冰淇淋
        template.icecream_1_Image = light.getComponent(entityManager.getEntity(33), "Image"); 
        template.icecream_2_Image = light.getComponent(entityManager.getEntity(59), "Image"); 
        template.icecream_3_Image = light.getComponent(entityManager.getEntity(91), "Image");
        // 场景三 汗
        template.sweat_1_Image = light.getComponent(entityManager.getEntity(22), "Image"); 
        template.init()
    }

    template.init = function(){
        template.icecream_1_Image.enabled = false;
        template.icecream_2_Image.enabled = false;
        template.icecream_3_Image.enabled = false;
        template.sweat_1_Image.enabled = false;
    }

    var currentMv = 1;

    template.onFrameUpdate = function (currentTime, entityManager, eventManager) {
        // light._disableDefaultBeauty([BASIC_STRETCH, BASIC_LIQUIFY, BASIC_SMOOTH]);
        if (!template.previewTouchState && template.currentTouchState) {
            if (currentMv == 1) {
                // 冰淇淋出现
                template.icecream_1_Image.enabled = true;
                template.icecream_2_Image.enabled = true;
                template.icecream_3_Image.enabled = true;
                // 桃子消失
                template.peach_1_Image.enabled = false;
                template.peach_2_Image.enabled = false;
                template.peach_3_Image.enabled = false;
                currentMv = 2;
            } else if (currentMv == 2) {
                // 汗出现
                template.sweat_1_Image.enabled = true;
                // 冰淇淋消失
                template.icecream_1_Image.enabled = false;
                template.icecream_2_Image.enabled = false;
                template.icecream_3_Image.enabled = false;
                currentMv = 3;
            } else if (currentMv == 3) {
                // 汗消失
                template.sweat_1_Image.enabled = false;
                // 桃子出现
                template.peach_1_Image.enabled = true;
                template.peach_2_Image.enabled = true;
                template.peach_3_Image.enabled = true;
                currentMv = 1;
            }
        }

        template.previewTouchState = template.currentTouchState;

        template.currentTouchState = false;
    }

    template.onInputEvent = function (params) {
        var hasTouch = params["event.touchPoint"];
        if (hasTouch) {
            console.log(params["event.touchPoint"]);
            template.currentTouchState = true;
        }
    }

}
());
