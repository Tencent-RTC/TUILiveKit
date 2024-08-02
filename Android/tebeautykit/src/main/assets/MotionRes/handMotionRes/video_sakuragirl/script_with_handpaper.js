
(function () {
    //定义素材对象
    var global = global || (function () {
        return this;
    }());
    var template = {};
    global.template = template;

    var resourcePool = {
        "zc_lvse_Image": new Resource("zc_lvse.pag"),
        "fg2": new Resource("fg2.pag")
    };
    global.resourcePool = resourcePool;

    template.onTemplateInit = function (entityManager, eventManager) {
        template.script_component_1 = light.getComponent(entityManager.getEntity(37), "Script");
        let aiRequire = new light.VectorString();
        aiRequire.add("Gender");
        aiRequire.add("Hand_Gesture");
        template.script_component_1.aiRequire = aiRequire;
        eventManager.emit(new light.ScriptOpenAIEvent(entityManager, aiRequire));

        template.multiply_MakeupFace = light.getComponent(entityManager.getEntity(27), "MakeupFace");
        template.multiply_MakeupFace.enabled = false;
        template.lvse_MakeupFace = light.getComponent(entityManager.getEntity(28), "MakeupFace");
        template.lvse_MakeupFace.enabled = false;
        template.iris_MakeupIris = light.getComponent(entityManager.getEntity(29), "MakeupIris");
        template.iris_MakeupIris.enabled = false;
        template.fajia2_Image = light.getComponent(entityManager.getEntity(30), "Image");
        template.fajia2_Image.enabled = false;
        template.toushi_left_Image = light.getComponent(entityManager.getEntity(31), "Image");
        template.toushi_left_Image.enabled = true;
        template.toushi_right_Image = light.getComponent(entityManager.getEntity(32), "Image");
        template.toushi_right_Image.enabled = true;
        template.hand_Image = light.getComponent(entityManager.getEntity(33), "Image");
        template.hand_Image.enabled = true;
        template.fg_Image = light.getComponent(entityManager.getEntity(34), "Image");
        template.fg_Image.enabled = true;
        // template.fg2_Image = light.getComponent(entityManager.getEntity(35), "Image");
        // template.fg2_Image.enabled = false;
        template.fg2_xj_Image = light.getComponent(entityManager.getEntity(36), "Image");
        template.fg2_xj_Image.enabled = true;
        template.zc_lvse_Image = light.getComponent(entityManager.getEntity(38), "Image");
        template.zc_lvse_Image.enabled = true;
        template.ranfa_Image = light.getComponent(entityManager.getEntity(63), "HairColor");
        template.ranfa_Image.enabled = true;
        template.triggerFlag = false;
        template.haspaper = false;
        template.triggerTime = 0;
    }

    template.onFemale = function () {
        template.multiply_MakeupFace.enabled = true;
        template.lvse_MakeupFace.enabled = true;
        template.iris_MakeupIris.enabled = true;
    }

    template.onMale = function () {
        template.multiply_MakeupFace.enabled = false;
        template.lvse_MakeupFace.enabled = false;
        template.iris_MakeupIris.enabled = false;
    }

    template.onPaper = function () {
        template.haspaper = true;
        if (template.triggerFlag == true) {
            return;
        } else {
            template.triggerFlag = true;
            template.hand_Image.enabled = true;
            // template.fg2_Image.enabled = true;
            template.fg2_xj_Image.enabled = true;
            template.fajia2_Image.enabled = true;
            template.toushi_left_Image.enabled = false;
            template.toushi_right_Image.enabled = false;
            template.ranfa_Image.enabled = false;
        }
    }

    template.onFrameUpdate = function (currentTime, entityManager, eventManager) {

        if (template.triggerTime == 0) {
            template.hand_Image.enabled = false;
            template.fg2_xj_Image.enabled = false;
            // template.fajia2_Image.enabled = false;
            template.zc_lvse_Image.enabled = false;
        }

        if (!template.triggerTime && template.triggerFlag) {
            template.triggerTime = currentTime;
        }
        if (template.triggerTime == currentTime && currentTime > 0) {
            template.zc_lvse_Image.enabled = true;
            template.zc_lvse_Image.src = resourcePool.zc_lvse_Image.key;
            eventManager.emit(new light.ResetPagEvent(resourcePool.zc_lvse_Image.key, currentTime * 1000, entityManager));

            template.fg_Image.src = resourcePool.fg2.key;
            eventManager.emit(new light.ResetPagEvent(resourcePool.fg2.key, currentTime * 1000, entityManager));
        }

        if (template.zc_lvse_Image.enabled && template.triggerTime && (currentTime - template.triggerTime) >= 2250) {
            template.zc_lvse_Image.enabled = false;
        }
        // 是paper才让跟手贴纸有效
        template.hand_Image.enabled  = template.haspaper;
        template.haspaper = false;
    }
}
());
