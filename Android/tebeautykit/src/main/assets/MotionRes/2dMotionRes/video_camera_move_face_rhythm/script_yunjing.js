/*** light-js-config
***/


let progress = 1.0;
let shadowProgress = 1.0;
let shadowAlpha = 0.0;
let shadowY = 0.0;
let STEP = 0.02;//MAX需要是STEP的整数倍
let MAX = 1.4;//MAX需要是STEP的整数倍
let SECOND_MAX = 1.2
let period = 0;//放大1-还原-放大2-还原
light.update = function (deltaTime, entityManager, eventManager) {
	let canvasYunjing = entityManager.getEntityByName("canvas_yunjing");
	let transform_comp = canvasYunjing.getComponent("ScreenTransform");

	let shadow = entityManager.getEntityByName("canvas_shadow");
	let shadowBlendMode = shadow.getComponent("BlendMode");
	let shadowTransform = shadow.getComponent("ScreenTransform");

	if (period == 0){
		progress += STEP;
		if (MAX - progress < 0.00001){
			period = 1;
		}
		shadowProgress = 1.0;
		shadowAlpha = 0.0;
		shadowY = 0.0;
	} else if (period == 1){
		progress -= STEP;
		if (progress - 1.0 < 0.00001){
			period = 2;
		}
		shadowProgress = 1.0;
		shadowAlpha = 0.0;
		shadowY = 0.0;
	} else if (period == 2){
		progress += STEP;
		if (SECOND_MAX - progress < 0.00001){
			period = 3;
		}
		shadowProgress += 0.1;
		shadowAlpha = 0.25;
		shadowY -= 0.04;
	} else if (period == 3){
		progress -= STEP;
		if (progress - 1.0 < 0.00001){
			period = 0;
		}
		shadowProgress = 1.0;
		shadowAlpha = 0.0;
		shadowY = 0.0;
	}

	var comp_scale = transform_comp.scale;
	comp_scale.x = progress;
	comp_scale.y = progress;
	comp_scale.z = 1.0;
	transform_comp.SetScale(comp_scale);

	shadowBlendMode.alpha = shadowAlpha;

	let comp_scale2 = shadowTransform.scale;
	comp_scale2.x = shadowProgress;
	comp_scale2.y = shadowProgress;
	comp_scale2.z = 1.0;
	shadowTransform.SetScale(comp_scale2);

	var shadow_pos = shadowTransform.pivot;
	shadow_pos.y = shadowY;
	shadowTransform.pivot(shadow_pos);

}


