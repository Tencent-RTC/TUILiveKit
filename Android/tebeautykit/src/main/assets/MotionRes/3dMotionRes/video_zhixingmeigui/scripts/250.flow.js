/*** light-js-config
//@requireAbility
***/
/* 该脚本流程面板生成 */
// 开始 Flow 生成代码
let parentId = [];
function bindLightAssetFragmentRootEntityId(entityId){
   if(typeof light.parentId === 'undefined'){
      light.parentId = [];
   }
   parentId.push(entityId);
}
light.on('start',function (entityManager, eventManager, scriptSystem) {
   let localParentId = parentId.shift();
   let getRuntimeID = function(id){
      if(typeof localParentId === 'undefined'){
         return id;
      }else {
         return light.EntityUtils.GetRuntimeEntityByID(entityManager, localParentId, id).id;
      }
   }
   var context = new light.NodeContext(entityManager, eventManager, scriptSystem);
   // 实例化
   let code_SwitchObject_2 = context.create("code/SwitchObject");
   code_SwitchObject_2.id = "2";
   let code_Start_1 = context.create("code/Start");
   code_Start_1.id = "1";
   let code_SwitchObject_4 = context.create("code/SwitchObject");
   code_SwitchObject_4.id = "4";
   let code_play_3 = context.create("code/play");
   code_play_3.id = "3";
   // 属性赋值
   code_SwitchObject_2.entityToDisplay = [];
   code_SwitchObject_2.entityToHide = [13];
   code_SwitchObject_4.entityToDisplay = [13];
   code_SwitchObject_4.entityToHide = [];
   code_play_3.entityId = getRuntimeID(9);
   code_play_3.loopType = "固定数字";
   code_play_3.playtimes = 1;
   code_play_3.keepLastFrame = false;
   // 数据连接
   // 事件连接
   context.connectEvent(code_Start_1, "Run", code_SwitchObject_2, "Run")
   context.connectEvent(code_play_3, "Next", code_SwitchObject_4, "Run")
   context.connectEvent(code_Start_1, "Run", code_play_3, "Run")
   code_Start_1.Run();
});