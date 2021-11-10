import { emojiMap, emojiUrl } from './emojiMap';
/** 传入messageBody（群系统消息SystemMessage，群提示消息GroupTip除外）
 * payload = {
 *  msgType: 'TIMTextElem',
 *  msgContent: {
 *    text: 'AAA[龇牙]AAA[龇牙]AAA[龇牙AAA]'
 *  }
 *}
 **/
export function decodeText(content) {
//   console.log(payload);
  const renderDom = [];
  let temp = content;
  let left = -1;
  let right = -1;
  while (temp !== '') {
    left = temp.indexOf('[');
    right = temp.indexOf(']');
    switch (left) {
      case 0:
        if (right === -1) {
          renderDom.push({
            name: 'text',
            content: temp,
          });
          temp = '';
        } else {
          const emoji = temp.slice(0, right + 1);
          if (emojiMap[emoji]) {
            renderDom.push({
              name: 'img',
              src: emojiUrl + emojiMap[emoji],
            });
            temp = temp.substring(right + 1);
          } else {
            renderDom.push({
              name: 'text',
              content: '[',
            });
            temp = temp.slice(1);
          }
        }
        break;
      case -1:
        renderDom.push({
          name: 'text',
          content: temp,
        });
        temp = '';
        break;
      default:
        renderDom.push({
          name: 'text',
          content: temp.slice(0, left),
        });
        temp = temp.substring(left);
        break;
    }
  }
  return renderDom;
}
