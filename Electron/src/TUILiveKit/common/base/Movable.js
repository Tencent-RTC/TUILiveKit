class Movable {
  constructor(movable, container, options = {
    calcPositionOnly: false,
    canExceedContainer: false,
  }) {
    if (!movable) {
      console.error("The movable argument must be an HTML element.");
      return;
    }
    this.movable = movable;
    this.container = container || document.body;
    this.options = {
      calcPositionOnly: !!options.calcPositionOnly || false,
      canExceedContainer: !!options.canExceedContainer || false
    };
    // Map<string, Array<Function>>
    this.callbacksMap = new Map();

    this.setStyle();

    this.originLeft = null;
    this.originTop = null;
    this.moveStartOfLeft = null;
    this.moveStartOfTop = null;

    this.onmousedown = this.onmousedown.bind(this);
    this.onmousemove = this.onmousemove.bind(this);
    this.onmouseup = this.onmouseup.bind(this);
    this.onmousemove5px = this.onmousemove5px.bind(this);
    this.onmouseup5px = this.onmouseup5px.bind(this);

    this.movable.addEventListener("mousedown", this.onmousedown, false);
  }

  setStyle() {
    this.movable.style.position = "absolute";
  }

  onmousedown(event) {
    console.log("on Movable mouse down");
    if (event.button !== 0)  {
      return;
    }
    event.preventDefault(); // Avoid select text content.

    var movableStyle = document.defaultView.getComputedStyle(this.movable);
    this.originLeft = window.parseInt(movableStyle.left);
    this.originTop = window.parseInt(movableStyle.top);

    this.moveStartOfLeft = window.parseInt(event.screenX);
    this.moveStartOfTop = window.parseInt(event.screenY);

    document.addEventListener('mousemove', this.onmousemove5px, false);
    document.addEventListener('mouseup', this.onmouseup5px, false);
  }

  onmousemove5px(event) {
    // eslint-disable-next-line
    var leftMovedDistance = window.parseInt(event.screenX) - this.moveStartOfLeft;
    var topMovedDistance = window.parseInt(event.screenY) - this.moveStartOfTop;
    if (Math.abs(leftMovedDistance) >= 5 || Math.abs(topMovedDistance) >= 5) {
      // 至少移动5像素，才开始真正的移动，避免鼠标点一下就立即移动
      console.log("on Movable mouse move more than 5px");
      document.removeEventListener("mousemove", this.onmousemove5px, false);
      document.removeEventListener("mouseup", this.onmouseup5px, false);
      document.addEventListener("mousemove", this.onmousemove, false);
      document.addEventListener("mouseup", this.onmouseup, false);
    }
  }
  onmouseup5px(event) {
    console.log("on Movable mouse up: onmouseup5px");
    document.removeEventListener("mousemove", this.onmousemove5px, false);
    document.removeEventListener("mouseup", this.onmouseup5px, false);
  }

  onmousemove(event) {
    // eslint-disable-next-line
    var leftMovedDistance = window.parseInt(event.screenX) - this.moveStartOfLeft;
    var topMovedDistance = window.parseInt(event.screenY) - this.moveStartOfTop;

    var left = this.originLeft + leftMovedDistance;
    var top = this.originTop + topMovedDistance;
    var movableWidth = this.movable.offsetWidth;
    var movableHeight = this.movable.offsetHeight;
    var containerWidth = this.container.offsetWidth;
    var containerHeight = this.container.offsetHeight;

    if (!this.options.canExceedContainer) {
      if (left < 0) {
        left = 0;
      } else if (left > containerWidth - movableWidth) {
        left = containerWidth - movableWidth;
      }
  
      if (top < 0) {
        top = 0;
      } else if (top > containerHeight - movableHeight) {
        top = containerHeight - movableHeight;
      }
    }

    if (!this.options.calcPositionOnly) {
      this.movable.style.left = left + "px";
      this.movable.style.top = top + "px";
    }

    this.emit("move", left, top);
  }

  onmouseup(event) {
    console.log("on Movable mouse up: onmouseup");
    document.removeEventListener("mousemove", this.onmousemove, false);
    document.removeEventListener("mouseup", this.onmouseup, false);

    this.originLeft = null;
    this.originTop = null;
    this.moveStartOfLeft = null;
    this.moveStartOfTop = null;
  }

  on(eventName, callback) {
    const callbacks = this.callbacksMap.get(eventName);
    if (callbacks) {
      callbacks.push(callback);
    } else {
      this.callbacksMap.set(eventName, [callback]);
    }
  }

  off(eventName, callback) {
    let callbacks = this.callbacksMap.get(eventName);
    if (callbacks) {
      callbacks = callbacks.filter((item) => item != callback);
      this.callbacksMap.set(eventName, callbacks);
    }
  }

  emit(eventName, ...arg) {
    let callbacks = this.callbacksMap.get(eventName);
    if (callbacks) {
      callbacks.forEach((callback) => {
        try {
          callback.apply(null, arg); // arg must be array
        } catch (error) {
          console.error("[Movable]emit error:", callback, error);
        }
      });
    }
  }

  destroy() {
    this.movable.removeEventListener("mousedown", this.onmousedown, false);
    this.callbacksMap.clear();
    this.movable = null;
    this.container = null;
  }
}

export default Movable;
