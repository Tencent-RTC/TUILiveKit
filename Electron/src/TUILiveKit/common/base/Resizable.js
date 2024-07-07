const ResizeAnchorMode = {
  Both: 0,
  Corner: 1,
  Edge: 2,
}

class Resizable {
  constructor(
    resizeTarget,
    container,
    options = { 
      keepRatio: false, 
      stopPropagation: false, 
      anchorMode: ResizeAnchorMode.Both,
      canExceedContainer: false
    }
  ) {
    if (!resizeTarget) {
      console.error("The resizable argument must be an HTML element.");
      return;
    }

    this.resizeTarget = resizeTarget;
    this.container = container || document.body;
    this.options = {
      keepRatio: !!options.keepRatio || false,
      stopPropagation: !!options.stopPropagation || false,
      anchorMode: options.anchorMode || ResizeAnchorMode.Both,
      canExceedContainer: !!options.canExceedContainer || false
    };
    // Map<string, Array<Function>>
    this.callbacksMap = new Map();

    this.topLeftAnchor = null;
    this.topAnchor = null;
    this.topRightAnchor = null;
    this.leftAnchor = null;
    this.rightAnchor = null;
    this.bottomLeftAnchor = null;
    this.bottomAnchor = null;
    this.bottomRightAnchor = null;

    this.mousedown = this.mousedown.bind(this);
    this.mousemove = this.mousemove.bind(this);
    this.mouseup = this.mouseup.bind(this);

    this.currentAnchor = null;
    this.resizeStartLeft = null;
    this.resizeStartTop = null;
    this.originLeft = null;
    this.originTop = null;
    this.originWidth = null;
    this.originHeight = null;

    this.createResizeAnchor();
    this.resizeTarget.classList.add("resizable");

    this.initResizeEvent();
  }

  createResizeAnchor() {
    var topLeftAnchor = document.createElement("div");
    topLeftAnchor.className = "resize-anchor top-left-anchor";
    this.topLeftAnchor = topLeftAnchor;

    var topAnchor = document.createElement("div");
    topAnchor.className = "resize-anchor top-anchor";
    this.topAnchor = topAnchor;

    var topRightAnchor = document.createElement("div");
    topRightAnchor.className = "resize-anchor top-right-anchor";
    this.topRightAnchor = topRightAnchor;

    var leftAnchor = document.createElement("div");
    leftAnchor.className = "resize-anchor left-anchor";
    this.leftAnchor = leftAnchor;

    var rightAnchor = document.createElement("div");
    rightAnchor.className = "resize-anchor right-anchor";
    this.rightAnchor = rightAnchor;

    var bottomLeftAnchor = document.createElement("div");
    bottomLeftAnchor.className = "resize-anchor bottom-left-anchor";
    this.bottomLeftAnchor = bottomLeftAnchor;

    var bottomAnchor = document.createElement("div");
    bottomAnchor.className = "resize-anchor bottom-anchor";
    this.bottomAnchor = bottomAnchor;

    var bottomRightAnchor = document.createElement("div");
    bottomRightAnchor.className = "resize-anchor bottom-right-anchor";
    this.bottomRightAnchor = bottomRightAnchor;

    if (this.options.anchorMode === ResizeAnchorMode.Both || this.options.anchorMode.Edge) {
      this.resizeTarget.appendChild(topAnchor);
      this.resizeTarget.appendChild(leftAnchor);
      this.resizeTarget.appendChild(rightAnchor);
      this.resizeTarget.appendChild(bottomAnchor);
    }
    if (this.options.anchorMode === ResizeAnchorMode.Both || this.options.anchorMode.Corner) {
      this.resizeTarget.appendChild(topLeftAnchor);
      this.resizeTarget.appendChild(topRightAnchor);
      this.resizeTarget.appendChild(bottomLeftAnchor);
      this.resizeTarget.appendChild(bottomRightAnchor);
    }
  }

  initResizeEvent() {
    this.topLeftAnchor.addEventListener("mousedown", this.mousedown, false);
    this.topAnchor.addEventListener("mousedown", this.mousedown, false);
    this.topRightAnchor.addEventListener("mousedown", this.mousedown, false);
    this.leftAnchor.addEventListener("mousedown", this.mousedown, false);
    this.rightAnchor.addEventListener("mousedown", this.mousedown, false);
    this.bottomLeftAnchor.addEventListener("mousedown", this.mousedown, false);
    this.bottomAnchor.addEventListener("mousedown", this.mousedown, false);
    this.bottomRightAnchor.addEventListener("mousedown", this.mousedown, false);
  }

  mousedown(event) {
    console.debug("on Resizable mouse down");
    if (event.button !== 0)  {
      return;
    }
    event.preventDefault(); // Avoid select text content.
    if (this.options.stopPropagation) {
      event.stopPropagation();
    }

    this.currentAnchor = event.target;

    this.resizeStartLeft = event.screenX;
    this.resizeStartTop = event.screenY;

    var resizeTargetStyle = document.defaultView.getComputedStyle(
      this.resizeTarget
    );
    this.originTop = window.parseInt(resizeTargetStyle.top);
    this.originLeft = window.parseInt(resizeTargetStyle.left);
    this.originWidth = this.resizeTarget.offsetWidth;
    this.originHeight = this.resizeTarget.offsetHeight;
    console.debug("resize origin:", this.originTop, this.originLeft, this.originWidth, this.originHeight);

    document.addEventListener("mousemove", this.mousemove, false);
    document.addEventListener("mouseup", this.mouseup, false);
  }

  mousemove(event) {
    console.log("resizing");
    var anchorType = this.currentAnchor.classList[1];
    let left = this.originLeft,
      top = this.originTop,
      width = this.originWidth,
      height = this.originHeight;
    let result;
    switch (anchorType) {
    case "top-left-anchor":
      result = this._resizeTop(event);
      top = result.top;
      height = result.height;

      result = this._resizeLeft(event);
      left = result.left;
      width = result.width;

      if (this.options.keepRatio) {
        if (width / this.originWidth < height / this.originHeight) {
          // 宽度缩放比例小于高度缩放比例，保持当前宽度，调整高度
          height = (width * this.originHeight) / this.originWidth;
          // 需要调整 y 坐标
          top = this.originTop + this.originHeight - height;
        } else {
          // 高度缩放比例小于宽度缩放比例，保持当前高度，调整宽度
          width = (height * this.originWidth) / this.originHeight;
          // 需要调整 x 坐标
          left = this.originLeft + this.originWidth - width;
        }
      }
      break;
    case "top-anchor":
      result = this._resizeTop(event);
      top = result.top;
      height = result.height;

      if (this.options.keepRatio) {
        width = this.originWidth * height / this.originHeight;
        if (width < 20) {
          width = 20;
          height = this.originHeight * width / this.originWidth;
          top = this.originTop + this.originHeight - height;
        } else if(!this.options.canExceedContainer && width > this.container.offsetWidth - this.originLeft) {
          width = this.container.offsetWidth - this.originLeft;
          height = this.originHeight * width / this.originWidth;
          top = this.originTop + this.originHeight - height;
        }
      }
      break;
    case "top-right-anchor":
      result = this._resizeTop(event);
      top = result.top;
      height = result.height;

      width = this._resizeRight(event);

      if (this.options.keepRatio) {
        if (width / this.originWidth < height / this.originHeight) {
          height = (width * this.originHeight) / this.originWidth;
          top = this.originTop + this.originHeight - height;
        } else {
          width = (height * this.originWidth) / this.originHeight;
        }
      }
      break;
    case "left-anchor":
      result = this._resizeLeft(event);
      left = result.left;
      width = result.width;

      if (this.options.keepRatio) {
        height = this.originHeight * width / this.originWidth;
        if (height < 20) {
          height = 20;
          width = this.originWidth * height / this.originHeight;
          left = this.originLeft + this.originWidth - width;
        } else if (!this.options.canExceedContainer && height > this.container.offsetHeight - this.originTop) {
          height = this.container.offsetHeight - this.originTop;
          width = this.originWidth * height / this.originHeight;
          left = this.originLeft + this.originWidth - width;
        }
      }
      break;
    case "right-anchor":
      width = this._resizeRight(event);
      if (this.options.keepRatio) {
        height = (width * this.originHeight) / this.originWidth;
        if (height < 20) {
          height = 20;
          width = height * this.originWidth / this.originHeight;
        } else if (!this.options.canExceedContainer && height > this.container.offsetHeight - this.originTop) {
          height = this.container.offsetHeight - this.originTop;
          width = height * this.originWidth / this.originHeight;
        }
      }
      break;
    case "bottom-left-anchor":
      height = this._resizeBottom(event);

      result = this._resizeLeft(event);
      left = result.left;
      width = result.width;

      if (this.options.keepRatio) {
        if (width / this.originWidth < height / this.originHeight) {
          height = (width * this.originHeight) / this.originWidth;
        } else {
          width = (height * this.originWidth) / this.originHeight;
          left = this.originLeft + this.originWidth - width;
        }
      }
      break;
    case "bottom-anchor":
      height = this._resizeBottom(event);
      if (this.options.keepRatio) {
        width = (height * this.originWidth) / this.originHeight;
        if (width < 20) {
          width = 20;
          height = width * this.originHeight / this.originWidth;
        } else if (!this.options.canExceedContainer && width > this.container.offsetWidth - this.originLeft) {
          width = this.container.offsetWidth - this.originLeft;
          height = width * this.originHeight / this.originWidth;
        }
      }
      break;
    case "bottom-right-anchor":
      height = this._resizeBottom(event);
      width = this._resizeRight(event);

      if (this.options.keepRatio) {
        if (width / this.originWidth < height / this.originHeight) {
          height = (width * this.originHeight) / this.originWidth;
        } else {
          width = (height * this.originWidth) / this.originHeight;
        }
      }
      break;
    }

    this.resizeTarget.style.left = left + "px";
    this.resizeTarget.style.top = top + "px";
    this.resizeTarget.style.height = height + "px";
    this.resizeTarget.style.width = width + "px";

    this.emit("resize", left, top, width, height);
  }

  _resizeLeft(event) {
    var leftResizedDistance =
      window.parseInt(event.screenX) - this.resizeStartLeft;
    var left = this.originLeft + leftResizedDistance;
    var width = this.originWidth - leftResizedDistance;

    if (!this.options.canExceedContainer && left < 0) {
      left = 0;
      width = this.originWidth + this.originLeft;
    } else if (left > this.originLeft + this.originWidth - 20) {
      left = this.originLeft + this.originWidth - 20;
      width = 20; // 最小宽度
    }

    return {
      left,
      width,
    };
  }

  _resizeTop(event) {
    var topResizedDistance =
      window.parseInt(event.screenY) - this.resizeStartTop;
    var top = this.originTop + topResizedDistance;
    var height = this.originHeight - topResizedDistance;

    if (!this.options.canExceedContainer && top < 0) {
      top = 0;
      height = this.originHeight + this.originTop;
    } else if (top > this.originTop + this.originHeight - 20) {
      top = this.originTop + this.originHeight - 20;
      height = 20; // 最小高度
    }

    return {
      top,
      height,
    };
  }

  _resizeRight(event) {
    var leftResizedDistance =
      window.parseInt(event.screenX) - this.resizeStartLeft;
    var width = this.originWidth + leftResizedDistance;

    if (width < 20) {
      width = 20; // 最小宽度
    } else if (!this.options.canExceedContainer && width > this.container.offsetWidth - this.originLeft) {
      width = this.container.offsetWidth - this.originLeft;
    }

    return width;
  }

  _resizeBottom(event) {
    var topResizedDistance =
      window.parseInt(event.screenY) - this.resizeStartTop;
    var height = this.originHeight + topResizedDistance;

    if (height < 20) {
      height = 20; // 最小高度
    } else if (!this.options.canExceedContainer && height > this.container.offsetHeight - this.originTop) {
      height = this.container.offsetHeight - this.originTop;
    }

    return height;
  }

  mouseup() {
    document.removeEventListener("mousemove", this.mousemove, false);
    document.removeEventListener("mouseup", this.mouseup, false);

    this.currentAnchor = null;
    this.resizeStartLeft = null;
    this.resizeStartTop = null;
    this.originLeft = null;
    this.originTop = null;
    this.originWidth = null;
    this.originHeight = null;
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
      const callbacksClone = callbacks.slice();
      callbacksClone.forEach((callback) => {
        try {
          callback.apply(null, arg); // arg must be array
        } catch (error) {
          console.error("[Resizable]emit error:", callback, error);
        }
      });
    }
  }

  destroy() {
    this.topLeftAnchor.removeEventListener("mousedown", this.mousedown, false);
    this.topAnchor.removeEventListener("mousedown", this.mousedown, false);
    this.topRightAnchor.removeEventListener("mousedown", this.mousedown, false);
    this.leftAnchor.removeEventListener("mousedown", this.mousedown, false);
    this.rightAnchor.removeEventListener("mousedown", this.mousedown, false);
    this.bottomLeftAnchor.removeEventListener(
      "mousedown",
      this.mousedown,
      false
    );
    this.bottomAnchor.removeEventListener("mousedown", this.mousedown, false);
    this.bottomRightAnchor.removeEventListener(
      "mousedown",
      this.mousedown,
      false
    );

    this.callbacksMap.clear();

    if (this.options.anchorMode === ResizeAnchorMode.Both || this.options.anchorMode.Edge) {
      this.resizeTarget.removeChild(this.topAnchor);
      this.resizeTarget.removeChild(this.leftAnchor);
      this.resizeTarget.removeChild(this.rightAnchor);
      this.resizeTarget.removeChild(this.bottomAnchor);
    }
    if (this.options.anchorMode === ResizeAnchorMode.Both || this.options.anchorMode.Corner) {
      this.resizeTarget.removeChild(this.topLeftAnchor);
      this.resizeTarget.removeChild(this.topRightAnchor);
      this.resizeTarget.removeChild(this.bottomLeftAnchor);
      this.resizeTarget.removeChild(this.bottomRightAnchor);
    }
    
    this.topLeftAnchor = null;
    this.topAnchor = null;
    this.topRightAnchor = null;
    this.leftAnchor = null;
    this.rightAnchor = null;
    this.bottomLeftAnchor = null;
    this.bottomAnchor = null;
    this.bottomRightAnchor = null;

    this.resizeTarget = null;
    this.container = null;

    this.currentAnchor = null;
    this.resizeStartLeft = null;
    this.resizeStartTop = null;
    this.originLeft = null;
    this.originTop = null;
    this.originWidth = null;
    this.originHeight = null;
  }
}

export default Resizable;
