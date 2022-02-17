/*
 * @Description: 页面布局计算, 页面布局包括：PC 端，移动端横屏，移动端竖屏
 * @Date: 2022-02-17 16:16:40
 * @LastEditTime: 2022-02-18 22:16:00
 */
import { UPDATE_LAYOUT } from '@/constants/mutation-types';
import { LAYOUT } from '@/constants/room';
import { isVerticalScreen, setFullScreen, exitFullScreen, isAndroid, isIosSafari } from '@/utils/utils';
import { mapState } from 'vuex';

export default {
  data() {
    return {
      isVerticalScreen: null,
    };
  },
  computed: {
    ...mapState({
      layout: 'layout',
    }),
    // 是否展示移动端竖屏布局
    isMobileVerticalLayout() {
      return this.$isMobile && this.layout === LAYOUT.VERTICAL;
    },
    // 是否展示移动端横屏布局
    isMobileHorizontalLayout() {
      return this.$isMobile && this.layout === LAYOUT.HORIZONTAL;
    },
    layoutClassName() {
      if (this.isMobileVerticalLayout) {
        return 'mobile-vertical-layout';
      }
      if (this.isMobileHorizontalLayout) {
        return 'mobile-horizontal-layout';
      }
      return 'app-layout';
    },
  },
  watch: {
    // 点击布局按钮触发布局改变
    layout(val) {
      this.handleRotateClass();
      if (isAndroid) {
        if (val === LAYOUT.VERTICAL) {
          exitFullScreen(document.querySelector('body'));
        } else if (val === LAYOUT.HORIZONTAL) {
          setFullScreen(document.querySelector('body'));
        }
      }
    },
    // 屏幕旋转触发布局改变
    isVerticalScreen() {
      this.handleRotateClass();
    },
    isMobileHorizontalLayout(val) {
      // safari 浏览器建议添加页面标签到主屏幕，从主屏幕打开页面享受沉浸式体验
      if (val && isIosSafari) {
        this.$message.info({
          message: this.$t('layout.Suggest adding to home screen'),
          showClose: true,
          duration: 10000,
        });
      }
    },
  },
  methods: {
    handleAppLayout() {
      this.isVerticalScreen = isVerticalScreen();
      this.$store.commit(UPDATE_LAYOUT, this.isVerticalScreen ? LAYOUT.VERTICAL : LAYOUT.HORIZONTAL);
    },
    handleRotateClass() {
      const screenWidth = document.documentElement.clientWidth;
      const screenHeight =  document.documentElement.clientHeight;
      if (this.isVerticalScreen && this.layout === LAYOUT.HORIZONTAL) {
        this.$refs.app.style.width = `${screenHeight}px`;
        this.$refs.app.style.height = `${screenWidth}px`;
        this.$refs.app.style.transform = 'rotate(90deg)';
        this.$refs.app.style.transformOrigin = '0 0';
        this.$refs.app.style.left = `${screenWidth}px`;
        return;
      }
      if (!this.isVerticalScreen && this.layout === LAYOUT.VERTICAL) {
        this.$refs.app.style.width = `${screenHeight}px`;
        this.$refs.app.style.height = `${screenWidth}px`;
        this.$refs.app.style.transform = 'rotate(-90deg)';
        this.$refs.app.style.transformOrigin = '0 0';
        this.$refs.app.style.top = `${screenHeight}px`;
        return;
      }
      this.$refs.app.style = {};
    },
  },
  created() {
    // 处理页面布局
    this.handleAppLayout();
    window.addEventListener('onorientationchange' in window ? 'orientationchange' : 'resize', this.handleAppLayout);
  },
  beforeDestroy() {
    window.removeEventListener('onorientationchange' in window ? 'orientationchange' : 'resize', this.handleAppLayout);
  },
};
